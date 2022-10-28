# This script contains the funtions which perform the processing on the graphics
# in the dashboard. Together, they keep the dashboard style consistent and make
# it easy to create new graphics for the dashboard.

leaflet_custom <- function(data = NULL, width = NULL, height = NULL, padding = 0, elementId = NULL) {
  htmlwidgets::createWidget(
    'leaflet',
    structure(
      list(),
      leafletData = data
    ),
    width = width, height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = '100%',
      defaultHeight = 400,
      padding = padding,
      browser.fill = TRUE
    ),
    elementId = elementId
  )
}


addTAZPolygons <- function(map) {
  # Adds TAZ.polys polygons to a leaflet map, working similarly to addPolygons()
  map <- map %>% 
    addPolygons(weight = 1,
                fillOpacity = 0.2,
                opacity = 0.5,
                color = ~Color,
                fillColor = ~Color,
                data = TAZ.polys)
  return(map)
}


getLeafletScaleFactor <- function(n, n.max = 5000) {
  
  raw.factor <- n/n.max
  rounded.factors <- c(1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
  diffs <- raw.factor - rounded.factors
  scale.factor <- rounded.factors[which(diffs < 0)[1]]
  return(scale.factor)
  
}


sort_levels <- function(x, decreasing = TRUE, weight = NULL) {
  
  x <- factor(x)
  if (is.null(weight)) {
    levels.sorted <- names(sort(table(x), decreasing = decreasing))
  } else {
    levels.sorted <- as.character(x)[order(weight, decreasing = decreasing)]
  }
  
  return(levels.sorted)
}


labelMAM <- function(x) {
  
  x <- as.mam(x)
  hour <- x %/% 60
  min <- x %% 60
  lab <- rep("", length(x))
  lab[hour < 12 & hour > 0] <- paste0(hour[hour < 12 & hour > 0], ":",
                                      str_pad(min[hour < 12 & hour > 0], width = 2, pad = "0"),
                                      " a.m.")
  lab[hour > 12] <- paste0(hour[hour > 12] - 12, ":",
                           str_pad(min[hour > 12], width = 2, pad = "0"),
                           " p.m.")
  lab[hour == 0] <- paste0(12, ":",
                           str_pad(min[hour == 0], width = 2, pad = "0"),
                           " a.m.")
  lab[hour == 12] <- paste0(hour[hour == 12], ":",
                            str_pad(min[hour == 12], width = 2, pad = "0"),
                            " p.m.")
  
  return(lab)
}

chart_sort <- function(dt, col, weight = NULL) {
  # This monster of a function that I insisted on putting on one line does the
  # following: counts number of unique entries of "col", either weighted by
  # the column "weight" or straight by N; sorts in ascending order by the
  # counted column (either N or V1); pulls the indices of that sort; cross-
  # references those indices back into the counted column; returns the
  # respective value of "col."
  if (is.null(weight)) {
    return(dt[, .N, by = col][sort.int(dt[, .N, by = col][, N], index.return = TRUE, decreasing = TRUE)$ix, get(col)])
  }
  return(dt[, sum(get(weight)), by = col][sort.int(dt[, sum(get(weight)), by = col][, V1], index.return = TRUE, decreasing = TRUE)$ix, get(col)])
}


addTAZPoints <- function(map, data, group, shp, TAZcol = "TAZ", scale.factor = 1,
                         rad.px = 2, checkBoxes = TRUE, weight = NULL) {
  
  # The layers of points to plot (drop any without records)
  g <- data[[group]]
  if (is.factor(g)) {
    g.table <- table(g)
    layers <- names(g.table)[g.table > 0]
  } else {
    layers <- sort(unique(g))
  }
  
  if (length(layers) > 24) warning("(DEVELOPER) Attempting to add more than 24 dot-density layers. This may result in a very large and unresponsive map. Recommend that you check your data.", immediate. = TRUE, call. = FALSE)
  
  #create a color pallette for the layers
  colorFun <- colorFactor(palette = rainbow(length(layers)), domain = layers, ordered = TRUE)
  
  for (level in layers) {
    if (is.null(weight)) {
      counts <- data[get(group) == level, .N, by = TAZcol]
    } else {
      counts <- data[get(group) == level, .(N = sum(get(weight))), by = TAZcol]
    }
    
    points.list <- list()
    for (i in seq(1, nrow(counts))) {
      points <- NULL
      n <- floor(counts[i, N] / scale.factor)
      if (n < 1) {
        next
      }
      tryCatch(
        {
          points <- spsample(shp[shp@data$TAZ == counts[i, get(TAZcol)], ],
                             n = n, type = "random")@coords
          points.list <- rbind(points.list, points)
        },
        error = function(e) {
          NULL
        }
      )
    }
    if (length(points.list) > 0) {
      lngs <- unlist(points.list[, 1])
      lats <- unlist(points.list[, 2])
      map <- map %>% addCircleMarkers(lng = lngs, lat = lats,
                                      radius = rad.px, fillOpacity = 1,
                                      stroke = FALSE, group = level,
                                      color = colorFun(level))
    }
  }
  if (checkBoxes) {
    map <- map %>% addLayersControl(overlayGroups = c(layers, "Background Map"),
                                    options = layersControlOptions(collapsed = FALSE))
  } else {
    map <- map %>% addLayersControl(overlayGroups = "Background Map",
                                    baseGroups = layers,
                                    options = layersControlOptions(collapsed = FALSE))
  }
  
  layers <- factor(layers, levels = as.character(layers), ordered = TRUE)
  map <- map %>% addLegend(position = "bottomright", pal = colorFun, values = layers)
  
  return(map)
}



# Create a map of grouped TAZ polygons and a function which is used for
# coloring them during mapping
prepTAZPolygons <- function(shp, group.by) {
  # Aggregates polygons by the unique values given in a column of the user's 
  # choice in the @data slot of an sp SpatialPolygonsDataFrame object. Also
  # cleans up the result to minimize memory usage.
  #
  # Args:
  #   shp      : SpatialPolygonsDataFrame object with polygons for each zone
  #   group.by : string naming the column in shp@data that will be the 
  #                grouping column for the geographic aggregation.
  #
  # Returns: 2 items in a list
  #   shp      : SpatialPolygonsDataFrame object with aggregated polygons
  #   colorFun : color mapping function that addTAZLayer() will use for making
  #                the legend
  #
  # Note: If you are trying to use a zone system where a zone might be composed 
  # of many separate polygons (e.g. Alaska and the Aleutian islands), you might 
  # need to change kMaxAreasToKeep to a higher number to keep more polygons in
  # the simplified shapes.
  
  # Aggregate spatially and rename output
  TAZ.polys <- aggregate(x = shp[c(group.by)],
                         by = list(shp@data[[group.by]]),
                         FUN = function(x){return(x[1])})
  TAZ.polys[["Group"]] <- TAZ.polys[["Group.1"]]
  TAZ.polys[["Group.1"]] <- NULL
  
  # TODO: Pick a new color palette or a more flexible one at least
  # Add colors to polygons for plotting
  colorFun <- colorFactor(palette = "Set3", domain = TAZ.polys@data[["Group"]])
  TAZ.polys[["Color"]] <- colorFun(TAZ.polys[["Group"]])
  
  # Remove holes left from polygon aggregation (often due to water bodies between zones)
  # Initialize function
  removePolyHoles <- function(polys.obj) {
    # Selects the single largest area Polygon object out of a Polygons object, and
    # gives a warning if this largest area polygon is marked as a hole. Returns a
    # new Polygons object only containing the chosen Polygon.
    kMaxAreasToKeep <- 5
    areas <- sapply(polys.obj@Polygons, function(x) x@area)
    selected.areas <- sort(areas, decreasing = TRUE)[1:kMaxAreasToKeep]
    list.condition <- sapply(polys.obj@Polygons, function(x) x@area %in% selected.areas & x@hole == FALSE)
    selected.poly.list <- polys.obj@Polygons[list.condition]
    
    selected.polys.obj <- Polygons(selected.poly.list, ID = polys.obj@ID)
    return(selected.polys.obj)
  }
  
  # Use the function just created above to remove holes from aggregated polygons
  TAZ.polys@polygons <- lapply(TAZ.polys@polygons, removePolyHoles)
  return(list(shp = TAZ.polys, colorFun = colorFun))
}



bar_plotter <- function(data, xvar, yvar, 
                        fill = "", position = "stack", 
                        xlabel = xvar, ylabel = yvar, 
                        xrotate = FALSE, yrotate = FALSE, 
                        coord_flip = FALSE, legend_label = TRUE,
                        yvalue = FALSE, png_name = NULL) {
  
  if (fill == "") {
    p <- ggplot(data, aes_q(x = quote(get(xvar)), y = quote(get(yvar)))) +
      geom_bar(stat = "identity", position = position, fill = "#08519c")
  } else {
    fill_levels <- uniqueN(data[,get(fill)])
    p <- ggplot(data, aes_q(x = quote(get(xvar)), y = quote(get(yvar)), fill = quote(get(fill)))) +
      geom_bar(stat = "identity", position = position) +
      labs(fill = fill) 
    if(fill_levels <= nrow(rsgcolordf)-1) {
      p <- p + scale_fill_manual(values = rgb(rsgcolordf[2:(fill_levels+1),],maxColorValue = 255))
    }
  }
  if(yvalue){
    p <- p + geom_text(aes_q(label=quote(get(yvar))), position=position_dodge(width = .9),
                       vjust = 125)
  }
  p <- p + xlab(xlabel) + ylab(ylabel) +
    theme_db
  if (xrotate) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if (yrotate) {
    p <- p + theme(axis.text.y = element_text(angle = 45, hjust = 1))
  }
  if (coord_flip) {
    p <- p + coord_flip()
  }
  if (max(data[, get(yvar)]) >= 1000) {
    p <- p + scale_y_continuous(labels = scales::comma)
  }
  if (max(data[, get(yvar)]) <= 1) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  if (!legend_label) {
    p <- p + labs(fill = "")
  }
  if(!is.null(png_name)){
    ggsave(filename = file.path(SCENARIO_OUTPUT_PATH, png_name), plot = p)
  }
  
  p <- plotly_build(p)
  p[["x"]][["data"]] <- lapply(p[["x"]][["data"]], function(x) {
    x[["text"]] <- gsub(x[["text"]], pattern = "get\\(xvar\\)", replacement = xlabel)
    x[["text"]] <- gsub(x[["text"]], pattern = "get\\(yvar\\)", replacement = ylabel)
    x[["hovertext"]] <- gsub(x[["hovertext"]], pattern = "get\\(xvar\\)", replacement = xlabel)
    x[["hovertext"]] <- gsub(x[["hovertext"]], pattern = "get\\(yvar\\)", replacement = ylabel)
    x[["text"]] <- gsub(x[["text"]], pattern = "get\\(fill\\)", replacement = fill)
    x[["text"]] <- sapply(x[["text"]], function(y) {
      long_num <- as.numeric(gsub(y, pattern = gsub(gsub(y, pattern = "[\\+\\*\\(\\)\\[\\]]*", replacement = "."), pattern = "\\d{4,}\\.{0,1}\\d*", replacement = ""), replacement = ""))
      if (!is.na(long_num)) {
        y <- gsub(y, pattern = "\\d{4,}", replacement = format(floor(long_num), big.mark = ","))
      }
      if (max(data[, get(yvar)]) < 1) {
        short_num <- as.numeric(gsub(y, pattern = gsub(gsub(y, pattern = "[\\+\\*\\(\\)\\[\\]]*", replacement = "."), pattern = "0\\.\\d*", replacement = ""), replacement = ""))
        if (!is.na(short_num)) {
          y <- gsub(y, pattern = "0\\.\\d*", replacement = percent(short_num))
        }
      }
      return(y)
    }, USE.NAMES = FALSE)
    if(x[["type"]]=="scatter") x[["textposition"]] <- "top center"
    return(x)
  })
  
  return(p)
  
}
