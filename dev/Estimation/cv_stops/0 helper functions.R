# Cross-validation function for hurdle models using hurdle function
crossvalidate_hurd <- function(formula, data, zero.dist = "binomial", dist = "poisson", link = "logit", k = 10, seed = 0, verbose = TRUE) {
  
  set.seed(seed)
  folds <- sample(1:k, size = nrow(data), replace = TRUE)
  errors <- c()
  AICm <- c()
  BICm <- c()
  
  # Run the cv models in parallel
  fit_ls = foreach(fold=seq_len(k), .packages = c("data.table", "pscl")) %dopar% {
    if (verbose) cat(".")
    data[, ss := folds != fold]
    fit <- pscl::hurdle(formula, data, subset = ss, zero.dist = zero.dist, dist = dist, link = link)
    y_hat <- predict(fit, newdata = data[folds == fold])
    mse <- mean((data[folds == fold, STOPS] - y_hat)^2)
    cAIC <- round(AIC(fit), 1)
    cBIC <- round(BIC(fit), 1)
    cat("Fold No: ", fold, "\t")
    cat("MSE: ", round(mse, 5), "\t")
    cat("AIC: ", cAIC, "\t")
    cat("BIC: ", cBIC, "\n")
    list("CV Err"=mse, "AICc" = cAIC, "BICc" = cBIC)
  }
  
  # Gather the stats of CV run
  fit_stat = do.call(rbind.data.frame, fit_ls)
  ave_fit_stat = apply(fit_stat, 2, mean)
  
  # # Run model on all data
  fit <- hurdle(formula, data, zero.dist = zero.dist, dist = dist, link = link)
  
  # Get significance stars
  stats <- summary(fit)[["coefficients"]]
  count_stars <- rep("", nrow(stats[["count"]]))
  count_stars[stats[["count"]][, 4] < 0.1] <- "."
  count_stars[stats[["count"]][, 4] < 0.05] <- "*"
  count_stars[stats[["count"]][, 4] < 0.01] <- "**"
  count_stars[stats[["count"]][, 4] < 0.001] <- "***"
  zero_stars <- rep("", nrow(stats[["zero"]]))
  zero_stars[stats[["zero"]][, 4] < 0.1] <- "."
  zero_stars[stats[["zero"]][, 4] < 0.05] <- "*"
  zero_stars[stats[["zero"]][, 4] < 0.01] <- "**"
  zero_stars[stats[["zero"]][, 4] < 0.001] <- "***"
  
  
  return(list(CVerr = ave_fit_stat["CV.Err"], zero.dist = zero.dist, dist = dist,
              zero_stars = zero_stars, count_stars = count_stars, link = link, fit = fit,
              AICm = ave_fit_stat["AICc"], BICm = ave_fit_stat["BICc"]))
  
}
# Cross-validation function for hurdle models using glmmtmb
crossvalidate_glmmtmb <- function(formula, ziformula, data, zero.dist = "binomial", dist = "truncated_poisson", k = 10, seed = 0, verbose = TRUE) {
  
  set.seed(seed)
  folds <- sample(1:k, size = nrow(data), replace = TRUE)
  errors <- c()
  AICm <- c()
  BICm <- c()
  
  if(dist == "truncated_poisson"){
    t_family = truncated_poisson()
  } else if (dist == "truncated_genpois"){
    t_family = truncated_genpois()
  } else if (dist == "truncated_nbinom1"){
    t_family = truncated_nbinom1()
  } else {
    t_family = truncated_nbinom2()
  }
  
  # Cross validate using parallel processing
  fit_ls = foreach(fold=seq_len(k), .packages = c("data.table", "pscl", "glmmTMB")) %dopar% {
    if (verbose) cat(".")
    fit <- glmmTMB(formula, data[folds != fold], family = t_family, ziformula = ziformula)
    y_hat <- predict(fit, newdata = data[folds == fold])
    mse <- mean((data[folds == fold, STOPS] - y_hat)^2)
    cAIC <- AIC(fit)
    cBIC <- BIC(fit)
    cat("Fold No: ", fold, "\t")
    cat("MSE: ", round(mse, 5), "\t")
    cat("AIC: ", cAIC, "\t")
    cat("BIC: ", cBIC, "\n")
    list("CV Err"=mse, "AICc" = cAIC, "BICc" = cBIC)
  }
  
  # Gather the CV stats
  cat("\n")
  fit_stat = do.call(rbind.data.frame, fit_ls)
  ave_fit_stat = apply(fit_stat, 2, mean)
  
  # Run model on all data
  fit <- glmmTMB(formula, data, family = t_family, ziformula = ziformula, verbose = verbose)
  
  # Get significance stars
  stats <- summary(fit)[["coefficients"]]
  count_stars <- rep("", nrow(stats[["cond"]]))
  count_stars[stats[["cond"]][, 4] < 0.1] <- "."
  count_stars[stats[["cond"]][, 4] < 0.05] <- "*"
  count_stars[stats[["cond"]][, 4] < 0.01] <- "**"
  count_stars[stats[["cond"]][, 4] < 0.001] <- "***"
  zero_stars <- rep("", nrow(stats[["zi"]]))
  zero_stars[stats[["zi"]][, 4] < 0.1] <- "."
  zero_stars[stats[["zi"]][, 4] < 0.05] <- "*"
  zero_stars[stats[["zi"]][, 4] < 0.01] <- "**"
  zero_stars[stats[["zi"]][, 4] < 0.001] <- "***"
  
  return(list(CVerr = ave_fit_stat["CV.Err"], zero.dist = zero.dist, dist = dist,
              zero_stars = zero_stars, count_stars = count_stars, link = "logit", fit = fit,
              AICm = ave_fit_stat["AICc"], BICm = ave_fit_stat["BICc"]))
  
}

fillWhiteSpace <- function(x, n) {
  
  nws <- n - sapply(x, FUN = nchar)
  for (i in 1:length(x)) x[i] <- paste0(x[i], paste0(rep(" ", nws[i]), collapse = ""), collapse = "")
  return(x)
  
}

appendResults <- function(results, tab = NULL) {
  
  # Extract model details and statistics
  n <- 3 # number of white spaces
  ws <- paste0(rep(" ", n), collapse = "")
  zero.dist <- results[["zero.dist"]]
  link <- results[["link"]]
  dist <- results[["dist"]]
  loglik <- results[["fit"]][["loglik"]]
  AIC_ <- AIC(results[["fit"]])
  BIC_ <- BIC(results[["fit"]])
  AIC_ave <- results[["AICm"]]
  BIC_ave <- results[["BICm"]]
  cverr <- results[["CVerr"]]
  zero_coefs <- summary(results[["fit"]])[["coefficients"]][["zero"]][, 1, drop = FALSE]
  count_coefs <- summary(results[["fit"]])[["coefficients"]][["count"]][, 1, drop = FALSE]
  zero_stars <- fillWhiteSpace(results[["zero_stars"]], n)
  count_stars <- fillWhiteSpace(results[["count_stars"]], n)
  param_fit <- length(zero_coefs)+length(count_coefs)
  
  modelstats <- data.frame(c(paste(zero.dist, ws), paste(link, ws), paste(dist, ws), paste(round(loglik, 1), ws), paste(round(AIC_, 1), ws), paste(round(BIC_, 1), ws), paste(round(AIC_ave, 1), ws), paste(round(BIC_ave, 1), ws), paste(param_fit, ws), paste(round(cverr, n), ws), " ", " ", paste(round(zero_coefs, 4), zero_stars), " ", " ", paste(round(count_coefs, 4), count_stars)))
  rownames(modelstats) <- c("Zero Model Distribution", "Zero Model Link Function", "Count Model Distribution", "Log-Likelihood", "AIC", "BIC", "CV AIC", "CV BIC", "Parameters", "CV MSE", "", "Zero Model Coefficients", paste("ZM -", rownames(zero_coefs)), "  ", "Count Model Coefficients", paste("CM -", rownames(count_coefs)))
  colnames(modelstats) <- paste("Model1", ws)
  modelstats[, 1] <- as.character(modelstats[, 1])
  
  if (is.null(tab)) {
    
    tab <- modelstats
    
  } else {
    
    M <- ncol(tab) + 1
    tab <- cbind(tab, rep("", nrow(tab)))
    colnames(tab)[M] <- paste0("Model", M, " ", ws)
    tab[, M] <- as.character(tab[, M])
    for (row in rownames(tab)) {if (row %in% rownames(modelstats) & row != "") tab[row, M] <- modelstats[row, ]}
    
    for (row in rownames(modelstats)) {
      
      if (!row %in% rownames(tab)) {
        
        if (substr(row, 1, 2) == "ZM") {
          last_zero <- match("  ", rownames(tab))
          tab <- rbind(tab[1:(last_zero-1),], rep("", M), tab[last_zero:nrow(tab),])
          tab[last_zero, M] <- modelstats[row,]
          rownames(tab)[last_zero] <- row
        }
        if (substr(row, 1, 2) == "CM") {
          
          tab <- rbind(tab, rep("", M))
          tab[nrow(tab), M] <- modelstats[row,]
          rownames(tab)[nrow(tab)] <- row
          
        }
      }
    }
  }
  
  return(tab)
  
}
