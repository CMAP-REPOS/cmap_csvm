# Assign establishments to Mesozones
firm_synthesis_mesozones <- function(Firms, mzemp){

  # Assign firms from Counties to model Mesozones that are smaller in size -- more like cities
  FirmsCMAP <- Firms[CountyFIPS %in% BASE_FIPS_INTERNAL, .(CountyFIPS, BusID, EmpCatName, Emp)]

  # Assign specific NAICS categories which would be used to locate businesses to tazs
  FirmsCMAP[EmpCatName %in% c("31","32","33"), EmpCatName := "3133"]
  FirmsCMAP[EmpCatName %in% c("44","45"), EmpCatName := "4445"]
  FirmsCMAP[EmpCatName %in% c("48","49"), EmpCatName := "4849"]
  
  # Convert the ranking table to long format
  mzemp <- melt.data.table(mzemp,
                id.vars = c("CountyFIPS", "Mesozone"),
                variable.name = "EmpCatName",
                value.name = "EmpRank")

  mzemp[, EmpCatName := sub("rank", "", as.character(EmpCatName))]
  
  # Merge the rankings dataset to the firms database based on county
  FirmsCMAP <- merge(FirmsCMAP,
                mzemp,
                by = c("CountyFIPS", "EmpCatName"),
                allow.cartesian = TRUE,
                all.x = TRUE)

  # Select candidate tazs based on the industry of the firm, firm size, and ranking of that particular industry in a taz
  FirmsCMAP[, candidate := 0L]
  FirmsCMAP[Emp > 5000 & EmpRank %in% c(9,10), candidate := 1L]
  FirmsCMAP[Emp > 2000 & Emp <= 5000 & EmpRank %in% c(7:10), candidate := 1L]
  FirmsCMAP[Emp > 500 & Emp <= 2000 & EmpRank %in% c(5:10), candidate := 1L]
  FirmsCMAP[Emp > 100 & Emp <= 500 & EmpRank %in% c(4:10), candidate := 1L]
  FirmsCMAP[Emp > 20 & Emp <= 100 & EmpRank %in% c(2:10), candidate := 1L]
  FirmsCMAP[Emp <= 20 & EmpRank %in% c(1:10), candidate := 1L]

  # small number of businesses that did not get a candiate TAZ -
  # allow those to have some candidates (small error is better than omitting the businesses)
  ZeroCand <- FirmsCMAP[,.(Candidates = sum(candidate)), by = BusID][Candidates == 0]$BusID
  FirmsCMAP[BusID %in% ZeroCand, candidate := 1L]

  # Remove non-candidate TAZs
  FirmsCMAP <- FirmsCMAP[candidate == 1,]

  # Generate a random number based on which one of the candidate tazs would be selected
  set.seed(BASE_SEED_VALUE)
  FirmsCMAP[, u := runif(.N)]

  # Assign the taz for which the random number generated is the highest among all candidate tazs
  FirmsCMAP <- FirmsCMAP[FirmsCMAP[,.I[which.max(u)], by = BusID]$V1,]

  # Assign MESOZONES for all firms
  Firms[FirmsCMAP, Mesozone := i.Mesozone, on = "BusID"]
  
  # Return the processed cbp table
  return(Firms)

}
