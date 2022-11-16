
#Enumerate firms and merge with correspondenses
firm_synthesis_enumerate <- function(Establishments, EstSizeCategories, TAZEmployment, mzemp){

  # Synthesize data for missing NAICS/county category 92
  emp_cty_n2 <- TAZEmployment[,.(Emp = sum(Employees.SE)), keyby = .(EmpCatName, CountyFIPS)]
  emp_cty_n2[Establishments[,.(Est = sum(est)), by = EmpCatName], Est := i.Est, on = c("EmpCatName")]
  emp_cty_n2[is.na(Est), Est := 0]
  emp_cty_n2_public <- emp_cty_n2[EmpCatName == 92]
  emp_cty_n2_public[emp_cty_n2[EmpCatName != 92, .(Emp = sum(Emp)), by = CountyFIPS], EmpOther := i.Emp, on = "CountyFIPS"]
  emp_cty_n2_public[, PctPublic := Emp/EmpOther]
  
  EstablishmentsMiss <- Establishments[, .(est = sum(est)), by = .(CountyFIPS, esizecat)]
  EstablishmentsMiss[emp_cty_n2_public, PctPublic := i.PctPublic, on = "CountyFIPS"]
  EstablishmentsMiss[, estPublic := est * PctPublic]
  EstablishmentsMiss[, estPublic := bucketRound(estPublic)]
  
  Establishments <- rbind(Establishments,
                          EstablishmentsMiss[, .(NAICS6 = 920000, CountyFIPS, 
                                                 EmpCatName = 92, esizecat, est = estPublic)])
  
  # Enumerates the agent businesses using the est variable.
  Firms <- Establishments[rep(seq_len(Establishments[, .N]), est),]

  # Estimate the number of employees
  # Draw from the employment range using a draw from the uniform distribution
  EmpBounds = c(EstSizeCategories$LowerBound, EstSizeCategories[nrow(EstSizeCategories)]$LowerBound * 2)
  
  set.seed(151)
  Firms[, Emp := round(runif(n = .N,
                                     min = EmpBounds[esizecat],
                                     max = EmpBounds[esizecat + 1] - 1))]

  # Add an ID and firm type
  Firms[, BusID := .I]

  # Remove uncessary fields
  Firms[, est := NULL]
  
  # Assign firms from Counties to Mesozones
  FirmsMZ <- Firms[CountyFIPS %in% BASE_FIPS_INTERNAL, .(CountyFIPS, BusID, EmpCatName, Emp)]
  
  # Assign specific NAICS categories which would be used to locate businesses to tazs
  FirmsMZ[EmpCatName %in% c("31","32","33"), EmpCatName := "3133"]
  FirmsMZ[EmpCatName %in% c("44","45"), EmpCatName := "4445"]
  FirmsMZ[EmpCatName %in% c("48","49"), EmpCatName := "4849"]
  
  # Convert the ranking table to long format
  mzemp <- melt.data.table(mzemp,
                           id.vars = c("CountyFIPS", "Mesozone"),
                           variable.name = "EmpCatName",
                           value.name = "EmpRank")
  
  mzemp[, EmpCatName := sub("rank", "", as.character(EmpCatName))]
  
  # Merge the rankings dataset to the firms database based on county
  FirmsMZ <- merge(FirmsMZ,
                     mzemp,
                     by = c("CountyFIPS", "EmpCatName"),
                     allow.cartesian = TRUE,
                     all.x = TRUE)
  
  # Select candidate tazs based on the industry of the firm, firm size, and ranking of that particular industry in a Mesozone
  FirmsMZ[, candidate := 0L]
  FirmsMZ[Emp > 5000 & EmpRank %in% c(9,10), candidate := 1L]
  FirmsMZ[Emp > 2000 & Emp <= 5000 & EmpRank %in% c(7:10), candidate := 1L]
  FirmsMZ[Emp > 500 & Emp <= 2000 & EmpRank %in% c(5:10), candidate := 1L]
  FirmsMZ[Emp > 100 & Emp <= 500 & EmpRank %in% c(4:10), candidate := 1L]
  FirmsMZ[Emp > 20 & Emp <= 100 & EmpRank %in% c(2:10), candidate := 1L]
  FirmsMZ[Emp <= 20 & EmpRank %in% c(1:10), candidate := 1L]
  
  # small number of businesses that did not get a candiate Mesozone -
  # allow those to have some candidates (small error is better than omitting the businesses)
  ZeroCand <- FirmsMZ[,.(Candidates = sum(candidate)), by = BusID][Candidates == 0]$BusID
  FirmsMZ[BusID %in% ZeroCand, candidate := 1L]
  
  # Remove non-candidate Mesozone
  FirmsMZ <- FirmsMZ[candidate == 1,]
  
  # Generate a random number based on which one of the candidate Mesozone would be selected
  set.seed(BASE_SEED_VALUE)
  FirmsMZ[, u := runif(.N)]
  
  # Assign the taz for which the random number generated is the highest among all candidate Mesozone
  FirmsMZ <- FirmsMZ[FirmsMZ[,.I[which.max(u)], by = BusID]$V1,]
  
  # Assign MESOZONES for all firms
  Firms[FirmsMZ, Mesozone := i.Mesozone, on = "BusID"]
  
  # Add an initial allocation to the TAZs within each Mesozone 
  # Allocation proportional to employment
  taz_prob <- TAZEmployment[, .(Employees.SE = sum(Employees.SE)), by = .(TAZ, Mesozone)]
  taz_prob[, Prob := Employees.SE/sum(Employees.SE), by = Mesozone]
    
  for (mz in BASE_MZ_INTERNAL){
    SampleTAZ <- TAZ_System[mz == Mesozone]$TAZ
    ProbTAZ <- taz_prob[mz == Mesozone]$Prob
    if(length(SampleTAZ) == 1){
      Firms[Mesozone == mz, TAZ := SampleTAZ]
    } else {
      Firms[Mesozone == mz, TAZ := sample(SampleTAZ, size = .N, replace = TRUE, prob = ProbTAZ)]
    }
  }
  
  # Return the enumerated firms table
  return(Firms)

}
