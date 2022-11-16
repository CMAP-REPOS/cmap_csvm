
#Enumerate firms and merge with correspondenses
firm_synthesis_enumerate <- function(Establishments, EstSizeCategories, TAZEmployment){

  # Aggregate the employment data by zones, NAICS, and firm size category
  # 1='1-19',2='20-99',3='100-249',4='250-499',5='500-999',6='1,000-2,499',7='2,500-4,999',8='Over 5,000'
  # Remove records with missing zones and NAICS codes
  # Renaming of some fields
  Firms <- Establishments[!is.na(CBPZONE) & !is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP),
                       .(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                         e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
                       by = .(NAICS6 = Industry_NAICS6_CBP, CountyFIPS = CBPZONE)]

  # Add 2 digit NAICS
  Firms[, EmpCatName := substr(NAICS6, 1, 2)]

  # Melt to create separate rows for each firm size category
  Firms <- melt.data.table(Firms,
                        measure.vars = paste0("e",1:8),
                        variable.name ="esizecat",
                        value.name = "est")

  # Convert esizecat to an integer (1:8)
  Firms[, esizecat := as.integer(esizecat)]
  
  # Synthesize data for missing NAICS/county category 92
  emp_cty_n2 <- TAZEmployment[,.(Emp = sum(Employment)), keyby = .(EmpCatName, CountyFIPS)]
  emp_cty_n2[Firms[,.(Est = sum(est)), by = .(EmpCatName = as.integer(EmpCatName))], Est := i.Est, on = c("EmpCatName")]
  emp_cty_n2[is.na(Est), Est := 0]
  emp_cty_n2_public <- emp_cty_n2[EmpCatName == 92]
  emp_cty_n2_public[emp_cty_n2[EmpCatName != 92, .(Emp = sum(Emp)), by = CountyFIPS], EmpOther := i.Emp, on = "CountyFIPS"]
  emp_cty_n2_public[, PctPublic := Emp/EmpOther]
  
  FirmsMiss <- Firms[, .(est = sum(est)), by = .(CountyFIPS, esizecat)]
  FirmsMiss[emp_cty_n2_public, PctPublic := i.PctPublic, on = "CountyFIPS"]
  FirmsMiss[, estPublic := est * PctPublic]
  FirmsMiss[, estPublic := bucketRound(estPublic)]
  
  Firms <- rbind(Firms,
                         FirmsMiss[, .(NAICS6 = 920000, CountyFIPS, 
                                               EmpCatName = 92, esizecat, est = estPublic)])
  
  # Enumerates the agent businesses using the est variable.
  Firms <- Firms[rep(seq_len(Firms[, .N]), est),]

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

  # Return the enumerated firms table
  return(Firms)

}
