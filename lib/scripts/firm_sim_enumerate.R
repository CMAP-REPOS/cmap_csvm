
#Enumerate firms and merge with correspondenses
firm_synthesis_enumerate <- function(cbp, c_cbp_mz, EmpBounds, emp_control_taz, cbp_ag = NULL){

  # Clean the input data, combine with Ag data if needed
  if(!is.null(cbp_ag)){
    # Combine Domestic Non Ag Firm Records with the Ag Firm Records
    # Remove any ag records in cbp and replacing with cbp_ag
    cbp <- rbind(cbp[Industry_NAICS6_CBP >= 113110],
                 cbp_ag)
  }
  
  # Extract just the region data for the 21 county CMAP region
  # For the 21 counties, CBPZONE == county FIPS code
  cbp <- cbp[CBPZONE %in% unique(c_cbp_mz$COUNTY)]

  # Aggregate the employment data by zones, NAICS, and firm size category
  # 1='1-19',2='20-99',3='100-249',4='250-499',5='500-999',6='1,000-2,499',7='2,500-4,999',8='Over 5,000'
  # Remove records with missing zones and NAICS codes
  FirmsDomestic <- cbp[!is.na(CBPZONE) & !is.na(FAFZONE) & !is.na(Industry_NAICS6_CBP),
                       .(e1 = sum(e1), e2 = sum(e2), e3 = sum(e3), e4 = sum(e4),
                         e5 = sum(e5), e6 = sum(e6), e7 = sum(e7), e8 = sum(e8)),
                       by = .(Industry_NAICS6_CBP, CBPZONE)]

  # Add 2 digit NAICS
  FirmsDomestic[, n2 := substr(Industry_NAICS6_CBP, 1, 2)]

  # Melt to create separate rows for each firm size category
  FirmsDomestic <- melt.data.table(FirmsDomestic,
                        measure.vars = paste0("e",1:8),
                        variable.name ="esizecat",
                        value.name = "est")

  # Convert esizecat to an integer (1:8)
  FirmsDomestic[, esizecat := as.integer(esizecat)]
  
  # Synthesize data for missing NAICS/county category 92
  emp_cty_n2 <- emp_control_taz[,.(Emp = sum(Employment)), keyby = .(n2 = NAICS, CBPZONE = CountyFIPS)]
  emp_cty_n2[FirmsDomestic[,.(Est = sum(est)), by = .(n2 = as.integer(n2))], Est := i.Est, on = c("n2")]
  emp_cty_n2[is.na(Est), Est := 0]
  emp_cty_n2_public <- emp_cty_n2[n2 == 92]
  emp_cty_n2_public[emp_cty_n2[n2 != 92, .(Emp = sum(Emp)), by = CBPZONE], EmpOther := i.Emp, on = "CBPZONE"]
  emp_cty_n2_public[, PctPublic := Emp/EmpOther]
  
  FirmsDomesticMiss <- FirmsDomestic[, .(est = sum(est)), by = .(CBPZONE, esizecat)]
  FirmsDomesticMiss[emp_cty_n2_public, PctPublic := i.PctPublic, on = "CBPZONE"]
  FirmsDomesticMiss[, estPublic := est * PctPublic]
  FirmsDomesticMiss[, estPublic := bucketRound(estPublic)]
  
  FirmsDomestic <- rbind(FirmsDomestic,
                         FirmsDomesticMiss[, .(Industry_NAICS6_CBP = 920000, CBPZONE, 
                                               n2 = 92, esizecat, est = estPublic)])
  
  # Enumerates the agent businesses using the est variable.
  FirmsDomestic <- FirmsDomestic[rep(seq_len(FirmsDomestic[, .N]), est),]

  # Estimate the number of employees
  # Draw from the employment range using a draw from the uniform distribution
  set.seed(151)
  FirmsDomestic[, Emp := round(runif(n = .N,
                                     min = EmpBounds[esizecat],
                                     max = EmpBounds[esizecat + 1] - 1))]

  # Add an ID and firm type
  FirmsDomestic[, BusID := .I]

  # Remove uncessary fields
  FirmsDomestic[, est := NULL]

  # Return the enumerated firms table
  return(FirmsDomestic)

}
