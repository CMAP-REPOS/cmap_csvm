# Scale employment by industry and mesozone to match control data
firm_synthesis_scaling <- function(Firms, emp_control_taz, TAZ_System, EstSizeCategories){

  # Control data for the CMAP region
  # emp_control_taz:
  # 1. TAZ 1-3632, use for CMAP portion of the model only, <= Mesozone 132
  # 2. NAICS 2 digit (full detail)

  # Update fieldnames/datatypes for consistency
  setnames(emp_control_taz, 
           c("Zone17", "NAICS"), 
           c("TAZ", "EmpCatName"))
  
  # Add an initial allocation to the TAZs if there is no TAZ field yet 
  # (this will be the case for base year firm synthesis but not alternative/future scenarios)
  # Allocation proportional to employment
  # Scaling algorithm will deal with any discrepancies at the TAZ level
  if(!"TAZ" %in% names(Firms)){
    
    taz_prob <- emp_control_taz[, .(Employment = sum(Employment)), by = .(TAZ, Mesozone)]
    taz_prob[, Prob := Employment/sum(Employment), by = Mesozone]
    
    for (mz in BASE_MZ_INTERNAL){
      SampleTAZ <- TAZ_System[mz == Mesozone]$TAZ
      ProbTAZ <- taz_prob[mz == Mesozone]$Prob
      if(length(SampleTAZ) == 1){
        Firms[Mesozone == mz, TAZ := SampleTAZ]
      } else {
        Firms[Mesozone == mz, TAZ := sample(SampleTAZ, size = .N, replace = TRUE, prob = ProbTAZ)]
      }
    }
  }

  # Ensure that the targets and firms are consistent in terms of industrial coverage
  # Function will produce an error if there are zero firms for a category where there is employment
  emp_control_taz <- emp_control_taz[EmpCatName %in% unique(Firms$EmpCatName)]
  
  # Format employment data for rFreight scaling function
  TAZEmployment <- dcast.data.table(emp_control_taz,
                                    TAZ ~ EmpCatName,
                                    fun.aggregate = sum,
                                    value.var = "Employment")
  
  # Scaling function
  Firms <- scaleEstablishmentsTAZEmployment(RegionFirms = Firms, 
                                            TAZEmployment = TAZEmployment[TAZ %in% BASE_TAZ_INTERNAL], 
                                            NewFirmsProportion = BASE_NEW_FIRMS_PROP,
                                            MaxBusID = max(Firms$BusID),
                                            EstSizeCategories = EstSizeCategories)
  
  # Those that were sampled might have the wrong Mesozone and County
  Firms[TAZ_System, Mesozone := i.Mesozone, on = "TAZ"]
  Firms[TAZ_System, CountyFIPS := i.CountyFIPS, on = "Mesozone"]
  
  # Set table structure
  setkey(Firms, BusID)

  # Return results
  return(Firms)

}

