
source("./dev/init_dev.R")

listOMX("./dev/Data_Processed/Skims/htruck_congested_skims.omx")
listOMX("./dev/Data_Processed/Skims/mtruck_congested_skims.omx")
length(BASE_TAZ_INTERNAL)
zone_lookup <- readLookupOMX("./dev/Data_Processed/Skims/htruck_congested_skims.omx",LookupName = "zone_number")
zone_lookup$Lookup[!zone_lookup$Lookup %in% BASE_TAZ_INTERNAL]

mat1 <- readMatrixOMX(OMXFileName = "./dev/Data_Processed/Skims/htruck_congested_skims.omx", MatrixName = "mf522_htruck_cng_p1_time")
str(mat1)

test <- omx_to_dt(matrix_name = "mf522_htruck_cng_p1_time",
                  matrix_path = "./dev/Data_Processed/Skims/htruck_congested_skims.omx",value_col_name = "time",
                  row_lookup_name = "zone_number", col_lookup_name = "zone_number")