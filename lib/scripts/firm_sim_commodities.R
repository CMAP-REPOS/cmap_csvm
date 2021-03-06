# Allocating specific commodities to each establishment
firm_synthesis_commodities <- function(Firms){

  # This function identifies producers who make 2+ commodities (especially wholesalers) and
  # simulates a specific commodity for them based on probability thresholds for multiple commodities
  set.seed(151)
  Firms[, temprand := runif(.N)]

  # For all the NAICS which may produce more than one SCTG commodity, simulate one SCTG commodity using set probability thresholds
  setkey(Firms, Industry_NAICS6_CBP)

  Firms[.(211111), Commodity_SCTG := c(16L, 19L)[1 + findInterval(temprand, c(0.45))]]
  Firms[.(324110), Commodity_SCTG := c(17L, 18L, 19L)[1 + findInterval(temprand, c(0.25, 0.50))]]

  setkey(Firms, n4)

  Firms["4245", Commodity_SCTG := c(1L, 2L, 3L, 4L)[1 + findInterval(temprand, c(0.25, 0.50, 0.75))]] #Farm Product Raw Material Merchant Wholesalers
  Firms["4244", Commodity_SCTG := c(5L, 6L, 7L, 9L)[1 + findInterval(temprand, c(0.25, 0.50, 0.75))]] #Grocery and Related Product Wholesalers
  Firms["4248", Commodity_SCTG := 8L] #Beer, Wine, and Distilled Alcoholic Beverage Merchant Wholesalers
  Firms["4233", Commodity_SCTG := c(10L, 11L, 12L, 25L, 26L)[1 + findInterval(temprand, c(0.10, 0.20, 0.80, 0.90))]] #Lumber and Other Construction Materials Merchant Wholesalers
  Firms["4235", Commodity_SCTG := c(13L, 14L, 31L, 32L)[1 + findInterval(temprand, c(0.25, 0.50, 0.75))]] #Metal and Mineral (except Petroleum) Merchant Wholesalers
  Firms["4237", Commodity_SCTG := c(15L, 33L)[1 + findInterval(temprand, c(0.50))]] #Hardware, and Plumbing and Heating Equipment and Supplies Merchant Wholesalers
  Firms["4247", Commodity_SCTG := c(16L, 17L, 18L,19L)[1 + findInterval(temprand, c(0.25, 0.50, 0.75))]] #Petroleum and Petroleum Products Merchant Wholesalers
  Firms["4246", Commodity_SCTG := c(20L, 21L, 22L,23L)[1 + findInterval(temprand, c(0.25, 0.50, 0.75))]] #Chemical and Allied Products Merchant Wholesalers
  Firms["4242", Commodity_SCTG := 21L] #Drugs and Druggists Sundries Merchant Wholesalers
  Firms["4234", Commodity_SCTG := 24L] #Professional and Commercial Equipment and Supplies Merchant Wholesalers
  Firms["4241", Commodity_SCTG := c(27L, 28L, 29L)[1 + findInterval(temprand, c(0.33, 0.67))]] #Paper and Paper Product Merchant Wholesalers
  Firms["4243", Commodity_SCTG := 30L] #Apparel, Piece Goods, and Notions Merchant Wholesalers
  Firms["4238", Commodity_SCTG := 34L] #Machinery, Equipment, and Supplies Merchant Wholesalers
  Firms["4251", Commodity_SCTG := c(35L, 38L)[1 + findInterval(temprand, c(0.50))]] #Wholesale Electronic Markets and Agents and Brokers
  Firms["4236", Commodity_SCTG := c(35L, 38L)[1 + findInterval(temprand, c(0.50))]] #Electrical and Electronic Goods Merchant Wholesalers
  Firms["4231", Commodity_SCTG := c(36L, 37L)[1 + findInterval(temprand, c(0.50))]] #Motor Vehicle and Motor Vehicle Parts and Supplies Merchant Wholesalers
  Firms["4232", Commodity_SCTG := 39L] #Furniture and Home Furnishing Merchant Wholesalers
  Firms["4239", Commodity_SCTG := 40L] #Miscellaneous Durable Goods Merchant Wholesalers
  Firms["4249", Commodity_SCTG := 40L] #Miscellaneous Nondurable Goods Merchant Wholesalers

  Firms[n2=="42", Industry_NAICS6_Make := paste0(n4, "00")]

  # Remove uncessary fields
  Firms[,c("Industry_NAICS6_CBP", "n4", "temprand") := NULL]

  # Return the processed Firms table
  return(Firms)

}
