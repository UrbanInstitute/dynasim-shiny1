# Aaron Williams, Urban Institute Program on Retirement Policy

# This script...

# Library and Source Statements
library(readxl)
library(tidyverse)

# Function Definition
readr <- function(sheetnum) {
  # purpose
  # Args:
  #   sheetnum: sheet number in "TrustFundSummaryBPC.xlsx"
  # Returns: Clean data frame
  
  temp.xl <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC.xlsx",
                          sheet = sheetnum, 
                          col_names = FALSE,
                          skip = 4)

  names(temp.xl) <- make.names(tolower(temp.xl[1, ]), unique = TRUE)

  temp.xl <- temp.xl %>%
    filter(row_number() > 2 & row_number() < 86) %>%
    select(-contains("na"))

  names(temp.xl) <- gsub("\\.\\.", ".", names(temp.xl))
  names(temp.xl) <- gsub("\\.$", "", names(temp.xl))  
  
  write.csv(temp.xl, links[sheetnum])

  return(temp.xl)
}

# Create a vector with the 18 different BPC options
bpc.options <- c("scheduled", "payable", "mini.pia", "tax.ssb", "cap.spouse", 
                 "survivor.js75", "taxmax90", "taxmax90.fica13.4", "fica13.4", 
                 "cola.chaincpi", "reduce.cola", "increase.fra", 
                 "increase.fra.era", "taxmax150000", "taxmax180000", "notaxmax",
                 "fica14", "fica15")

# Create links for writing the 18 BPC options to csv files
links <- paste0(bpc.options, ".csv")
links <- paste0("csv_files\\", links)

# Fun the function on the 18 different excel sheets
scheduled <- readr(1)             
payable <- readr(2)
mini.pia <- readr(3)
tax.ssb <- readr(4)
cap.spouse <- readr(5)
survivor.js75 <- readr(6)
taxmax90 <- readr(7)
taxmax90.fica13.4 <- readr(8)
fica13.4 <- readr(9)
cola.chaincpi <- readr(10)
reduce.cola <- readr(11)
increase.fra <- readr(12)
increase.fra.era <- readr(13)
taxmax150000 <- readr(14)
taxmax180000 <- readr(15)
notaxmax <- readr(16)
fica14 <- readr(17)
fica15 <- readr(18)


# Create a list of the 18 data frames
dfs <- list(scheduled, payable, mini.pia, tax.ssb, cap.spouse, survivor.js75,
             taxmax90, taxmax90.fica13.4, fica13.4, cola.chaincpi, reduce.cola,
             increase.fra, increase.fra.era, taxmax150000, taxmax180000, 
             notaxmax, fica14, fica15)




combiner <- function(option, var.name) {

  option <- data_frame(calendar.year = 2005:2087)
  
  for (i in 1:18) {
    
    temp <- dfs[[i]]
    
    temp <- select_(temp, var.name)   
    
    names(temp) <- bpc.options[i]
    
    output <- cbind(output, temp)
  }
  
  return(tbl_df(output))

}
  
  
combiner(trustfund, "trust.fund.ratio")
  
  






# Trust Fund Ratio
trustfund <- data_frame(calendar.year = 2005:2087)
for (i in 1:18) {
  
  temp <- dfs[[i]]
  
  temp <- select(temp, trust.fund.ratio)   
  
  names(temp) <- bpc.options[i]
  
  trustfund <- cbind(trustfund, temp)
  
}









