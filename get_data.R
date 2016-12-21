# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads social security trust fund finance measures from simulations 
# of the Bipartisan Policy Center's reforms located at 
# "X:\programs\run912\BPCtabs\Final Spreadsheets\TrustFundSummaryBPC.xlsx".
# The script also cleans the data and then takes the dataframes of BPC options
# which contain measures and turns them into data frames of measures that 
# BPC options. It outputs .csv files. 

# Library and Source Statements
library(readxl)
library(tidyverse)

# Function Definition
readr <- function(sheetnum, linknum) {
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
    select(-contains("na")) %>%
    mutate_all(funs(as.numeric))

  names(temp.xl) <- gsub("\\.\\.", ".", names(temp.xl))
  names(temp.xl) <- gsub("\\.$", "", names(temp.xl))  
  
  write.csv(temp.xl, links[linknum])

  return(temp.xl)
}

combiner <- function(output, var.name) {
  
  output <- data_frame(calendar.year = 2005:2087)
  
  for (i in 1:19) {
    
    temp <- dfs[[i]]
    
    temp <- select_(temp, var.name)   
    
    names(temp) <- bpc.options[i]
    
    output <- cbind(output, temp)
  }
  
  return(tbl_df(output))
  
}

# Executed statements

# Create a vector with the 18 different BPC options
bpc.options <- c("scheduled", "payable", "mini.pia", "tax.ssb", "cap.spouse", 
                 "survivor.js75", "taxmax90", "taxmax90.fica13.4", "fica13.4", 
                 "cola.chaincpi", "reduce.cola", "increase.fra", 
                 "increase.fra.era", "taxmax150000", "taxmax180000", "notaxmax",
                 "fica14", "fica15", "bpc.package")

# Create links for writing the 19 BPC options to .csv files
links <- paste0(bpc.options, ".csv")
links <- paste0("csv_files\\", links)

# Run the function on the 19 different excel sheets
scheduled <- readr(1, 1)            
payable <- readr(2, 2)
mini.pia <- readr(3, 3)
tax.ssb <- readr(4, 4)
cap.spouse <- readr(5, 5)
survivor.js75 <- readr(6, 6)
taxmax90 <- readr(7, 7)
taxmax90.fica13.4 <- readr(8, 8)
fica13.4 <- readr(9, 9)
cola.chaincpi <- readr(10, 10)
reduce.cola <- readr(11, 11)
increase.fra <- readr(12, 12)
increase.fra.era <- readr(13, 13)
taxmax150000 <- readr(14, 14)
taxmax180000 <- readr(15, 15)
notaxmax <- readr(16, 16)
fica14 <- readr(17, 17)
fica15 <- readr(18, 18)
bpc.package <- readr(24, 19)

# Create a list of the 19 data frames
dfs <- list(scheduled, payable, mini.pia, tax.ssb, cap.spouse, survivor.js75,
             taxmax90, taxmax90.fica13.4, fica13.4, cola.chaincpi, reduce.cola,
             increase.fra, increase.fra.era, taxmax150000, taxmax180000, 
             notaxmax, fica14, fica15, bpc.package)

# Create data frame with individual variable from each BPC option
trustfund <- combiner(trustfund, "trust.fund.ratio")
cost.payroll <- combiner(cost.payroll, "cost.taxable.payroll")
solvency <- combiner(solvency, "income.cost")
  
# Write the data to .csv file
write_csv(trustfund, "data//trust_fund_ratio.csv")
write_csv(cost.payroll, "data//cost_payroll.csv")
write_csv(solvency, "data//solvency.csv")