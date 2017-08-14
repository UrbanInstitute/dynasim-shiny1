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

# Function Definitions
readr <- function(sheetname, linknum) {
  # purpose
  # Args:
  #   sheetname: sheet number in "TrustFundSummaryBPC.xlsx"
  # Returns: Clean data frame
  
  temp.xl <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC_actuarial_deficitV4.xlsx", sheet = sheetname, col_names = TRUE, skip = 4)
  
  temp.xl <- temp.xl %>%
    filter(row_number() > 1 & row_number() < 88) %>%
    select(calendar.year = `Calendar Year`,
           trust.fund.ratio = `Trust Fund Ratio`,
           cost.taxable.payroll = `Cost/Taxable Payroll`,
           income.cost = `Income /Cost`)
  
  return(temp.xl)
}

combiner <- function(output, var.name) {
  
  output <- data_frame(calendar.year = 2005:2090)
  
  for (i in 1:21) {
    
    temp <- dfs[[i]]
    
    temp <- select_(temp, var.name)   
    
    names(temp) <- bpc.options[i]
    
    output <- cbind(output, temp)
  }
  
  output <- output %>%
    gather(key = variable, value = value, -calendar.year) %>%
    mutate(variable = ifelse(variable == "scheduled", "Scheduled Law", variable),
           variable = ifelse(variable == "payable", "Payable Law", variable),
           variable = ifelse(variable == "mini.pia", "Annual PIA", variable),
           variable = ifelse(variable == "tax.ssb", "Increase Benefits Taxation", variable),
           variable = ifelse(variable == "cap.spouse", "Cap Spouse Benefits", variable),
           variable = ifelse(variable == "survivor.js75", "75% Survivor Benefit", variable),
           variable = ifelse(variable == "taxmax90", "90% Tax Max", variable),
           variable = ifelse(variable == "taxmax90.fica13.4", "90% Tax Max and 13.4% Payroll Tax", variable),
           variable = ifelse(variable == "fica13.4", "13.4% Payroll Tax", variable),
           variable = ifelse(variable == "cola.chaincpi", "Reduce COLA", variable),
           variable = ifelse(variable == "reduce.cola", "Chained-CPI COLA", variable),
           variable = ifelse(variable == "increase.fra", "Increase FRA", variable),
           variable = ifelse(variable == "increase.fra.era", "Increase FRA and EEA", variable),
           variable = ifelse(variable == "taxmax150000", "$150,000 Tax Max", variable),
           variable = ifelse(variable == "taxmax180000", "$180,000 Tax Max", variable),
           variable = ifelse(variable == "notaxmax", "Eliminate the Tax Max", variable),
           variable = ifelse(variable == "fica14", "14.4% Payroll Tax", variable),
           variable = ifelse(variable == "fica15", "15.4% Payroll Tax", variable),
           variable = ifelse(variable == "bpc.package", "BPC Package", variable),
           variable = ifelse(variable == "bmb", "Basic Minimum Benefit", variable),
           variable = ifelse(variable == "cpie.cola", "Increase COLA", variable))
  
  return(tbl_df(output))
  
}

# Executed statements

# Create a vector with the 18 different BPC options
bpc.options <- c("scheduled", "payable", "mini.pia", "tax.ssb", "cap.spouse", 
                 "survivor.js75", "taxmax90", "taxmax90.fica13.4", "fica13.4", 
                 "cola.chaincpi", "reduce.cola", "increase.fra", 
                 "increase.fra.era", "taxmax150000", "taxmax180000", "notaxmax",
                 "fica14", "fica15", "bpc.package", "BMB", "CPIEcola")

# Run the function on the 19 different excel sheets
scheduled <- readr("current Law Scheduled", 1)      
payable <- readr("current Law Payable", 2)
mini.pia <- readr("1-miniPIA", 3)
tax.ssb <- readr("2-tax ssb", 4)
cap.spouse <- readr("3-capSpouse", 5)
survivor.js75 <- readr("4-survivorjs75", 6)
taxmax90 <- readr("5-TAXMAX90%", 7)
taxmax90.fica13.4 <- readr("5b-TAXMAX90+FICA13.4", 8)
fica13.4 <- readr("5c-FICA13.4", 9)
cola.chaincpi <- readr("COLAChainCPI", 10)
reduce.cola <- readr("6-ReduceCOLA", 11)
increase.fra <- readr("7-IncreaseFRA", 12)
increase.fra.era <- readr("8-IncreaseFRA+ERA", 13)
taxmax150000 <- readr("12-taxmax150000", 14)
taxmax180000 <- readr("taxmax180000", 15)
notaxmax <- readr("noTAXMAX", 16)
fica14 <- readr("Fica14", 17)
fica15 <- readr("Fica15", 18)
bpc.package <- readr("bpcrun5supertax", 19)
bmb <- readr("BMB", 20)
cpie.cola <- readr("CPIEcola", 21)

# Create a list of the 21 data frames
dfs <- list(scheduled, payable, mini.pia, tax.ssb, cap.spouse, survivor.js75,
            taxmax90, taxmax90.fica13.4, fica13.4, cola.chaincpi, reduce.cola,
            increase.fra, increase.fra.era, taxmax150000, taxmax180000, 
            notaxmax, fica14, fica15, bpc.package, bmb, cpie.cola)

# Create data frame with individual variable from each BPC option
trust.fund.ratio <- combiner(trust.fund.ratio, "trust.fund.ratio")
cost.payroll <- combiner(cost.payroll, "cost.taxable.payroll")
solvency <- combiner(solvency, "income.cost")
  
rm(scheduled, payable, mini.pia, tax.ssb, cap.spouse, survivor.js75,
  taxmax90, taxmax90.fica13.4, fica13.4, cola.chaincpi, reduce.cola,
  increase.fra, increase.fra.era, taxmax150000, taxmax180000, 
  notaxmax, fica14, fica15, bpc.package, bmb, cpie.cola)

# Combine into one dataset
trust.fund.ratio <- trust.fund.ratio %>%
  rename(trust.fund.ratio = value) %>%
  mutate(trust.fund.ratio = trust.fund.ratio / 100)

cost.payroll <- cost.payroll %>%
  rename(cost.payroll = value)
solvency <- solvency %>%
  rename(income.cost = value)

temp <- left_join(trust.fund.ratio, cost.payroll)
rm(trust.fund.ratio, cost.payroll)
solvency_measures <- left_join(temp, solvency)
rm(temp, solvency, bpc.options, dfs)

# Write the data to .csv file
write_csv(solvency_measures, "data//solvency_measures.csv")
