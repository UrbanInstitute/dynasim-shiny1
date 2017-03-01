library(readxl)
library(tidyverse)

summary <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC_actuarial_deficitV4.xlsx",
                      sheet = "Shiny Numbers")
  
# Clean dataframe
summary <- summary %>%  
  filter(!is.na(Option)) %>%
  select(`Option Number`, Option, `Insolvency Year`, `Open Group Unfunded Obligation`, `75-Year Actuarial Balance`)
  
# Transform and format Open Group Unfunded Obligation 
summary <- summary %>% 
  mutate(`Open Group Unfunded Obligation` = round(`Open Group Unfunded Obligation` / 1000000, 2)) %>%
  mutate(`Open Group Unfunded Obligation` = paste0("$", `Open Group Unfunded Obligation`, " trillion")) %>%
  mutate(`Open Group Unfunded Obligation` = gsub("\\$-", "-$", `Open Group Unfunded Obligation`))

# Transform and format 75-Year Actuarial Deficit
summary <- summary %>%
  mutate(`75-Year Actuarial Balance` = paste0(round(`75-Year Actuarial Balance` * 100, 2), "%"))

write_csv(summary, "data//summary.csv")