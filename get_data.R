library(readxl)
library(tidyverse)

readr <- function(sheetnum) {
  
  temp.xl <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC.xlsx",
                          sheet = sheetnum, 
                          col_names = FALSE,
                          skip = 4)

  names(temp.xl) <- make.names(tolower(temp.xl[1, ]), unique = TRUE)

  temp.xl <- temp.xl %>%
    filter(row_number() > 2 & row_number() < 85) %>%
    select(-contains("na"))

  names(temp.xl) <- gsub("\\.\\.", ".", names(temp.xl))
  names(temp.xl) <- gsub("\\.$", "", names(temp.xl))  
  
  write.csv(temp.xl, links[sheetnum])

  return(temp.xl)
}

bpc.options <- c("scheduled", "payable", "mini.pia", "tax.ssb", "cap.spouse", 
                 "survivor.js75", "taxmax90", "taxmax90.fica13.4", "fica13.4", 
                 "cola.chaincpi", "reduce.cola", "increase.fra", 
                 "increase.fra.era", "taxmax150000", "taxmax180000", "notaxmax",
                 "fica14", "fica15")

links <- paste0(bpc.options, ".csv")
links <- paste0("csv_files\\", links)



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

# 1  scheduled
# 2  payable
# 3  mini.pia
# 4  tax.ssb
# 5  cap.spouse
# 6  survivor.js75
# 7  taxmax90
# 8  taxmax90.fica13.4
# 9  fica13.4
# 10 cola.chaincpi
# 11 reduce.cola
# 12 increase.fra
# 13 increase.fra.era
# 14 taxmax150000
# 15 taxmax180000
# 16 notaxmax
# 17 fica14
# 18 fica15










scheduled <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC.xlsx",
                        sheet = 2, 
                        col_names = FALSE,
                        skip = 4)

names(scheduled) <- make.names(tolower(scheduled[1, ]), unique = TRUE)


scheduled <- scheduled %>%
  filter(row_number() > 2 & row_number() < 85) %>%
  select(-contains("na"))

names(scheduled) <- gsub("\\.\\.", ".", names(scheduled))
names(scheduled) <- gsub("\\.$", "", names(scheduled))  











write.csv(, paste0())

select(-na, -na.1, -NA., -NA..1, -NA..2, -NA..3, -na.2)


