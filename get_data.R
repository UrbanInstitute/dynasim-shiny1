library(readxl)
library(tidyverse)




function (destination, sheetnum)


scheduled <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC.xlsx",
           sheet = 1, 
           col_names = FALSE,
           skip = 3)

names(scheduled) <- make.names(tolower(scheduled[1, ]), unique = TRUE)


scheduled <- scheduled %>%
  filter(row_number() > 2 & row_number() < 85) %>%
  select(-na, -na.1, -NA., -NA..1, -NA..2, -NA..3, -na.2)

names(scheduled) <- gsub("\\.\\.", ".", names(scheduled))
names(scheduled) <- gsub("\\.$", "", names(scheduled))  
  




readr <- function(sheetnum) {
  
  temp.xl <- read_excel("X:\\programs\\run912\\BPCtabs\\Final Spreadsheets\\TrustFundSummaryBPC.xlsx",
                          sheet = sheetnum, 
                          col_names = FALSE,
                          skip = 3)

  names(temp.xl) <- make.names(tolower(temp.xl[1, ]), unique = TRUE)


  temp.xl <- temp.xl %>%
    filter(row_number() > 2 & row_number() < 85) %>%
    select(-na, -na.1, -NA., -NA..1, -NA..2, -NA..3, -na.2)

  names(temp.xl) <- gsub("\\.\\.", ".", names(temp.xl))
  names(temp.xl) <- gsub("\\.$", "", names(temp.xl))  
  
  return(temp.xl)
}


boom <- readr(1)

scheduled <- readr(1)
payable <- readr()
mini.pia <- readr()
tax.ssb <- readr()
cap.spouse <- readr()
survivor.js75 <- readr()
taxmax90 <- readr()
taxmax90.fica13.4 <- readr()
fica13.4 <- readr()
cola.chaincpi <- readr()
reduce.cola <- readr()
increase.fra <- readr()
increase.fra.era <- readr()
taxmax150000 <- readr()
taxmax180000 <- readr()
notaxmax <- readr()
fica14 <- readr()
fica15 <- readr()








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

















