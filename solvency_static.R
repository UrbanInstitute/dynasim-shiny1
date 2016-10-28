## Libraries

library(extrafont)
library(grid)
library(RColorBrewer)
library(reshape2)
library(tidyverse)

Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Windows
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
#source('urban_theme_windows.R')


# Source file for Mac
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')

# Load data
solvency <- read.csv("solvency.csv", header = TRUE)

# Melt data into long format
solvency.m <- melt(solvency, id = 1)

# Build graphic
ggplot(solvency.m, aes(x = year, y = value, colour = variable)) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Social Security Income/Benefit Ratio for Scheduled Benefits")




ggplot(economics_long, aes(date, value01, colour = variable)) +
geom_line() +
scale_y_continuous(expand = c(0,0)) +
ggtitle('Example line chart')


