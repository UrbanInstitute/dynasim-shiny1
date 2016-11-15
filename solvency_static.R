## Libraries and Source Files
library(extrafont)
library(grid)
library(RColorBrewer)
library(tidyverse)
library(plotly)

# Source file for Windows
#source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R')
#source('urban_theme_windows.R')

#Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")

# Source file for Mac
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_ggplot_theme.R')

# Load data and gather into long format for ggplot2
solvency.m <- read_csv("data/solvency.csv") %>%
    gather(key = variable, value = value, -calendar.year)

# Build graphic
gg <- solvency.m %>%
    filter(variable == "scheduled" | variable == "payable") %>%
    ggplot(aes(x = calendar.year, y = value, colour = variable)) +
    geom_line(size = 1) +
    scale_y_continuous(expand = c(0,0)) +
    ggtitle("Income-Benefit Ratio for Scheduled Benefits")

ggplotly(gg)