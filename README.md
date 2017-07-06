# dynasim-shiny1

Karen Smith used DYNASIM to model many Social Security reforms for the Bipartisan Policy Center's [Report of the Commission on Retirement Security and Personal Savings](http://cdn.bipartisanpolicy.org/wp-content/uploads/2016/06/BPC-Retirement-Security-Report.pdf). These scripts pull data from trust fund summaries created by this analysis which are stored at `X:\programs\run912\BPCtabs\Final Spreadsheets\TrustFundSummaryBPC.xlsx` and build a basic shiny application for interactively visualizing the output.

## Scripts

### get_data.R

This script pulls and cleans data from the first eighteen sheets of `X:\programs\run912\BPCtabs\FinalSpreadsheets\TrustFundSummaryBPC.xlsx`. Each page represents a BPC reform option and contains different measures of the financial health of Social Security. This script creates data frames for each measure of financial health which contain vectors for each BPC reform option and then writes those data frames to .csv files which can be found in the data folder. 

### get_summary_stats.R

This script calculates summary statistics that show up in the text of the Shiny application. 

### app.R

This script takes the .csv files created in get_data.R and stored in the data folder, and turns them into a two column interactive shiny graphic.  

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `solvency_shiny.R` need to be commented out when switching between operating systems. 

### www/

All images and .css go in `www/`

## Built With
* R
* [Shiny](https://shiny.rstudio.com/)

## Authors
* Aaron Williams
* Karen Smith

Data source: Urban Institute's Dynamic Simulation of Income Model (DYNASIM), 2015
