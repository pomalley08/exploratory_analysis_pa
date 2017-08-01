################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 1
# Author: Patrick O'Malley
# Date:   8/1/17
################################################################################

# load libraries
library(tidyverse) # tidyverse packages

# load data --------------------------------------------------------------------
# read in pm2.5 data
getwd()
nei <- readRDS("exploratory_analysis_pa\\summarySCC_PM25.rds")
# read in table mapping SCC digit strings to name of pm2.5 source
scc <- readRDS("exploratory_analysis_pa\\Source_Classification_Code.rds")


# 1 ----------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the United States from 1999 
# to 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# view total emissions by year
tot_em <- nei %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())

# create plot
png("exploratory_analysis_pa\\plot1.png", width = 480, height = 480)
with(tot_em, plot(year, tot_pm25, type = "l", main = "Total PM2.5 Emission per Year"))
# add trendline
tot_lm <- lm(tot_em$tot_pm25 ~ tot_em$year)
abline(tot_lm, col = "red", lwd = 2)
dev.off()
