################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 2
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


# 2 ----------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

# filter for Baltimore City, Maryland
balt <- nei %>% 
        filter(fips == "24510") %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())
balt

# create plot
png("exploratory_analysis_pa\\plot2.png", width = 480, height = 480)
with(balt, plot(year, tot_pm25, type = "l", main = "Baltimore City PM2.5 Emission per Year"))
# add trendline
balt_lm <- lm(balt$tot_pm25 ~ balt$year)
abline(balt_lm, col = "red", lwd = 2)
dev.off()
