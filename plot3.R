################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 3
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

# 3 ----------------------------------------------------------------------------
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# create variable for baltimore grouped by year and type
balt_type <- nei %>% 
        filter(fips == "24510") %>% 
        group_by(year, type) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())
balt_type


# create plot
png("exploratory_analysis_pa\\plot3.png", width = 480, height = 480)

ggplot(data = balt_type, aes(x = year, y = tot_pm25)) + geom_line() +
        facet_wrap(~type, scales = "free") + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Baltimore City PM2.5 Emissions",
             subtitle = "Per Type from 1999 to 2008")
dev.off()
