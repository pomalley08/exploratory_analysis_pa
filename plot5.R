################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 5
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

# 5 ----------------------------------------------------------------------------
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

table(scc$SCC.Level.One)
# filter for SCCs of highway vehicle sources 
mv_em <- scc %>% 
        filter(SCC.Level.Two %in% c("Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"))
table(mv_em$SCC.Level.One)
mv_em <- droplevels(mv_em)
table(mv_em$SCC.Level.Two)
table(mv_em$SCC.Level.Four)
mv_scc <- mv_em$SCC

# filter for pm25 data from baltimore related to motor vechicle SCCs
balt_mv <- nei %>% 
        filter(fips == "24510") %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())



# create plot
png("exploratory_analysis_pa\\plot5.png", width = 480, height = 480)
# plot baltimore mv pm2.5 data
ggplot(data = balt_mv, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = "tot_pm25")) +
        geom_smooth(aes(y = tot_pm25), method = "lm", se = F) +
        labs(title = "Baltimore City PM2.5 Emissions",
             subtitle = "For Motor Vehicles from 1999 to 2008") +
        scale_colour_manual("", breaks = c("tot_pm25"),
                            values = c("tot_pm25" = "black"))

dev.off()
