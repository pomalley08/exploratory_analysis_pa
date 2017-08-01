################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 4
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

# 4 ----------------------------------------------------------------------------
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

# filter for the SCCs for all coal combustion sources
table(scc$SCC.Level.One)
coal_comb <- scc %>% 
        filter(grepl("Combustion", SCC.Level.One) & grepl("Coal", SCC.Level.Three))

table(coal_comb$SCC.Level.One)
coal_scc <- droplevels(coal_comb$SCC) 
str(coal_scc) # 80 SCCs related to coal combustion

# filter pm2.5 data for only coal combustion sources
coal_pm25 <- nei %>% 
        filter(SCC %in% coal_scc) %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())
coal_pm25        


# create plot
png("exploratory_analysis_pa\\plot4.png", width = 480, height = 480)

ggplot(data = coal_pm25, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = "tot_pm25")) +
        geom_smooth(aes(y = tot_pm25), method = "lm", se = F) +
        labs(title = "United States PM2.5 Emissions",
             subtitle = "For Coal Combustion from 1999 to 2008") +
        scale_colour_manual("", breaks = c("tot_pm25"),
                            values = c("tot_pm25" = "black"))
dev.off()
