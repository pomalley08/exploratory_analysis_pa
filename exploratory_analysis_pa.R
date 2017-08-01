################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment
# Author: Patrick O'Malley
# Date:   7/27/17
################################################################################

# This script will contain all of the code used for the exploratory analysis
# additional scripts will be created for each plot required per the assignment

# load libraries
library(tidyverse) # tidyverse packages

# load data --------------------------------------------------------------------
# read in pm2.5 data
getwd()
nei <- readRDS("exploratory_analysis_pa\\summarySCC_PM25.rds")
# read in table mapping SCC digit strings to name of pm2.5 source
scc <- readRDS("exploratory_analysis_pa\\Source_Classification_Code.rds")

str(nei)
str(scc)
head(nei)
head(scc)
table(nei$Pollutant) # all pollutants in table at pm2.5
table(nei$year) # years: 1999, 2002, 2005, 2008
table(nei$type) # non-road, nonpoint, on-road, point

# 1 ----------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the United States from 1999 
# to 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# view total emissions by year
tot_em <- nei %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())
with(tot_em, plot(year, tot_pm25, type = "l", main = "Total PM2.5 Emission per Year"))
# add trendline
tot_lm <- lm(tot_em$tot_pm25 ~ tot_em$year)
abline(tot_lm, col = "red", lwd = 2)
# There is a downward trend in the total yearly pm2.5 emissions from 1999 to 2008

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
with(balt, plot(year, tot_pm25, type = "l", main = "Baltimore City PM2.5 Emission per Year"))
# add trendline
balt_lm <- lm(balt$tot_pm25 ~ balt$year)
abline(balt_lm, col = "red", lwd = 2)
# There is a downward trend in the total yearly pm2.5 emissions for Baltimore City
# from 1999 to 2008, however there is a large jump from 2002 to 2005

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
# plot results
ggplot(data = balt_type, aes(x = year, y = tot_pm25)) + geom_line() +
        facet_wrap(~type, scales = "free") + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Baltimore City PM2.5 Emissions",
             subtitle = "Per Type from 1999 to 2008")
# Point is the only emission type that increased from 1999 to 2008
# all other types (Non-Road, Non-Point, On-Road) all decresed from 1999 to 2008

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

ggplot(data = coal_pm25, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = "tot_pm25")) +
        geom_smooth(aes(y = tot_pm25), method = "lm", se = F) +
        labs(title = "United States PM2.5 Emissions",
             subtitle = "For Coal Combustion from 1999 to 2008") +
        scale_colour_manual("", breaks = c("tot_pm25"),
                            values = c("tot_pm25" = "black"))
# emissions from Coal have been dropping in the US from 1999 to 2008 with a significant
# drop between 2005 and 2008

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

# plot baltimore mv pm2.5 data
ggplot(data = balt_mv, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = "tot_pm25")) +
        geom_smooth(aes(y = tot_pm25), method = "lm", se = F) +
        labs(title = "Baltimore City PM2.5 Emissions",
             subtitle = "For Motor Vehicles from 1999 to 2008") +
        scale_colour_manual("", breaks = c("tot_pm25"),
                            values = c("tot_pm25" = "black"))
# There is a downward trend in the total yearly pm2.5 emissions for Baltimore City
# from 1999 to 2008, however there is a large jump up from 2002 to 2005

# 6 ----------------------------------------------------------------------------
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# filter for pm25 data from baltimore related to motor vechicle SCCs
la_mv <- nei %>% 
        filter(fips == "06037") %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())

# plot LA mv pm2.5 data
ggplot(data = la_mv, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = "tot_pm25")) +
        geom_smooth(aes(y = tot_pm25), method = "lm", se = F) +
        labs(title = "Los Angeles PM2.5 Emissions",
             subtitle = "For Motor Vehicles from 1999 to 2008") +
        scale_colour_manual("", breaks = c("tot_pm25"),
                            values = c("tot_pm25" = "black"))

# create data frame with baltimore and LA data
comp_balt_la <- data.frame(location = c("Baltimore", "Baltimore", "LA", "LA"), 
                           year = rep(0, 4), pm25 = rep(0, 4))
comp_balt_la[1, 2:3] <- balt_mv[1, 1:2] 
comp_balt_la[2, 2:3] <- balt_mv[4, 1:2] 
comp_balt_la[3, 2:3] <- la_mv[1, 1:2] 
comp_balt_la[4, 2:3] <- la_mv[4, 1:2] 

comp_balt_la
balt_mv
# plot baltimore and LA data for comparison
ggplot(data = comp_balt_la, aes(x = year)) + 
        geom_line(aes(y = pm25, color = location))

ggplot(data = comp_balt_la, aes(x = year)) + 
        geom_line(aes(y = pm25, color = location), lwd = 2) +
        geom_point(aes(y = pm25, color = location), size = 4)

# create new plot that retains all years
balt_mv <- cbind(location = rep("Baltimore", 4), balt_mv)
la_mv <- cbind(location = rep("LA", 4), la_mv)
comp_balt_la <- rbind(balt_mv, la_mv)

g <- ggplot(data = comp_balt_la, aes(x = year)) + 
        geom_line(aes(y = tot_pm25, color = location), lwd = 2) +
        geom_point(aes(y = tot_pm25, color = location), size = 4) + 
        labs(title = "Total Change in PM2.5 Emissions",
             subtitle = "For Motor Vehicles from 1999 to 2008",
             y = "Total Change (tons)", x = "Year", color = "Location")
# since LA is much larger and has more vehicles than Baltimore the total graph is
# not as useful for comparison

# create data frame with relative amounts
comp_rel <- comp_balt_la %>% 
        select(-count)
       
comp_rel <- cbind(comp_rel, relative.change = rep(0, 8))
comp_rel
comp_rel[2, 4] <- (comp_rel[2, 3] - comp_rel[1, 3]) / comp_rel[1, 3]
comp_rel[3, 4] <- (comp_rel[3, 3] - comp_rel[1, 3]) / comp_rel[1, 3]
comp_rel[4, 4] <- (comp_rel[4, 3] - comp_rel[1, 3]) / comp_rel[1, 3]

comp_rel[6, 4] <- (comp_rel[6, 3] - comp_rel[5, 3]) / comp_rel[5, 3]
comp_rel[7, 4] <- (comp_rel[7, 3] - comp_rel[5, 3]) / comp_rel[5, 3]
comp_rel[8, 4] <- (comp_rel[8, 3] - comp_rel[5, 3]) / comp_rel[5, 3]

# plot relative data
h <- ggplot(data = comp_rel, aes(x = year)) + 
        geom_line(aes(y = relative.change, color = location), lwd = 2) +
        geom_point(aes(y = relative.change, color = location), size = 4) +
        labs(title = "Relative Change in PM2.5 Emissions",
             subtitle = "For Motor Vehicles from 1999 to 2008",
             y = "Relative Change", x = "Year", color = "Location")

# plot total and relative data for comparison
install.packages("gridExtra")
library(gridExtra)
grid.arrange(g, h, ncol = 2)

# Looking at the relative change in pm2.5 emissions from motor vehicles for LA 
# and Baltimore shows that Baltimore has actually reduced its emissions more than LA