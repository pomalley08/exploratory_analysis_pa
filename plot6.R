################################################################################
# Course: Exploratory Data Analysis
# Title:  Programming Assignment - Plot 6
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

# 6 ----------------------------------------------------------------------------
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# filter for pm25 data from baltimore related to motor vechicle SCCs
balt_mv <- nei %>% 
        filter(fips == "24510") %>% 
        group_by(year) %>% 
        summarise(tot_pm25 = sum(Emissions), count = n())
# repeat for LA data
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


# create plot
png("exploratory_analysis_pa\\plot6.png", width = 960, height = 480)
grid.arrange(g, h, ncol = 2)

dev.off()
