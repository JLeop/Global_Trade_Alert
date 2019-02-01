#######################################################################
# Global Trade Alert: Task 3
# Volume dynamics in world trade since the first crisis-related G20 
# summit held in November 2008
#
# Base year currently 2010 for prices.
#
# J. Leopold, St. Gallen, 2018
#######################################################################

rm(list=ls()) # remove variables
dev.off() # clear graphics
cat("\014") # clear console

# Load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

world_trade_data_xlsx <- 
  read_excel("data/CPB-World-Trade-Monitor-November-2018.xlsx")

# Data Cleaning -----------------------------------------------------------

# transpose
world_trade_data <- t(world_trade_data_xlsx)

# colnames 
new_colnames <- as.vector(world_trade_data["CPB WORLD TRADE MONITOR",])
new_colnames <- gsub(" ", "_", new_colnames)
colnames(world_trade_data) <- new_colnames # works only if data is not a tibble

# remove first 4 rows
world_trade_data <- tail(world_trade_data, -4)
# remove last row (all NAs)
world_trade_data <- head(world_trade_data, -1)

# remove na columns
world_trade_data <- 
  world_trade_data[,colSums(is.na(world_trade_data)) < 
                     nrow(world_trade_data)]

# change first column header
colnames(world_trade_data)[1] <- "year_month"

# change fist column to date
world_trade_data <- as.tibble(world_trade_data)

world_trade_data$year_month <- gsub("m","-", world_trade_data$year_month)
world_trade_data$year_month <- paste(world_trade_data$year_month,"-01", sep = "")

world_trade_data$year_month <- format(as.Date(world_trade_data$year_month), "%Y-%m")

# write_csv(world_trade_data, "data/world_trade_data.csv")

str(world_trade_data)

# Sample to work faster with less data ------------------------------------

short_world_trade_data <- world_trade_data[sample(nrow(world_trade_data), 
                                                  50), ]

# Visualization -----------------------------------------------------------

ggplot(short_world_trade_data, aes(x=year_month, y=World_trade)) + 
  geom_point()

