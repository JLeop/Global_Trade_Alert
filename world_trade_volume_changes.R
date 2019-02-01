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
new_colnames <- world_trade_data["CPB WORLD TRADE MONITOR",]
colnames(world_trade_data) <- new_colnames

# remove first 4 rows
world_trade_data <- tail(world_trade_data, -4)
# remove last row (all NAs)
world_trade_data <- head(world_trade_data, -1)

# remove na columns
world_trade_data <- 
  world_trade_data[,colSums(is.na(world_trade_data)) < 
                     nrow(world_trade_data)]

tail(world_trade_data)
