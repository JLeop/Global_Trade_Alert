#######################################################################
# Global Trade Alert: Task 2
# Interventions to share of 2008 world GDP
#
# Imports: WDI data from R package.
# Output: Scatterplot showing the relationship between the number of
# interventions per country and the share of the 2008 world GDP.
# The Plot is saved as png.
# 
# J. Leopold, St. Gallen, 2019
#######################################################################

rm(list=ls()) # remove variables
dev.off() # clear graphics
cat("\014") # clear console

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(WDI)

# Load data ---------------------------------------------------------------

WDIsearch('gdp')
# WDI data for GDP 

# scatter plot illustrating the relationship between the number of interventions per implementing country and its 
# share of 2008 world GDP

# Include an exponential trend line in the graph.

# save as png