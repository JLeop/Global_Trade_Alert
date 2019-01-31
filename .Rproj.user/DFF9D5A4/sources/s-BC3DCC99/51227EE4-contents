#######################################################################
# Global Trade Alert: Task 1
# Interventions Plotted Over Time
#
# Imports: mater_plus.Rdata, country_iso_un.csv
# Output: Lineplot depicting the number of interventions from 2009-2018.
# Lineplots with number of interventions per year for each G20 member.
# Plots saved as png.
# Data saved in xlsx.
# 
# J. Leopold, St. Gallen, 2018
#######################################################################

rm(list=ls()) # remove variables
dev.off() # clear graphics
cat("\014") # clear console

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(openxlsx)

# Load functions ----------------------------------------------------------

source("functions/ggplot_interventions.R")

# Load data ---------------------------------------------------------------

country_data <- read_csv2("data/country_iso_un.csv") # ";" delimited
load("data/master_plus.Rdata")
master_data <- as_tibble(master)
rm(master) # keep RAM clean

# Overview Over Data ------------------------------------------------------

str(master_data)
# affected.sector and affected product type chr containing one or multiple 
# numbers.

length(unique(master_data$intervention.id)) == 
  length(master_data$intervention.id)
# The same intervention appears in multiple observations.

# Data Transformation: Number of Interventions Each Year ------------------

# Select info relevant for plot 
interventions_data <- select(master_data, 
                             intervention.id, 
                             date.announced,
                             gta.evaluation, 
                             implementing.jurisdiction)

# Extract only year
interventions_data$year.announced <- 
  format(as.Date(master_data$date.announced,
                 format="%Y-%M-%D"),"%Y")

# Subset data: 
uniq_interventions <- interventions_data %>% 
  select(intervention.id, year.announced, gta.evaluation) %>%
  filter(year.announced > "2008" & year.announced < "2019") %>%
  distinct(intervention.id, .keep_all = TRUE) # Unique interventions by id
  
# Summarize obs for unique combination of year and gta.evaluation
sum_interventions <- uniq_interventions %>% 
  group_by(year.announced, gta.evaluation) %>%
  summarize(count=n()) 

# Add total value by summing up all gta.evaluations
total <- setNames(aggregate(sum_interventions$count, 
                   by=list(sum_interventions$year.announced), 
                   FUN=sum), c("year.announced", "count"))

total <- mutate(total, gta.evaluation = "Total")

# Bind to sum_uniq_interventions data
eval_and_tot_interventions <- bind_rows(sum_interventions, total)
eval_and_tot_interventions$gta.evaluation <- 
  factor(eval_and_tot_interventions$gta.evaluation) 
levels(eval_and_tot_interventions$gta.evaluation) <- 
  c("Green","Amber","Red","Total")

# Plot --------------------------------------------------------------------

# Prepare min and max values within each group for ggrepel

max <- setNames(aggregate(eval_and_tot_interventions$count, 
                          by=list(eval_and_tot_interventions$gta.evaluation), 
                          FUN=max), c("gta.evaluation", "value"))

min <- setNames(aggregate(eval_and_tot_interventions$count, 
                          by=list(eval_and_tot_interventions$gta.evaluation), 
                          FUN=min), c("gta.evaluation", "value"))

repel_labels <- bind_rows(min, max)

# show only the minimum and maximum values in the label column
eval_and_tot_interventions$labels <- eval_and_tot_interventions$count
eval_and_tot_interventions$labels <- ifelse(eval_and_tot_interventions$labels %in% 
                                        repel_labels$value, 
                                        eval_and_tot_interventions$labels,"")


# Create plot -------------------------------------------------------------
# Using function created in separate file.
ggplot_interventions(eval_and_tot_interventions)

# Save Plot as png --------------------------------------------------------

ggsave("plots/Number_of_Interventions.png", 
       width = 16, 
       height = 10, 
       units = c("cm"))

# Save data as xlsx -------------------------------------------------------

total$gta.evaluation <- NULL

wb_long <- createWorkbook()
sheet_eval <- addWorksheet(wb_long, "Evaluated_Interventions")
sheet_tot <- addWorksheet(wb_long, "Total_Interventions")

writeData(wb_long, sheet = sheet_eval, x = sum_interventions)
writeData(wb_long, sheet = sheet_tot, x = total)

# long format
saveWorkbook(wb_long, file = "data/summed_interventions_long.xlsx", overwrite = TRUE)

# wide format
sum_interventions_wide <- spread(sum_interventions, 
                                      key = year.announced,
                                      value = count)
total_wide <- spread(total, 
                     key = year.announced, 
                     value = count)

wb_wide <- createWorkbook()
sheet_eval <- addWorksheet(wb_wide, "Evaluated_Interventions")
sheet_tot <- addWorksheet(wb_wide, "Total_Interventions")

writeData(wb_wide, sheet = sheet_eval, x = sum_interventions_wide)
writeData(wb_wide, sheet = sheet_tot, x = total_wide)

# long format
saveWorkbook(wb_wide, file = "data/summed_interventions_wide.xlsx", overwrite = TRUE)

# Plot for each G20 Member ------------------------------------------------



