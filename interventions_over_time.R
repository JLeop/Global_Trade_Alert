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
# J. Leopold, St. Gallen, 2019
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
source("functions/data_transformation.R")

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



# Export interventions_data for scatterplot in Task 2 ---------------------

write_csv(interventions_data, "data/interventions_data.csv")

# Data Transformation -----------------------------------------------------

# Using functions created in separate file.
sum_interventions <- sum_interventions_data(interventions_data)
total <- total_data(sum_interventions)
# Adding evluation as label and total interventions.
eval_and_tot_interventions <- bind_and_label_data(sum_interventions,
                                                  total,
                                                  ggrepel_labels = TRUE)

# Create plot -------------------------------------------------------------

# Using function created in separate file.
ggplot_interventions(eval_and_tot_interventions)

# Save Plot as png --------------------------------------------------------

ggsave("plots/Number_of_Interventions.png", 
       width = 16, 
       height = 10, 
       units = c("cm"))

# Save data as xlsx (long and wide) ---------------------------------------

total$gta.evaluation <- NULL # Variable needed for plot. Not needed in xlsx.

# Personally I believe the long format is more suitable for the creation
# of plots.

wb_long <- createWorkbook()
sheet_eval <- addWorksheet(wb_long, "Evaluated_Interventions")
sheet_tot <- addWorksheet(wb_long, "Total_Interventions")

writeData(wb_long, sheet = sheet_eval, x = sum_interventions)
writeData(wb_long, sheet = sheet_tot, x = total)

# long format
saveWorkbook(wb_long, 
             file = "data/summed_interventions_long.xlsx", 
             overwrite = TRUE)

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
saveWorkbook(wb_wide, 
             file = "data/summed_interventions_wide.xlsx", 
             overwrite = TRUE)

# Plot for each G20 Member ------------------------------------------------

# checking for duplicates/spelling errors in country names.
country_names <- unique(master_data$affected.jurisdiction)
country_names[order(country_names)]
# Russia != Russian Federation
# United Kindom != United Kingdom of Great Britain and Northern Ireland
# European Union values need to be aggregated from country_data$EU.G20

# select G20 members
G20_names <- country_data %>%
  filter(G20 == 1) %>%
  select(name)
G20_names[[18,1]] <- "United Kingdom"

G20_names[[14,1]] <- "Russia"

# for each member in G20 create and save plot
no_data_available = list() # check which countries do not have data

for (index in c(1:length(G20_names$name))){
  
  # select interventions for each member
  interventions_G20_member <- interventions_data %>%
    filter(implementing.jurisdiction == G20_names[[index,1]]) %>%
    select(intervention.id, gta.evaluation, year.announced)
  
  # sum data for each year
  if(nrow(interventions_G20_member) > 0){ # check if data is available
    # Using functions created in separate file.
    sum_interventions <- sum_interventions_data(interventions_G20_member)
    total <- total_data(sum_interventions)
    # Adding evluation as label and total interventions.
    eval_and_tot_interv_G20 <- bind_and_label_data(sum_interventions,
                                                      total,
                                                      ggrepel_labels = FALSE)
  } else {
    eval_and_tot_interv_G20 <- tibble("year.announced" = c(2009:2018), 
                                      "count" = 0, 
                                      "gta.evaluation" = 'Total',
                                      "labels" = "")
    no_data_available <- c(no_data_available, G20_names[[index,1]]) 
    # add country name to list if there is no data 
  }
  
  # make name shorter to fit plot if the name is longer
  # happens for UK & Ireland and US
  if(nchar(G20_names[[index,1]]) > 20){
    name_split <- strsplit(G20_names[[index,1]], " ")
    country_name <- paste(lapply(name_split, '[[', 1), 
                          lapply(name_split, '[[', 2))
  } else {
    country_name <- G20_names[[index,1]]
  }
  
  print(country_name)
  
  # create plot
  ggplot_interventions(eval_and_tot_interv_G20, 
                       plot_title = paste("Interventions of", 
                                          country_name, "from 2009 to 2018"))
  
  # save plot
  ggsave(paste("plots/Number of Interventions", country_name,".png"), 
         width = 16, 
         height = 10, 
         units = c("cm"))
  
}

# Checking Data of Empty Plots --------------------------------------------

# For these countries no data about interventions is available.
print(no_data_available)
# No data for the European Union.

# European Union Interventions --------------------------------------------

country_name <- "European Union"
  
EU_G20_names <- country_data %>%
  filter(EU.G20 == 1) %>%
  select(name)
EU_G20_names[[4,1]] <- "United Kingdom" # match name in interventions data

# select interventions for each member
interventions_EU <- interventions_data %>%
  filter(implementing.jurisdiction %in% EU_G20_names$name) %>%
  select(intervention.id, gta.evaluation, year.announced)

# sum data for each year
sum_interventions <- sum_interventions_data(interventions_EU)
total <- total_data(sum_interventions)
# Adding evluation as label and total interventions.
eval_and_tot_EU_G20 <- bind_and_label_data(sum_interventions,
                                               total,
                                               ggrepel_labels = FALSE)

# create plot
ggplot_interventions(eval_and_tot_EU_G20, 
                     plot_title = paste("Interventions of", 
                                        country_name, "from 2009 to 2018"))

# save plot
ggsave(paste("plots/Number of Interventions", country_name,".png"), 
       width = 16, 
       height = 10, 
       units = c("cm"))

# Remarks -----------------------------------------------------------------

# Interventions available and plotted only for UK not for UK and 
# Northern Ireland.
