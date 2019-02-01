#######################################################################
# Global Trade Alert: Task 1
# Data Transformation Function
#
# This file contains three functions to prepare the data for a line 
# plot.
# 1. Extract the summed unique interventions.
# 2. Extract the total interventions.
# 3. Bind total interventions and summed interventions and add labels
# for ggrepel in plot.
#
# input: dataframe containing intervention.id, year.announced,
# and gta.evaluation 
# output: same data summed up for each year from 2009 to 2018, total
# interventions over the years, labels to show min and max values for 
# each gta.evaluation level.
# 
# J. Leopold, St. Gallen, 2019
#######################################################################


sum_interventions_data <- function(interventions_data){
  
  # Subset data: 
  uniq_interventions <- interventions_data %>% 
    select(intervention.id, year.announced, gta.evaluation) %>%
    filter(year.announced > "2008" & year.announced < "2019") %>%
    distinct(intervention.id, .keep_all = TRUE) # Unique interventions by id
  
  # Summarize obs for unique combination of year and gta.evaluation
  sum_interventions <- uniq_interventions %>% 
    group_by(year.announced, gta.evaluation) %>%
    summarize(count=n()) 
}

# Create total value by summing up all gta.evaluations --------------------

total_data <- function(sum_interventions){
  
  total <- setNames(aggregate(sum_interventions$count, 
                              by=list(sum_interventions$year.announced), 
                              FUN=sum), c("year.announced", "count"))
  
  total <- mutate(total, gta.evaluation = "Total")
  
  return(total)
}

# Bind total to sum_interventions data and add labels --------------------

bind_and_label_data <- function(sum_interventions, 
                                total, 
                                ggrepel_labels = TRUE){

  eval_and_tot_interventions <- bind_rows(sum_interventions, total)
  eval_and_tot_interventions$gta.evaluation <- 
    factor(eval_and_tot_interventions$gta.evaluation) 
  levels(eval_and_tot_interventions$gta.evaluation) <- 
    c("Green","Amber","Red","Total")

  # Labels for ggrepel

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
  eval_and_tot_interventions$labels <- 
    ifelse(eval_and_tot_interventions$labels %in% 
            repel_labels$value, 
           eval_and_tot_interventions$labels,"")
  
  # include function to have no labels for individual plots.
  if (ggrepel_labels == FALSE){
    eval_and_tot_interventions$labels = ""
  }
  
  return(eval_and_tot_interventions)
}