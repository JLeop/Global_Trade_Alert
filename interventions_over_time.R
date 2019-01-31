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

# Load Data ---------------------------------------------------------------

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
sum_uniq_interventions <- uniq_interventions %>% 
  group_by(year.announced, gta.evaluation) %>%
  summarize(count=n()) 

# Add total value by summing up all gta.evaluations
total <- setNames(aggregate(sum_uniq_interventions$count, 
                   by=list(sum_uniq_interventions$year.announced), 
                   FUN=sum), c("year.announced", "count"))

total <- mutate(total, gta.evaluation = "Total")

# Bind to sum_uniq_interventions data
sum_uniq_interventions <- bind_rows(sum_uniq_interventions, total)
sum_uniq_interventions$gta.evaluation <- 
  factor(sum_uniq_interventions$gta.evaluation) 
levels(sum_uniq_interventions$gta.evaluation) <- 
  c("Green","Amber","Red","Total")

# Plot --------------------------------------------------------------------

# Prepare min and max values within each group for ggrepel

max <- setNames(aggregate(sum_uniq_interventions$count, 
                          by=list(sum_uniq_interventions$gta.evaluation), 
                          FUN=max), c("gta.evaluation", "value"))

min <- setNames(aggregate(sum_uniq_interventions$count, 
                          by=list(sum_uniq_interventions$gta.evaluation), 
                          FUN=min), c("gta.evaluation", "value"))

repel_labels <- bind_rows(min, max)

# show only the minimum and maximum values in the label column
sum_uniq_interventions$labels <- sum_uniq_interventions$count
sum_uniq_interventions$labels <- ifelse(sum_uniq_interventions$labels %in% 
                                        repel_labels$value, 
                                        sum_uniq_interventions$labels,"")

# Prepare Theme

GTA_theme <-  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        text = element_text(color = "gray20"),
        axis.title.x = element_text(size = 12, vjust = -0.2),
        axis.title.y = element_text(size = 12, vjust = 0.5),
        axis.line = element_line(size = 0.25),
        axis.ticks.x.bottom = element_line(size = 0.25),
        axis.ticks.y.left = element_line(size = 0.25),
        axis.ticks.length = unit(3, "pt"),
        axis.text.x = element_text(vjust = -0.2, face="italic"), 
        axis.text.y = element_text(face="italic"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust=0),
        plot.title = element_text(size = 16, face = "bold", color = "#3B88C8"))

# Create Plot

ggplot(data = sum_uniq_interventions, aes(x = year.announced, y = count,
                                          group = gta.evaluation,
                                          color = gta.evaluation,
                                          label = labels)) + 
  geom_line(size = 0.75) +
  geom_point(shape = 21, size = 3) +
  geom_point(size = 1.5) + 
  geom_text_repel(mapping = aes(label = labels), 
                  label.padding = 0.5,
                  point.padding = 0.5,
                  size = 2.7,
                  nudge_x = 0.1,
                  nudge_y = 150,
                  show.legend = FALSE
                  ) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number of Interventions",
                     limits = c(0,2250),
                     breaks = seq(0, 2500, by = 250)) +
  scale_color_manual(name = "Evaluation by GTA", 
                     values = c("#329141",
                                "#F39B24",
                                "#D22F5A",
                                "#000000"))+
  labs(caption="Global Trade Alert, 2019") +
  ggtitle("Number of Interventions from 2009 - 2018") +
  GTA_theme
  
# Save Plot as png --------------------------------------------------------

ggsave("plots/Number_of_Interventions.png", width = 16, height = 10, units = c("cm"))

# Save data as xlsx -------------------------------------------------------


# Plot for each G20 Member ------------------------------------------------



