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
library(ggrepel)
library(lubridate)

# Load data ---------------------------------------------------------------

world_trade_data_xlsx <- 
  read_excel("data/CPB-World-Trade-Monitor-November-2018.xlsx")

# Data consists of percentage changes in World Trade with the base rate of
# 2010 at 14488.3 billion USD.

# Data Cleaning -----------------------------------------------------------

# transpose
world_trade_data <- t(world_trade_data_xlsx)

# colnames 
new_colnames <- as.vector(world_trade_data["X__1",])
colnames(world_trade_data) <- new_colnames 
# works only if data is not a tibble

# Extract base rates ------------------------------------------------------

base_rates_2010 <- as.tibble(world_trade_data)[3,]
# remove na columns
base_rates_2010 <- base_rates_2010[, colSums(is.na(base_rates_2010)) == 0]
base_rates_2010 <- base_rates_2010[, -1]

# Extract Unique Identifiers ----------------------------------------------

iid_names <- as.tibble(world_trade_data)[1:2,]
# remove na columns
iid_names <- iid_names[, colSums(is.na(iid_names)) == 0]
colnames(iid_names) <- iid_names[1,]
iid_names <- iid_names[-1,]

# Further Data Cleaning ---------------------------------------------------

world_trade_data <- world_trade_data[-c(1:4),]
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

world_trade_data$year_month <- gsub("m", "-", 
                                    world_trade_data$year_month)
world_trade_data$year_month <- paste(world_trade_data$year_month,
                                     "-01", sep = "")

world_trade_data <- world_trade_data %>%
  mutate(year_month=ymd(year_month))
  
world_trade_data[,2:length(colnames(world_trade_data))] <- 
  sapply(world_trade_data[,2:length(colnames(world_trade_data))],
         as.numeric)

sapply(world_trade_data, class)

write_csv(world_trade_data, "data/world_trade_data.csv")

# Add G20 meeting to data -------------------------------------------------

world_trade_data$label <- ifelse(world_trade_data$year_month == "2008-11-01",
                           world_trade_data$label <- "G20 Crisis\nMeeting",
                           world_trade_data$label <- "")
                           
# Sample to work faster with less data ------------------------------------

sample_world_trade_data <- world_trade_data[sample(nrow(world_trade_data), 
                                                  50), ]

# Coordinates for G20 Meeting ---------------------------------------------

x_g20_point <- world_trade_data$year_month[world_trade_data$label != ""]
y_g20_point <- world_trade_data$tgz_w1_qnmi_sn[world_trade_data$label != ""]

# Visualization of Relative Change ----------------------------------------

ggplot(world_trade_data, aes(x=year_month, y=tgz_w1_qnmi_sn)) + 
  geom_line(color = "#3B88C8", size = 0.75) +
  geom_point(mapping = aes(x = x_g20_point, y = y_g20_point)) + 
  geom_point(mapping = aes(x = x_g20_point, y = y_g20_point), 
             shape = 21, size = 4, stroke = 0.75) +
  geom_text_repel(mapping = aes(label = label), 
                  point.padding = 0.3,
                  size = 2.7,
                  nudge_x = 4,
                  nudge_y = -10,
                  show.legend = FALSE) +
  scale_x_date(name = "Year",
               limits = c(ymd("2005-01-01"), ymd("2018-01-01"))) + 
  scale_y_continuous(name = "World Trade Volume (in %)",
                     limits = c(75, 
                                max(world_trade_data$tgz_w1_qnmi_sn))) +
  labs(caption="Global Trade Alert, 2019", 
       subtitle = "Base rate (2010): 100% = 14488.29 billion USD") +
  ggtitle("Relative Changes in World Trade Volume") +
  theme_minimal() +
  theme(text = element_text(color = "gray20"),
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
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", 
                                  color = "#3B88C8"))

# Save Plot ---------------------------------------------------------------

ggsave("plots/World Trade Volume in Percent.png", 
       width = 16, 
       height = 10, 
       units = c("cm"))

# Absolute Change in World Trade Volume -----------------------------------

world_trade_data$tgz_w1_qnmi_sn_abs <- world_trade_data$tgz_w1_qnmi_sn * as.numeric(base_rates_2010$tgz_w1_qnmi_sn)/1000

# Visualization -----------------------------------------------------------

y_g20_point <- world_trade_data$tgz_w1_qnmi_sn_abs[world_trade_data$label != ""]

ggplot(world_trade_data, aes(x=year_month, y=tgz_w1_qnmi_sn_abs)) + 
  geom_line(color = "#3B88C8", size = 0.75) +
  geom_point(mapping = aes(x = x_g20_point, y = y_g20_point)) + 
  geom_point(mapping = aes(x = x_g20_point, y = y_g20_point), 
             shape = 21, size = 4, stroke = 0.75) +
  geom_text_repel(mapping = aes(label = label), 
                  point.padding = 0.3,
                  size = 2.7,
                  nudge_x = 4,
                  nudge_y = -10,
                  show.legend = FALSE) +
  scale_x_date(name = "Year",
               limits = c(ymd("2005-01-01"), ymd("2018-01-01"))) + 
  scale_y_continuous(name = "World Trade Volume (in Trillion USD)") +
  labs(caption="Global Trade Alert, 2019") +
  ggtitle("Changes in World Trade Volume") +
  theme_minimal() +
  theme(text = element_text(color = "gray20"),
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
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0),
        plot.subtitle = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", 
                                  color = "#3B88C8"))


# Save --------------------------------------------------------------------

ggsave("plots/World Trade Volume USD.png", 
       width = 16, 
       height = 10, 
       units = c("cm"))

