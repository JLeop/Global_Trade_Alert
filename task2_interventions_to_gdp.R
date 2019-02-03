#######################################################################
# Global Trade Alert: Task 2
# Interventions to share of 2008 world GDP
#
# Imports: WDI data from R package.
# Output: Scatterplot showing the relationship between the number of
# interventions per country and the share of the 2008 world GDP.
# The Plot is saved as png.
#
# Documentation on WDI Indicators https://data.worldbank.org/indicator
# 
# J. Leopold, St. Gallen, 2019
#######################################################################

rm(list=ls()) # remove variables
dev.off() # clear graphics
cat("\014") # clear console

# Load packages -----------------------------------------------------------

library(tidyverse)
library(WDI)
library(ggrepel)
# library(fuzzyjoin) # not suitable

# Load data ---------------------------------------------------------------

WDI_indicators <- as_tibble(WDIsearch('gdp'))
WDI_GDP_indicators <- WDI_indicators[grep("current US", 
                                          WDI_indicators$name),]

# WDI data: GDP for all available countries. 
WDI_GDP_08_data <- WDI(indicator='NY.GDP.MKTP.CD', country = "all", start=2008, end=2008)
WDI_GDP_08_data <- na.omit(WDI_GDP_08_data)

world_gdp_08 <- WDI_GDP_08_data %>%
  filter(country == "World") %>%
  select('NY.GDP.MKTP.CD')

# Interventions Data
interventions_data <- read_csv("data/interventions_data.csv")
# Regions for plot
economist_data <- read_csv("data/EconomistData.csv")

# Data Transformation and Cleaning ----------------------------------------

WDI_GDP_08_data <- na.omit(WDI_GDP_08_data)

# scaled to billion USD
WDI_GDP_08_data$NY.GDP.MKTP.CD <- WDI_GDP_08_data$NY.GDP.MKTP.CD/world_gdp_08$NY.GDP.MKTP.CD

# rename and select columns 
WDI_GDP_08_data <- WDI_GDP_08_data %>%
  rename(share_world_gdp = NY.GDP.MKTP.CD) %>%
  select(share_world_gdp, country)

# sum data by country to get total interventions
interventions_08_data <- interventions_data %>%
  select(intervention.id, implementing.jurisdiction, year.announced) %>%
  filter(year.announced=="2008") %>%
  group_by(implementing.jurisdiction) %>%
  summarize(interventions_count=n()) %>%
  rename(country = implementing.jurisdiction)

# sum data by country to get total interventions
interventions_08_to_18_data <- interventions_data %>%
  select(intervention.id, implementing.jurisdiction, year.announced) %>%
  filter(year.announced > "2007" & year.announced < "2019") %>%
  group_by(implementing.jurisdiction) %>%
  summarize(interventions_count=n()) %>%
  rename(country = implementing.jurisdiction)

unique(interventions_08_data$country) # data for 75 unique countries in 2008
unique(interventions_08_to_18_data$country) # data for 137 unique countries 
# from 2008 to 2018

# Matching Counrty Names --------------------------------------------------

# check which country names are different in the two data sets
diff_names <- setdiff(interventions_08_data$country, 
                            WDI_GDP_08_data$country)

diff_names <- c(diff_names, setdiff(WDI_GDP_08_data$country, 
                                    interventions_08_data$country))

length(diff_names)

diff_names[order(diff_names)]

# Fuzzy Matching ----------------------------------------------------------
# Does not work well. To imprecise e.g. Ireland = Iceland, Iraq = Iran

# interventions_08_to_18_data %>%
#   stringdist_left_join(WDI_GDP_08_data, 
#                        by = "country", 
#                        max_dist = 1)

# Cleaning of names -------------------------------------------------------

interventions_08_data$country <- gsub("&", " and ", 
                                      interventions_08_data$country)

# Join dataframes ---------------------------------------------------------

GDP_interventions_08_data <- left_join(interventions_08_data, 
                                       WDI_GDP_08_data, 
                                       by = "country")

# Check which countries could not be matched:
not_matched <- GDP_interventions_08_data %>%
  filter(is.na(share_world_gdp))

length(not_matched$country) # 7 not matched

# Manually correct not matching names -------------------------------------

intervention_data_names <- as.list(not_matched$country)
intervention_data_names[[5]] <- NULL
WDI_names <- list("Czech Republic", "Korea, Rep.", 
                      "Russian Federation", "Slovak Republic", 
                      "United States", "Venezuela, RB")

for (i in 1:length(intervention_data_names)){
  WDI_GDP_08_data$country <- gsub(WDI_names[i], 
                                  intervention_data_names[i], 
                                        WDI_GDP_08_data$country)
}

GDP_interventions_08_data <- left_join(interventions_08_data, 
                                       WDI_GDP_08_data, by = "country")

# only Swaziland is NA
GDP_interventions_08_data <- na.omit(GDP_interventions_08_data)


# Add Region --------------------------------------------------------------

region_data <- economist_data %>%
  select(Country, Region) %>%
  distinct(Country, .keep_all = TRUE) %>%
  rename(country = Country)

region_data$Region <- factor(region_data$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))

GDP_interventions_08_data <- left_join(GDP_interventions_08_data, 
                                       region_data, by = "country")


# Manually add NA values
GDP_interventions_08_data$Region[19] <- "Central &\nEastern Europe" # Czechia
GDP_interventions_08_data$Region[55] <- "Asia &\nOceania" # Republic of Korea
GDP_interventions_08_data$Region[68] <- "OECD" # United Kindom
GDP_interventions_08_data$Region[69] <- "Americas" # United States
GDP_interventions_08_data$Region[73] <- "Asia &\nOceania" # Vietnam


# Add Labels --------------------------------------------------------------

GDP_interventions_08_data$labels <- 
  ifelse(GDP_interventions_08_data$share_world_gdp > 0.04 | 
           GDP_interventions_08_data$interventions_count > 250, 
         GDP_interventions_08_data$country, "")

# Scatter plot ----------------------------------------------------------

ggplot(GDP_interventions_08_data, aes(x=share_world_gdp, 
                                      y=interventions_count,
                                      label = labels)) +
  geom_smooth(method = 'lm',
              aes(linetype = "Exp.\nTrend"),
              color = '#D22F5A',
              fill = '#b5b5b5',
              formula = y ~ exp(x)) +
  geom_point(aes(colour=Region), shape=21, fill = 'white', size=2.5, stroke=1) +
  geom_text_repel(mapping = aes(label = labels), 
                  point.padding = 0.3,
                  size = 2.7,
                  nudge_x = 0.01,
                  nudge_y = 20,
                  show.legend = FALSE) +
  scale_y_continuous(name = "Total of Interventions",
                     limits = c(-30,1600),
                     breaks = seq(0,1600, by = 200)) +
  scale_x_continuous(name = "Share of World GDP") +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#96503F",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#099DD7",
                                "#000000"),
                     guide = guide_legend(nrow = 1, order=1)) +
  labs(caption="Global Trade Alert, 2019") +
  ggtitle("Total Interventions to Share of World GDP in 2008") +
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
        plot.title = element_text(size = 14, face = "bold", color = "#3B88C8"))
  
# Save as .png
ggsave("plots/Interventions to Share of GDP 2008.png", 
       width = 16, 
       height = 10, 
       units = c("cm"))
