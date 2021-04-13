# Load libraries
library(tidyverse)
library(readxl)

# Get data
ghg_cw_data <- read_excel("data/CW_CAIT_GHG_Emissions.xlsx")
temp_data <- read_csv("data/GlobalTemperatures.csv")

# Figure 3
ghg_cw_data_summary <- ghg_cw_data %>%
  filter(Sector == "Total including LUCF")

ghg_cw_world_summary <- na.omit(ghg_cw_data_summary) %>%
  select(-c(Country, Source, Sector)) %>%
  pivot_longer(-c(Gas), names_to = "year",
               values_to = "emission_amt") %>%
  group_by(Gas, year) %>%
  summarise(total_emissions = sum(emission_amt)) %>%
  mutate(year = as.numeric(year))

ggplot(aes(x = year), data = ghg_cw_world_summary) +
  geom_line(aes(y = total_emissions/1000, colour = Gas)) +
  scale_x_continuous(limits = c(1990, 2018),
                     breaks = c(seq(1990, 2018, by = 5), 2018)) +
  scale_colour_manual(values = c("#073b4c","#739099","#118ab2", "#739099", "#739099")) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#d2d2d1", colour = "#d2d2d1")) +
  geom_text(x = 2014, y = 100, label = "Total", colour = "#073b4c") +
  geom_text(x = 2014, y = 74, label = TeX('$CO_2$'), colour = "#118ab2") +
  geom_text(x = 2014, y = 22, label = TeX('$CH_4$'), colour = "#739099") +
  geom_text(x = 2014, y = 11, label = TeX('$N_2O$'), colour = "#739099") +
  geom_text(x = 2014, y = 4, label = "F-gases", colour = "#739099") +
  labs(title = "Greenhouse gas emissions are higher than ever before",
       subtitle = "Total World Greenhouse gas emissions 1990-2018",
       x = "", y = "Emissions (in billions of tonnes)",
       caption = "Source: CAIT Climate Data Explorer via Climate Watch\nhttps://www.climatewatchdata.org/data-explorer/historical-emissions")

# Figure 4
temp_data_summary <- temp_data %>%
  mutate(year = as.numeric(format(dt, "%Y"))) %>%
  select(year, LandAverageTemperature) %>%
  group_by(year) %>%
  summarise(avg_temp = mean(LandAverageTemperature)) %>%
  filter(year >= 1990)

ggplot(aes(x = year), data = temp_data_summary) +
  geom_line(aes(y = avg_temp), colour = "#ef476f") +
  scale_y_continuous(limits = c(8.8, 9.9), 
                     breaks = seq(8.8, 9.9, by = .2)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#d2d2d1", colour = "#d2d2d1")) +
  labs(title = "... and global temperatures continue rising",
       subtitle = "Average yearly temperatures 1990-2015",
       x = "", y = "Temperature (°C)",
       caption = "Source: Berkeley Earth via Kaggle\nhttps://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data")