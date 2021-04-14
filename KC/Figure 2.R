
# Load libraries
library(tidyverse)
library(readxl)
library(stringr)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Load data
climate_opinion_data <- read_excel("data/climate_data_relevant.xlsx")
countries_long_lat <- read_csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv")

# Tidy data

# Is Climate Change real? And its causes
climate_change_real2020 <- climate_opinion_data %>%
  select(COSI01, COUNTRYR, QB1, QB3) %>%
  filter(COSI01 == 2020) %>%
  mutate(real = case_when(str_starts(QB1, "Yes") ~ "believe that climate change is real",
                          str_starts(QB1, "No") ~ "Climate change is not real",
                          str_starts(QB1, "Don't know") ~ "Don't know if climate change is real")) %>%
  mutate(cause = case_when(str_detect(QB3, "human") ~ "mainly due to human activity",
                           str_detect(QB3, "natural") ~ "mainly due to natural phenomenon",
                           str_detect(QB3, "no way") ~ "there's no way to know the cause",
                           str_detect(QB3, "Don't know") ~ "Don't know",
                           (is.na(QB3) && (real == "No")) ~ "Doesn't believe",
                           (is.na(QB3) && (real == "Don't know")) ~ "Don't know")) %>%
  mutate(answer = str_c(real, cause, sep = " and ")) %>%
  mutate(answer = replace_na(answer, replace = "Climate change is not real"))



climate_change_real2020_clean <- climate_change_real2020 %>%
  mutate(answer = case_when(str_starts(answer, "Climate change is not real") ~ "Climate change is not real",
                            str_starts(answer, "Don't know") ~ "Don't know",
                            str_detect(answer, "no way") ~ "Climate change is real and unsure of cause",
                            str_ends(answer, "Don't know") ~ "Climate change is real and unsure of cause",
                            TRUE ~ answer))

climate_change_real2020_summary <- climate_change_real2020_clean %>%
  select(COUNTRYR, answer) %>%
  pivot_longer(-c(COUNTRYR), 
               names_to = "type", 
               values_to = "response") %>%
  select(COUNTRYR, response) %>%
  group_by(COUNTRYR, response) %>%
  summarise(beliefs = n())

climate_change_2020_totals <- climate_change_real2020_clean %>%
  group_by(COUNTRYR) %>%
  summarise(total_respondants = n())

climate_change_real2020_summary1 <- climate_change_real2020_clean %>%
  select(COUNTRYR, answer) %>%
  pivot_longer(-c(COUNTRYR), 
               names_to = "type", 
               values_to = "response") %>%
  mutate(response = case_when(response != "believe that climate change is real and mainly due to human activity" ~ "have mixed opinions about climate change and its causes",
                              TRUE ~ response)) %>%
  select(COUNTRYR, response) %>%
  group_by(COUNTRYR, response) %>%
  summarise(beliefs = n()) %>%
  left_join(climate_change_2020_totals, by = "COUNTRYR") %>%
  mutate(percent = beliefs*100/total_respondants)

climate_change_real2020_summary2 <- climate_change_real2020_summary1 %>%
  group_by(COUNTRYR) %>%
  filter(percent > 50) %>%
  select(COUNTRYR, beliefs_real = response, real_percent = percent) %>%
  mutate(real_percent = round(real_percent))


# What kinds of effects will Climate change have on your region?
climate_change_effects2020 <- climate_opinion_data %>%
  select(COSI01, COUNTRYR, QB5_1) %>%
  filter(COSI01 == 2020)

climate_change_effects2020_summary <- climate_change_effects2020 %>%
  select(COUNTRYR, QB5_1) %>%
  pivot_longer(-c(COUNTRYR), 
               names_to = "type", 
               values_to = "response") %>%
  select(COUNTRYR, response) %>%
  group_by(COUNTRYR, response) %>%
  summarise(beliefs = n())

climate_change_effects2020_summary2 <- left_join(climate_change_effects2020_summary, climate_change_2020_totals, by = "COUNTRYR") %>%
  mutate(percent = beliefs*100/total_respondants) %>%
  filter(percent > 40)

climate_change_effects2020_summary3 <- climate_change_effects2020_summary2[-c(4, 8, 14, 23, 29, 31, 35, 37),] %>%
  select(COUNTRYR, beliefs_effect = response, percent_effect = percent) %>%
  mutate(percent_effect = round(percent_effect))

# What actions are citizens of your country taking to fight climate change?
climate_change_actions2020 <- climate_opinion_data %>%
  select(COSI01, COUNTRYR, QC2_6) %>%
  filter(COSI01 == 2020) %>%
  mutate(answer = case_when(str_detect(QC2_6, "A lot") ~ "A lot or somewhat",
                            str_detect(QC2_6, "Somewhat") ~ "A lot or somewhat",
                            str_detect(QC2_6, "Not") ~ "Not really or not at all"))

climate_change_actions2020_summary <- climate_change_actions2020 %>%
  select(COUNTRYR, answer) %>%
  pivot_longer(-c(COUNTRYR), 
               names_to = "type", 
               values_to = "response") %>%
  select(COUNTRYR, response) %>%
  group_by(COUNTRYR, response) %>%
  summarise(beliefs = n())

climate_change_actions2020_summary2 <- left_join(climate_change_actions2020_summary, climate_change_2020_totals, by = "COUNTRYR") %>%
  mutate(percent = beliefs*100/total_respondants) %>%
  filter(percent > 43.5)

climate_change_actions2020_summary3 <- climate_change_actions2020_summary2[-c(13),] %>%
  select(COUNTRYR, beliefs_action = response, percent_action = percent) %>%
  mutate(percent_action = round(percent_action))

# Join tables to make chernoff faces
chernoff_map_data <- full_join(climate_change_real2020_summary2, climate_change_effects2020_summary3) %>%
  full_join(climate_change_actions2020_summary3) %>%
  rename(country = COUNTRYR)

chernoff_map_data <- chernoff_map_data %>%
  mutate(type = case_when((beliefs_real == "believe that climate change is real and mainly due to human activity") && 
                            (beliefs_effect == "Only negative consequences") && 
                            (beliefs_action == "A lot or somewhat") ~ "yes_neg_some",
                          (beliefs_real == "believe that climate change is real and mainly due to human activity") && 
                            (beliefs_effect == "Only negative consequences") && 
                            (beliefs_action == "Not really or not at all") ~ "yes_neg_not",
                          (beliefs_real == "believe that climate change is real and mainly due to human activity") && 
                            (beliefs_effect == "Both negative and positive consequences") && 
                            (beliefs_action == "A lot or somewhat") ~ "yes_both_some",
                          (beliefs_real == "believe that climate change is real and mainly due to human activity") && 
                            (beliefs_effect == "Both negative and positive consequences") && 
                            (beliefs_action == "Not really or not at all") ~ "yes_both_not",
                          (beliefs_real == "have mixed opinions about climate change and its causes") && 
                            (beliefs_effect == "Only negative consequences") && 
                            (beliefs_action == "A lot or somewhat") ~ "no_neg_some",
                          (beliefs_real == "have mixed opinions about climate change and its causes") && 
                            (beliefs_effect == "Only negative consequences") && 
                            (beliefs_action == "Not really or not at all") ~ "no_neg_not",
                          (beliefs_real == "have mixed opinions about climate change and its causes") && 
                            (beliefs_effect == "Both negative and positive consequences") && 
                            (beliefs_action == "A lot or somewhat") ~ "no_both_some",
                          (beliefs_real == "have mixed opinions about climate change and its causes") && 
                            (beliefs_effect == "Both negative and positive consequences") && 
                            (beliefs_action == "Not really or not at all") ~ "no_both_not"))

chernoff_map_data <- countries_long_lat %>%
  select(country, longitude, latitude) %>%
  mutate(country = str_replace(country, "Mexico", "México")) %>%
  right_join(chernoff_map_data, by = "country")

# make map
chernoffFaces <- iconList(
  yes_neg_some = makeIcon("pics/yes-neg-some.png", iconWidth = 50, iconHeight = 50),
  yes_neg_not = makeIcon("pics/yes-neg-not.png", iconWidth = 50, iconHeight = 50),
  yes_both_some = makeIcon("pics/yes-both-some.png", iconWidth = 50, iconHeight = 50),
  yes_both_not = makeIcon("pics/yes-both-not.png", iconWidth = 50, iconHeight = 50),
  no_neg_some = makeIcon("pics/no-neg-some.png",  iconWidth = 50, iconHeight = 50),
  no_neg_not = makeIcon("pics/no-neg-not.png", iconWidth = 50, iconHeight = 50),
  no_both_some = makeIcon("pics/no-both-some.png", iconWidth = 50, iconHeight = 50),
  no_both_not = makeIcon("pics/no-both-not.png", iconWidth = 50, iconHeight = 50)
)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("What do people think about climate change across the world? <br> People's beliefs on the causes, effects and actions against climate change across 30 countries")
)

caption <- tags$div(
  tag.map.title, HTML("Source: Obs'COP - EDF / IPSOS - October 2020")
)

leaflet(options = leafletOptions(zoomControl = FALSE), chernoff_map_data) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
  addTiles() %>%
  addMarkers(icon = ~chernoffFaces[type], popup = ~paste(as.character(country), ": ", "<B>", as.character(real_percent), "%</B> of  people ", tolower(beliefs_real), ".<br>", 
                                                         "<B>", as.character(percent_effect), "%</B> of people think that climate change will have ", tolower(beliefs_effect), " in their region.", 
                                                         "<br><B>", as.character(percent_action), "%</B> think that citizens are taking ", tolower(beliefs_action), " action to fight climate change in their country."), 
             labelOptions = labelOptions(maxWidth = 10)) %>%
  addControl(title, position = "topleft") %>%
  addControl(caption, position = "bottomright")
