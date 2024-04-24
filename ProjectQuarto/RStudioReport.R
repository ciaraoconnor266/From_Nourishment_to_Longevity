install.packages("tidyverse")
install.packages("plotly")
install.packages("gridExtra")

library(tidyverse)
library(plotly)
library(gridExtra)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")

#final data object
data_join <- full_join(unicef_metadata, unicef_indicator_1)
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = join_by(country, alpha_2_code, alpha_3_code))
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country", "alpha_2_code", "alpha_3_code"))

#world map
map_world <- map_data("world")

#map 1- 1999 life exp
data_join_1999 <- data_join %>%
  filter(year == 1999)

map_data_join_1999 <- full_join(data_join_1999, map_world, by = c("country" = "region"))

ggplot(map_data_join_1999) +
  aes(x = long, y = lat, group = group, fill = LifeExp_at_birth) +
  geom_polygon() +
  labs(
    title = "Life Expectancy in 1999")

#map 2- 1999 gdp
ggplot(map_data_join_1999) +
  aes(x = long, y = lat, group = group, fill = GDP_per_capita) +
  geom_polygon()+
  labs(
    title = "GDP per Capita in 1999")

#map 3- 2021 life exp
data_join_2021 <- data_join %>%
  filter(year == 2021)

map_data_join_2021 <- full_join(data_join_2021, map_world, by = c("country" = "region"))

ggplot(map_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = LifeExp_at_birth) +
  geom_polygon()+
  labs(
    title = "Life Expectancy in 2021")

#map 4- 2021 gdp
ggplot(map_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = GDP_per_capita) +
  geom_polygon() +
  labs(
    title = "GDP per Capita in 2021")

#time series
timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, LifeExp_at_birth, group = country, colour = country) +
  geom_line() +
  labs(
    title = "Life Expectancy over the Years")

ggplotly(timeseries_plot_1)

average_life_expectancy <- data_join %>%
  group_by(country) %>%
  summarize(Average_LifeExp_at_birth = mean(LifeExp_at_birth, na.rm = TRUE))

average_GDP_per_capita <- data_join %>%
  group_by(country) %>%
  summarize(Average_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE))

average_obs_value <- data_join %>%
  group_by(country) %>%
  summarize(Average_obs_value = mean(obs_value, na.rm = TRUE))

#scatter plot
ggplot(data_join) +
  aes(GDP_per_capita, LifeExp_at_birth,) +
  geom_point(fill = "blue") +
  labs(title = "Average GDP per Capita & How it Affects a Countries Life Expectancy") +
  theme_minimal()

#bar chart- fruit/veg consumption
#attempt 1
ggplot(data_join, aes(x = alpha_2_code, y = obs_value)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Fruit/Vegetable Consumption Across the World",
       x = "Country",
       y = "Fruit/Vegetable Consumption")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#attempt 2
ggplot(data_join, aes(x = country, y = obs_value, label = country)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Fruit/Vegetable Consumption Across the World",
       y = "Fruit/Vegetable Consumption") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Bandladesh Bar Chart
bangladesh_data <- data.frame(
  Year = c("2011", "2019"),
  Fruit_Veg_Consumption = c(55.4, 44.8),
  GDP = c(1017.63, 1557.96),
  Life_Expectancy = c(68.81, 72.81))

ggplot(bangladesh_data, aes(x = Year, y = Fruit_Veg_Consumption, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Fruit/Vegetable Consumption in Bangladesh (2011 vs. 2019)",
       x = "Year",
       y = "Fruit/Vegetable Consumption (%)",
       fill = "Year") +
  geom_text(aes(label = paste0(Fruit_Veg_Consumption, "%"), y = Fruit_Veg_Consumption + 2), vjust = -0.5, color = "black") +
  theme_minimal() +
  theme(legend.position = "none")

#Peru Bar Chart
peru_data <- data.frame(
  Year = c("2007", "2019"),
  Fruit_Veg_Consumption = c(12.1, 6.1),
  GDP = c(4397.05, 6550.45),
  Life_Expectancy = c(73.22, 76.16))

ggplot(peru_data, aes(x = Year, y = Fruit_Veg_Consumption, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Fruit/Vegetable Consumption in Peru (2007 vs. 2019)",
       x = "Year",
       y = "Fruit/Vegetable Consumption (%)",
       fill = "Year") +
  geom_text(aes(label = paste0(Fruit_Veg_Consumption, "%"), y = Fruit_Veg_Consumption + 0.5), vjust = -0.5, color = "black") +
  theme_minimal() +
  theme(legend.position = "none")