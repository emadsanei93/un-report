library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data %>%
  summarise(averageLifeExp = mean(lifeExp))

# %>% # shift + command + m  (piping)
  
gapminder_data_summarized <- gapminder_data %>% summarise(averageLifeExp = mean(lifeExp))  


#DANGER
# gapminder_data <-gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))  (This is overwriting your data)

gapminder_data %>% 
  summarise(recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarise(average = mean(lifeExp))

# What is the average GPD per capita for the first year in the dataset?

gapminder_data %>% 
  filter(year == min(year)) %>%  #filter just look for the head name of the column you have in your data
  summarise(average = mean(gdpPercap))

#or
my_object <- gapminder_data %>% 
  summarise(average = mean(gpdPercap))
  
  
  


#Calculate the average life expectancy by continent

gapminder_data %>% 
  group_by(continent)
  summarise(average=mean(lifeExp))
  
gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average=mean(lifeExp), min=min(lifeExp))


#mutate()

gapminder_data %>% mutate(gdp = pop * gdpPercap)


gapminder_data %>% 
  mutate(gpd = pop * gdpPercap, popInMillions = pop/1000000)

#select()     to remove columns


gapminder_data %>% 
  select(pop, year)


gapminder_data %>% 
  select(-continent)

# get a data frame only the columns country, continent, year, lifeExp

gapminder_data %>% 
  select(-gdpPercap, -pop)


gapminder_data %>% 
  select(year, starts_with('c'))

gapminder_data %>% 
  select(ends_with("p"))

?select  #To open help for more information about select function
  
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#new dataset

getwd() #where am I?

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)


read_csv("co2-un-data.csv")

temp <- read.csv("co2-un-data.csv")

read_csv("co2-un-data.csv", skip = 1)

co2_emissions_dirty <-read_csv("co2-un-data.csv", skip = 2,
                               col_names = c("region", "country", "year",
                                             "series", "value", "footnotes",
                                             "source"))

co2_emissions_dirty

read_csv("co2-un-data.csv", skip = 1) %>% 
  rename(country=...2)

co2_emissions_dirty %>% 
  select(country, year, series, value)


co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode (series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)


co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode (series, 
                          "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)


#filter out data for just 2005, and drop out the year column

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode (series, 
                          "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)


co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode (series, 
    "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)



inner_join(gapminder_data, co2_emissions)

inner_join(gapminder_data, co2_emissions, by = "country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

ggplot(gapminder_co2, aes(x = gdpPercap, y = Per_percapita_emissions)) +
  geom_point() +
  labs(x = "GPD (per capita)", y = "CO2 emitted (oer capita)")





  