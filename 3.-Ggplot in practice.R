#Data visualization
#Case study: new insights on poverty

library(tidyverse)
library(dslabs)
data(gapminder)

gapminder %>% as_tibble()

#use dplyr

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

gapminder %>% 
  filter(year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() 
  
gapminder %>% 
  filter(year == 2010) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() 

#Faceting
#facet_grid

gapminder %>%
  filter( year %in% c(1962,1980,1990,2000,2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid( .~year)# rows, columns

#facet_wrap

gapminder %>%
  filter( year %in% c(1962,1980,1990,2000,2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap( .~year)+
  theme_excel_new()

#Time Series plots

gapminder %>% filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

#two or more lines

gapminder %>% filter(country %in% c("United States","Peru")) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

#is.na values
countries <- c("South Korea", "Germany")

gapminder %>% filter(country %in% countries & !is.na(fertility))%>%
  ggplot(aes(year,fertility, col = country)) +
  geom_line()

#Labels instead of legends

labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))

gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country))+
  geom_line()+
  geom_text(data = labels, aes(x,y, label = country), size = 5) +
  theme(legend.position = "none")


#Log transformation

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
past_year <- 1970


gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# below, the transformation
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2((dollars_per_day)))) +
  geom_histogram(binwidth = 1, color = "black")

#Visualizing multimodal distributions

#Comparing multiple distributions with boxplots and ridge plots

gapminder%>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate( region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")


#Boxplots
West <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America")
EastAsia <- c("Eastern Asia", "South-Eastern Asia")
LatinAme <- c("Caribbean","Central America","South America")

gapminder <- gapminder %>% 
  mutate(group = case_when(
    region %in% West ~ "West",
    region %in% EastAsia ~ "East Asia",
    region %in% LatinAme ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"
  ))


gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America",
                                          "East Asia", "Sub- Saharan", "West")))

library(ggthemes)

gapminder%>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate( region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(group,dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(alpha = 0.5)

#Ridge plots
install.packages("ggridges")
library(ggridges)

gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2") +
  geom_density_ridges() + #This functions overlays the graphs
  geom_density_ridges(jittered_points = TRUE,) # GIVE US THE POINTS

gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2") +
  geom_density_ridges() + #This functions overlays the graphs
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(height = 0),
                      point_shape = '|', point_size = 3,
                      point_alpha = 1, alpha = 0.7) # GIVE US THE POINTS

#Comparing

past_year <- 1970
present_year <- 2010
years <- c(past_year,present_year)
gapminder %>% 
  filter(year %in% years & !is.na(gdp))%>%
  mutate(west = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color = "black")+
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)
#Intersecting information

country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  pull(country)

country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>%
  pull(country)

country_list <- intersect(country_list_1,country_list_2)


gapminder %>% 
  filter( country %in% country_list & year %in% years & !is.na(gdp))%>%
  mutate(west = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color = "black")+
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)


gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  ggplot(aes(group,dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(year = factor(year)) %>% #If we want to see both years in one plot, we should create a factor
  ggplot(aes(group, dollars_per_day, fill = year)) + # we use fill to separate both years.
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  ggplot(aes(dollars_per_day)) + # we use fill to separate both years.
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)


#Overlaying
gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(group = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)


#### Accesing computed variables ######
#aes(x = dollars_per_day, y  = ..count..)
gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(group = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day,y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125,300)) +
  geom_density(alpha = 0.2, bw = 0.75) + # alpha : level of smoothness # bw: level of density
  facet_grid(year ~ .)

#Comparing multiple distributions with boxplots and ridge plots.

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.125,300)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)


# Order categories by a meaningful value
library(dslabs)
library(tidyverse)
library(ggplot2)
library(dplyr)

#fucntion: reorder
murders %>%
  mutate(murder_rate = total/population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  ggplot(aes(state,murder_rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6))+
  xlab("")

#jitter
heights %>%
  ggplot(aes(sex,height))+
  geom_jitter(width = 0.1, alpha = 0.2) # shows all points within a graph

#Align plots vertically to see horizontal changes and horizontally to see vertical changes

heights %>%
  ggplot(aes(height, ..density..))+
  geom_histogram(binwidth = 1, color = "black") +
  facet_grid(sex~.)

heights %>%
  ggplot(aes(sex,height))+
  geom_boxplot(coef = 3, colour = "#000099")+
  geom_jitter(width = 0.1, alpha = 0.2) +
  ylab("Height in inches")

# To add colours to the graph, click on http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#simple-color-assignment
#Consider transformations

#Plots for two variables

#Slope Charts
# To compare variables of the same type but in different time points.
#Relatively small comparisons.











   
   
   