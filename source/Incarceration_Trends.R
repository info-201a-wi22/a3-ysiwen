
#reading data
data <- read.csv("incarceration_trends.csv")
head(data)



#### Counties with highest average Black jail Population

#county with highest average black jail pop
library(dplyr)
data%>%
  group_by(county_name)%>%
  summarise(avg_black_jail_pop = mean(black_jail_pop, na.rm = T))%>%
  arrange(desc(avg_black_jail_pop))


#### Counties with highest average White jail Population

#county with highest average white jail pop
data%>%
  group_by(county_name)%>%
  summarise(avg_white_jail_pop = mean(white_jail_pop, na.rm = T))%>%
  arrange(desc(avg_white_jail_pop))

#### Native Jail Population Rate For last 10 Years

#change in native jail pop rate for last 10 year
data%>%
  filter(year != 3139, year > 2008)%>%
  group_by(year)%>%
  summarise(avg_native_jail_pop_rate = mean(native_jail_pop_rate, na.rm = T))


#Counties with highest Female Jail Population Rate in 2018


#county with highest female jail pop rate in 2018
data%>%
  filter(year == 2018)%>%
  group_by(county_name)%>%
  summarise(avg_female_jail_pop = mean(female_jail_pop_rate))%>%
  arrange(desc(avg_female_jail_pop))

#Female Adult Jail Population By Region

#average female adult jail pop by region
data%>%
  group_by(region)%>%
  summarise(avg_female_adult_jail_pop = mean(female_adult_jail_pop, na.rm = T))

#top 10 states with highest black_jail_pop
data%>%
  group_by(state)%>%
  summarise(avg_black_jail_pop = mean(black_jail_pop, na.rm = T))%>%
  arrange(desc(avg_black_jail_pop))

#Line Plot of TOp 10 States with Highest Black Jail Population in last 10 years
library(ggplot2)
data2 <- data%>%
  filter(year != 3039, year > 2006, year < 2017, state %in% c("DC", "NJ", "FL", "CA", "MD", "LA", "NY", "MA", "PA","GA"))
ggplot(data2, aes(x = year, y = black_jail_pop )) +
  geom_line() +
  facet_wrap(~ state) +
  labs(title = "Top 10 States with Highest Black Jail Population in last 10 years")

#scatter plot of white and black jail population
ggplot(data, aes(x = white_jail_pop, y = black_jail_pop)) +
  geom_point(col = "steelblue") +
  labs(title = "Black Jail Pop Vs White Jail Pop")


#Map for Black Jail Population Rate by Counties
library(usmap)
library(ggplot2)
library(plotly)
plot_usmap(region = "counties", data = data2, values = "black_jail_pop_rate", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Black Jail Population", label = scales::comma
  ) +
  theme(legend.position = "right") +
  labs(title = "Distribution of Black Jail population Rate by Counties")

