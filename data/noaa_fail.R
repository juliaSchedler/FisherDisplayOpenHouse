#https://www.ncei.noaa.gov/data/local-climatological-data/doc/LCD_documentation.pdf

## 2015-2024
weather_15 <- read.csv("data/3998904.csv") %>% 
  select(DATE, 
         MonthlyMeanTemperature, 
         MonthlyMaximumTemperature,
         MonthlyMinimumTemperature)
## 2005-2014
weather_05 <- read.csv("data/3998979.csv")%>% 
  select(DATE, 
         MonthlyMeanTemperature, 
         MonthlyMaximumTemperature,
         MonthlyMinimumTemperature)
## 1995-2004
weather_95 <- read.csv("data/3998980.csv")%>% 
  select(DATE, 
         MonthlyMeanTemperature, 
         MonthlyMaximumTemperature,
         MonthlyMinimumTemperature)
## 1985-1994
## 1975-1984

weather <- bind_rows(weather_95, weather_05, weather_15) %>%
  mutate(date = as.Date(DATE))

monthly_weather <- weather %>% filter(!is.na(MonthlyMeanTemperature))

ggplot(monthly_weather, aes(x = date, y = MonthlyMeanTemperature))+
  geom_line() +
  geom_point() + 
  geom_line(aes(y = MonthlyMaximumTemperature), col = "red") +
  geom_point(aes(y = MonthlyMaximumTemperature), col = "red") +
  geom_line(aes(y = MonthlyMinimumTemperature), col = "blue") +
  geom_point(aes(y = MonthlyMinimumTemperature), col = "blue") 
  