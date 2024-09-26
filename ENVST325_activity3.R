# start with installing packages and loading packages/data
install.packages(c('dplyr', 'lubridate', 'ggplot2')) # now adding as a comment so that it doesn't run every time
library(dplyr)
library(lubridate)
library(ggplot2)

datco2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

#look at column names in df (prints to console)
colnames(datco2)
#change last column name to something shorter
colnames(datco2)[4] <- 'CO2'
colnames(datco2)

#making plots using base r: can make plots quickly, but hard to add a lot of things to it (colors and sizes based on groups)
plot(datco2$Year, datco2$CO2, 
     xlab = 'Year', ylab = 'CO2 emissions (tons)')

#making plots using ggplot2 <333
#make a quick lil df to filter US y Mexico and Canada eh
UMC_co2 <- datco2 %>%
  filter(Entity == 'United States' |
           Entity == 'Mexico' | Entity == 'Canada')
  
  
ggplot(data = UMC_co2, 
        aes(x = Year, y = CO2, color = Entity)) + 
  geom_line() + 
  labs(x = "Year", y = "CO2 emissions (tons)") +
  theme_classic()




#in-class activity 
#read in climate change data
dat_cc <- read.csv("/cloud/project/activity03/climate-change.csv")

#prompt 1: Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2.
#base R, N Hemisphere
north_cc <- dat_cc %>%
  filter(Entity == 'Northern Hemisphere')

#parse date 
north_cc$DayF <- ymd(north_cc$Day)

plot(north_cc$DayF, north_cc$temperature_anomaly, ylim = c(-1.6, 2), 
     xlab = 'Day', ylab = "Temperature Anomaly")

#base R, S Hemisphere
south_cc <- dat_cc %>%
  filter(Entity == 'Southern Hemisphere')

#parse date 
south_cc$DayF <- ymd(south_cc$Day)

plot(south_cc$DayF, south_cc$temperature_anomaly, ylim = c(-1.6,2), 
     xlab = 'Day', ylab = 'Temperature Anomaly')


#make a plot of both in ggplot
#parse date 
dat_cc$DayF <- ymd(dat_cc$Day)

#new df with only N and S hemispheres
hemisphere_cc <- dat_cc %>%
  filter(Entity == 'Northern Hemisphere' |
           Entity == 'Southern Hemisphere')

ggplot(hemisphere_cc, 
       aes(x = Day, y = temperature_anomaly, color = Entity)) + 
  geom_point() + 
  labs(x = 'Day', y = 'Temperature Anomaly')

#prompt 2: Plot the total all time emissions for the United States, Mexico, and Canada.
ggplot(UMC_co2, 
       aes(x = Year, y = CO2, fill = Entity)) + 
  geom_area(alpha = 0.5) + 
  labs(x = 'Year', y = expression(CO[2]))

#optional if time prompt: how would you add subscripts for CO2 in your plot axes label?
#you use the expression() function, supercripts are denoted with a carrot (^) and subscripts are denoted with brackets [ ]
#we added subscripts to the y-axis label on our plot for the second question :)


### Homework 3
#Question 1: Make a graph that communicates about emissions from any countries of your choice. Explain how you considered principles of visualization in making your graph.



