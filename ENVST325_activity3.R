# start with installing packages and loading packages/data
install.packages(c('dplyr', 'lubridate', 'ggplot2')) # then add it as a comment so that it doesn't run every time
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
  labs(x = 'Year', y = expression('CO'[2]))

#optional if time prompt: how would you add subscripts for CO2 in your plot axes label?
#you use the expression() function, supercripts are denoted with a carrot (^) and subscripts are denoted with brackets [ ]
#we added subscripts to the y-axis label on our plot for the second question :)


### Homework 3
#Question 1: Make a graph that communicates about emissions from any countries of your choice. Explain how you considered principles of visualization in making your graph.
#I want to make a graph displaying annual emissions from countries w/ highest cumulative CO2 emissions (according to CarbonBrief)
#first subset the data, limit to years after 1914 (due to post-wartime industrialization)
top_co2 <- datco2[datco2$Year >= 1914 & datco2$Entity == "United States" |
                    datco2$Year >= 1914 & datco2$Entity == "China" |
                    datco2$Year >= 1914 & datco2$Entity == "Russia" |
                    datco2$Year >= 1914 & datco2$Entity == "Brazil" |
                    datco2$Year >= 1914 & datco2$Entity == "Indonesia" |
                    datco2$Year >= 1914 & datco2$Entity == "Germany" |
                    datco2$Year >= 1914 & datco2$Entity == "India" |
                    datco2$Year >= 1914 & datco2$Entity == "United Kingdom" |
                    datco2$Year >= 1914 & datco2$Entity == "Japan" |
                    datco2$Year >= 1914 & datco2$Entity == "Canada" , ]

#make the plot 
ggplot(data = top_co2, 
       aes(x = Entity, y = CO2)) + 
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90") + 
  labs(x = "Country", y="Annual emissions (tons CO2)")

#since the plot doesn't seem very useful with China and US lengthenning the y-axis, 
#make a plot without China and United States
top_co2_without <- datco2[datco2$Year >= 1914 & datco2$Entity == "Russia" |
                            datco2$Year >= 1914 & datco2$Entity == "Brazil" |
                            datco2$Year >= 1914 & datco2$Entity == "Indonesia" |
                            datco2$Year >= 1914 & datco2$Entity == "Germany" |
                            datco2$Year >= 1914 & datco2$Entity == "India" |
                            datco2$Year >= 1914 & datco2$Entity == "United Kingdom" |
                            datco2$Year >= 1914 & datco2$Entity == "Japan" |
                            datco2$Year >= 1914 & datco2$Entity == "Canada" , ]

ggplot(data = top_co2_without, 
       aes(x = Entity, y = CO2)) + 
  geom_violin(fill=rgb(0.85,0.85,0.98))+ # add a violin plot with purple color
  geom_boxplot(width=0.03,size=0.15, fill="grey90") + 
  labs(x = "Country", y="Annual emissions (tons CO2)") + 
  ggtitle("Annual emissions by top polluting countries (excluding US and China)") 
#looks a lot better, can see trends by country more clearly 

#Question 2: Plot world CO2 emissions on one graph and world air temperature anomalies on the other graph.
#first need to subset both data sets to only look at world emissions and world temperature anomalies, respectively
world_emit <- datco2 %>%
  filter(Entity == "World")

world_temp <- dat_cc %>%
  filter(Entity == "World")

#now I can plot each on its own graph
ggplot(world_emit, 
       aes(x = Year, y = CO2)) + 
  geom_area(fill = 'grey40') + 
  labs(x = "Year", y = expression("Emissions (CO"[2]*")")) + 
  ggtitle("Annual worldwide emissions (1750-2020)") + 
  theme_light()
  

ggplot(world_temp, 
       aes(x = DayF, y = temperature_anomaly)) +
  geom_line() + 
  geom_hline(yintercept = 0, color = "purple3") + 
  labs(x = "Monthly Observations", y = "Temperature Anomaly (degrees)") + 
  ggtitle("World temperature anomalies (1880-2021)") + 
  theme_light()

#Question 3: Remake graph from data downloaded from Our World in Data
#read in data
milk <- read.csv("/cloud/project/activity03/environmental-footprint-milks.csv")

names(milk)

#make 4 bar charts to show the environmental footprints of each milk type 
land_use <- ggplot(milk, 
                   aes(x = reorder(Entity, Land.use.of.milks..m2.),
                       y = Land.use.of.milks..m2., 
                       fill = Entity)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Dairy milk" = "steelblue", 
                               "Oat milk" = "lightgoldenrod2", 
                              "Soy milk" = "mediumpurple2", 
                              'Almond milk' = "indianred3",
                              'Rice milk' = "palegreen3")) + 
  labs(x = "Type of milk", y = expression("Land use (m"^2*" per L milk)")) + 
  ggtitle("Environmental impact of milks on land use") + 
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
land_use

water_use <- ggplot(milk, 
                    aes(x = reorder(Entity, Water.use.of.milks..L.), 
                        y = Water.use.of.milks..L., fill = Entity)) + 
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("Dairy milk" = "steelblue", 
                               "Oat milk" = "lightgoldenrod2", 
                               "Soy milk" = "mediumpurple2", 
                               'Almond milk' = "indianred3",
                               'Rice milk' = "palegreen3")) +
  labs(x = "Type of milk", y = "Freshwater use (L water per L milk)") + 
  ggtitle("Environmental impact of milks on freshwater use") + 
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
water_use

GHG_emissions <- ggplot(milk, 
                        aes(x = reorder(Entity, GHG.emissions.of.milks..kg.CO2eq.), 
                            y = GHG.emissions.of.milks..kg.CO2eq., fill = Entity)) + 
  geom_bar(stat = "identity") + 
scale_fill_manual(values = c("Dairy milk" = "steelblue", 
                               "Oat milk" = "lightgoldenrod2", 
                               "Soy milk" = "mediumpurple2", 
                               'Almond milk' = "indianred3",
                               'Rice milk' = "palegreen3")) +
  labs(x = "Type of milk", y = "Greenhouse gas emissions (kg per L of milk)") + 
  ggtitle("Environmental impact of milks on GHG emissions") + 
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
GHG_emissions

eutrophic <- ggplot(milk, 
                    aes(x = reorder(Entity, Eutrophication.from.milks..g.PO43.eq.), 
                        y = Eutrophication.from.milks..g.PO43.eq., fill = Entity)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Dairy milk" = "steelblue", 
                               "Oat milk" = "lightgoldenrod2", 
                               "Soy milk" = "mediumpurple2", 
                               'Almond milk' = "indianred3",
                               'Rice milk' = "palegreen3")) +
  labs(x = "Type of milk", y = "Eutrophication (g per L milk)") + 
  ggtitle("Environmental impact of milks on eutrophication") + 
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")
eutrophic
