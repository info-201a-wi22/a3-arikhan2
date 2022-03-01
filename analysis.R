#using libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(patchwork)

#step 1 - downloading and analyzing the data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#step 2 - filtering down to values of interest; general interest = incarceration by race 
l_incarcerated <- mean(incarceration$latinx_jail_pop, na.rm = TRUE)
b_incarcerated <- mean(incarceration$black_jail_pop, na.rm = TRUE)
w_incarcerated <- mean(incarceration$white_jail_pop, na.rm = TRUE)

highest_state_jail <- incarceration %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>% 
  pull(state)

highest_state_black <- incarceration %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(state)

highest_state_latinx <- incarceration %>% 
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull(state)


#tackling the first chart - trends over time; I'm investigating the total incarceration of latinx over time
#first - narrowing down values to create chart
latinx_over_time <- incarceration[, c("year", "latinx_jail_pop")] %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarize(avg = mean(latinx_jail_pop)) %>% 
  filter(avg > 0)

#dataset for white incarceration (for comparison purposes)
white_over_time <- incarceration[, c("year", "white_jail_pop")] %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarize(avg = mean(white_jail_pop)) %>% 
  filter(avg > 0)

#creating bar plot - latinx jail trends over time
latinx_trends <- ggplot(data = latinx_over_time) +
  geom_line(
    mapping = aes(x = year, y = avg, color = avg)
  ) +
  labs(
    title = "Average Latinx Jail Population Over Time",
    x = "Year",
    y = "Avg. Latinx Population",
    color = "Key"
  )

#creating bar plot - white jail trends over time
white_trends <- ggplot(data = white_over_time) +
  geom_line(
    mapping = aes(x = year, y = avg, color = avg)
  ) +
  labs(
    title = "Average White Jail Population Over Time",
    x = "Year",
    y = "Avg. White Population",
    color = "Key"
  )

#creating dataset for bar plot
two_variables <- incarceration[, c("year", "black_jail_pop", "latinx_jail_pop", "white_jail_pop")] %>%   
  na.omit() %>% 
  group_by(year) %>% 
  summarize(avg_b = mean(black_jail_pop), avg_l = mean(latinx_jail_pop), avg_w = mean(white_jail_pop)) %>% 
  filter(avg_b > 0, avg_l > 0, avg_w >0) %>% 
  filter(year > 2008) 
  two_variables$col3 <- two_variables$avg_b + two_variables$avg_l


#creating a pie chart - comparing white incarceration to 'underrepresented'
#underrepresented = black and latinx population
slices <- c(112.312, 138.6206)
lbls <- c("White", "Underrepresented")

piepercent<- (round(100*slices/sum(slices), 1))

pie_chart<- pie(slices, 
                labels = piepercent, 
                radius = .7, 
                main = "Average Incarceration in 2010; White vs. Underrepresented",
                col = rainbow(length(slices)))
legend("topleft", c("White","Underrepresented"), cex = 0.8,
       fill = rainbow(length(slices)))



#dataset for the map
#going to average black and latinx data - will represent the underrepresented
library("stringr")
l_underrepresented <- incarceration[c("year", "fips", "county_name", "latinx_jail_pop")] %>% 
  filter(year == 2010) %>%
  filter(latinx_jail_pop != 0.00) %>% 
  na.omit(incarceration)


#creating the map! - problem = 'region' does not align
library(maps)
library(tidyr)
county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")
  
county_shapes <- left_join(county_shapes, l_underrepresented, by = "fips") %>% 
  na.omit()



#creating basic map
map1 <- ggplot(county_shapes, aes( x = long, y = lat, group = group)) +
  geom_polygon(
    mapping = aes(x = long, y = lat, fill = latinx_jail_pop), 
               color = "white",
               size = .1
               ) +
  labs(
    fill = "Key", x = "Longitude", y = "Latitude",
  ) +
ggtitle("Latinx Incarceration Across America")
