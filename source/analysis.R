#using libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(patchwork)

#step 1 - downloading and analyzing the data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration)

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
  filter(avg > 0) %>% 
  filter(year > 2008)
View(latinx_over_time)

#dataset for white incarceration (for comparison purposes)
white_over_time <- incarceration[, c("year", "white_jail_pop")] %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarize(avg = mean(white_jail_pop)) %>% 
  filter(avg > 0) %>% 
  filter(year > 2008)
View(white_over_time)

#creating bar plot - latinx jail trends over time
latinx_trends <- ggplot(data = latinx_over_time) +
  geom_line(
    mapping = aes(x = year, y = avg, color = avg)
  ) +
  labs(
    title = "Average Latinx Jail Population Over Time",
    x = "Year",
    y = "Avg. Latinx Population"
  )

#creating bar plot - white jail trends over time
white_trends <- ggplot(data = white_over_time) +
  geom_line(
    mapping = aes(x = year, y = avg, color = avg)
  ) +
  labs(
    title = "Average White Jail Population Over Time",
    x = "Year",
    y = "Avg. White Population"
  )

latinx_trends + white_trends

#creating dataset for bar plot
two_variables <- incarceration[, c("year", "black_jail_pop", "latinx_jail_pop", "white_jail_pop")] %>%   
  na.omit() %>% 
  group_by(year) %>% 
  summarize(avg_b = mean(black_jail_pop), avg_l = mean(latinx_jail_pop), avg_w = mean(white_jail_pop)) %>% 
  filter(avg_b > 0, avg_l > 0, avg_w >0) %>% 
  filter(year == 2010) 
  two_variables$col3 <- two_variables$avg_b + two_variables$avg_l
two_variables <- two_variables %>% 
  subset(select = -avg_b) %>% 
  subset(select = -avg_l)

View(two_variables)


#creating a pie chart - comparing white incarceration to 'underrepresented'
#underrepresented = black and latinx population
library(plotrix)

slices <- c(112.312, 138.6206)
lbls <- c("White", "Underrepresented")

piepercent<- (round(100*slices/sum(slices), 1))

pie(slices, 
    labels = piepercent, 
    radius = .7, 
    main = "Average Incarceration in 2010; White vs. Underrepresented",
    col = rainbow(length(slices)))
legend("topleft", c("White","Underrepresented"), cex = 0.8,
       fill = rainbow(length(slices)))


#dataset for the map
#going to average black and latinx data - will represent the underrepresented
library("stringr")
l_underrepresented <- incarceration[c("year", "fips", "county_name", "latinx_jail_pop", "black_jail_pop")] %>% 
  filter(year == 2008) %>%
  filter(latinx_jail_pop != 0.00) %>% 
  filter(black_jail_pop != 0.00) %>% 
  na.omit(incarceration)
View(l_underrepresented)

l_underrepresented$col3 <- l_underrepresented$latinx_jail_pop + l_underrepresented$black_jail_pop
View(l_underrepresented)
#creating the map! - problem = 'region' does not align
library(maps)
library(tidyr)
county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")
  
county_shapes <- left_join(county_shapes, l_underrepresented, by = "fips") 
  na.omit()
View(county_shapes)


#creating map - incarceration of latinx and black
inc_map <- ggplot(county_shapes) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.2
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(county_shapes$black_jail_pop)), na.value = "white", low = "yellow", high = "red") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  ggtitle("Underrepresented Incarceration Trends in the US")

inc_map

#creating basic map
map1 <- ggplot(county_shapes, aes( x = long, y = lat, group = group)) +
  geom_polygon(
    mapping = aes(x = long, y = lat, fill = black_jail_pop), 
               color = "white",
               size = .1
               ) 
map1

#adding cool gradient to basic map - more advanced map
map2 <- map1 + scale_fill_gradient(name = "Latinx incarceration trends in the US",
                                   low = "dark blue", 
                                   high = "light yellow", 
                                   na.value = "light grey") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  coord_map()

map2
