library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)
library(knitr)
knitr::opts_knit$set(root.dir='../')

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways

####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)

#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

full_wide <- spread(full_long, key = "data", value = "value") %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>% 
  mutate(month=month(DateTime),
         year=year(DateTime))

ggplot(full_wide,aes(x=ndvi,y=ndmi,color=site)) + 
  geom_point() +
  scale_color_few() +
  theme_few() + 
  theme(legend.position=c(0.2,0.4))

summer_only <- filter(full_wide, month %in% c(6,7,8,9))

ggplot(summer_only,aes(x=ndvi,y=ndmi,color=site)) + 
  geom_point() +
  scale_color_few() +
  theme_few() + 
  theme(legend.position=c(0.8,0.8))

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 

winter_NDSI <- full_wide %>%
  filter(month %in% c(1,2,3,4)) %>%
  group_by(site,year) %>%
  summarize(mean_ndsi = mean(ndsi))

summer_NDVI <- full_wide %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(site,year) %>%
  summarize(mean_ndvi = mean(ndvi))

q2_data <- winter_NDSI %>%
  right_join(summer_NDVI, by=c("site","year"))

ggplot(q2_data,aes(x=mean_ndsi,y=mean_ndvi, color=site)) + 
  geom_point() +
  scale_color_few() +
  theme_few()
  
## End code for question 2 -----------------

###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

#scatterplot of mean  ndvi vs. ndsi, separated by pre-/post- burn and site:

comparison <- q2_data %>%
  mutate(period = cut(year,breaks=c(0,2003,2020),
                      labels=c('pre-burn','post-burn'))) %>%
  group_by(site,year,period)

ggplot(comparison,aes(x=mean_ndsi,y=mean_ndvi, color=period)) + 
  geom_point() + 
  theme_few() + 
  scale_color_few() + 
  facet_wrap(~site)

## End code for question 3 -----------------

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

#Average monthly ndvi regardless of site or pre-/post- burn:

avg_ndvi <- full_wide %>%
  group_by(month) %>%
  summarize(mean_ndvi = mean(ndvi))

ggplot(avg_ndvi,aes(x=month, y=mean_ndvi)) +
  geom_point() + 
  geom_line() +
  theme_few()

#Average monthly ndvi separated by site and pre-/post- burn:

monthly_NDVI <- full_wide %>%
  mutate(period = cut(year,breaks=c(0,2003,2020),
                      labels=c('pre-burn','post-burn'))) %>%
  group_by(site,period,month) %>%
  summarize(mean_ndvi = mean(ndvi))

ggplot(monthly_NDVI,aes(x=month,y=mean_ndvi,color=period)) +
  geom_point() + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  facet_wrap(~site)

## End code for question 4 -----------------

##### Question 5 ####
#What month is the snowiest on average?

#Average monthly ndsi regardless of location or pre-/post- burn:

avg_ndsi <- full_wide %>%
  group_by(month) %>%
  summarize(mean_ndsi = mean(ndsi))

ggplot(avg_ndsi,aes(x=month, y=mean_ndsi)) +
  geom_point() + 
  geom_line() +
  theme_few()

# Average monthly ndsi separated by location and pre-/post- burn:

monthly_NDSI <- full_wide %>%
  mutate(period = cut(year,breaks=c(0,2003,2020),
                      labels=c('pre-burn','post-burn'))) %>%
  group_by(site,period,month) %>%
  summarize(mean_ndsi = mean(ndsi))

ggplot(monthly_NDSI,aes(x=month,y=mean_ndsi,color=period)) +
  geom_point() + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  facet_wrap(~site)
## End code for question 5 -----------------
