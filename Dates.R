# Name: Caren Koli
# Date:17th Mar 2020
#Lubridate-Assignment
library(tidyverse)
library(lubridate)
visit_case <- read.csv("C:\\Users\\admin\\Downloads\\visit_caseStudy.csv",header = T,sep = ";")

visit <- visit_case %>% 
  mutate(date = ymd_hms(date))
str(visit$date)
 
#Generate year
 visit1 <- visit %>% 
   mutate(mwaka = year(date)) %>% 
 select(mwaka)
 #Generate month
 visit2 <- visit %>% 
   mutate(mwezi = month(date, label = T, abbr = T)) %>% 
    mutate(mwaka = year(date)) %>% 
    select(mwaka,mwezi)
 #Generate day
  visit3 <- visit %>% 
    mutate(mwaka = year(date)) %>% 
     mutate (mwezi =month(date,label = T,abbr = T)) %>% 
     mutate(tarehe = day(date)) %>% 
     select(mwaka,mwezi,tarehe)
  #Generate weekday
  visit4 <- visit %>% 
    mutate(mwaka = year(date)) %>% 
    mutate (mwezi =month(date,label = T,abbr = T)) %>% 
    mutate(tarehe = day(date)) %>%
    mutate(siku = wday(date,label = T,abbr = T)) %>% 
    select(mwaka,mwezi,tarehe,siku)
  #Generate hour
  visit5 <- visit %>% 
    mutate(mwaka = year(date)) %>% 
    mutate (mwezi =month(date,label = T,abbr = T)) %>% 
    mutate(tarehe = day(date)) %>%
    mutate(siku = wday(date,label = T,abbr = T)) %>% 
    mutate(saa = hour(date)) %>% 
    select(mwaka,mwezi,tarehe,siku,saa)
  #Genertae minute
  visit6 <- visit %>% 
    mutate(mwaka = year(date)) %>% 
    mutate (mwezi =month(date,label = T,abbr = T)) %>% 
    mutate(tarehe = day(date)) %>%
    mutate(siku = wday(date,label = T,abbr = T)) %>% 
    mutate(saa = hour(date)) %>%
    mutate(dakika = minute(date)) %>% 
    select(mwaka,mwezi,tarehe,siku,saa,dakika)
  #Generate second
  visit7 <- visit %>% 
    mutate(mwaka = year(date)) %>% 
    mutate (mwezi =month(date,label = T,abbr = T)) %>% 
    mutate(tarehe = day(date)) %>%
    mutate(siku = wday(date,label = T,abbr = T)) %>% 
    mutate(saa = hour(date)) %>%
    mutate(dakika = minute(date)) %>% 
    mutate (sekunde = second(date)) %>% 
    select(mwaka,mwezi,tarehe,siku,saa,dakika,sekunde)
  #Generate a datset that shows the number of visits per hour
  
  website_visit <-visit7 %>% 
    group_by(saa) %>% 
    summarize(count = n())
 
    
   
  # Generate a variable that contains categories of the hour variable
  ### 1:00 am - 6:00 am (Early Morning)
  ### 7:00 am - 11:00 am (Morning)
  ### 12:00 - 5:00 pm (Afternoon)
  ### 6:00 pm - 9:00 pm (Evening)
  ### 10:00 pm - Midnight (Late Night
  
  hour <- visit7 %>% 
   mutate(hour_cat = if_else(saa >=1 & saa <=6, "Early morning",
                    if_else(saa >=7 & saa <=11, "Morning",
                     if_else(saa >=12 & saa <= 17,"Afternoon",
                     if_else(saa >=18 & saa <=21,"Evening","Late Night")))))
  
  
  
 