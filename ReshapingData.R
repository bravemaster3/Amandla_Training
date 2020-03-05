## reshaping data
library(tidyverse)
year <- c(2000:2007)
jan <- c(500:507)
feb <- c(600:607)
mar <- c(700:707)
apr <- c(800:807)
may <- c(900:907)
june <- c(1000:1007)
##reshaping from wide to long
##method 1 :gather()
 tab1 <- data.frame(year,jan,feb,mar,apr,may,june)
 tab_gather <- tab1 %>% 
   gather(months,visits,-year)
  
 ##method2:pivot_longer()
 ?gather
 tab2 <- tab1 %>% 
   pivot_longer(c(jan,feb,mar,apr,may,june),names_to = "months",values_to = "visits")
 ##reshaping from long to wide
 ## method 1:spread()
 tab_spread <- tab_gather %>% 
   spread(months,visits)
 ##method 2:pivot_longer
 tab_spread2 <- tab_gather %>% 
  
   pivot_wider()##
   
 ##Example2
 year <- c(2010:2014)
 Q1 <- c(1003,1532,954,841,823)
 Q2 <- c(1359,933,992,1434,1034)
 Q3 <- c(1326,904,845,1480,1184)
 Q4 <- c(1122,1479,889,1174,1317)
 ##calculate total sums per year
 Sales <- data.frame(year,Q1,Q2,Q3,Q4)
 
##reshape to long
 ##group by 
 ##summarize
sales_gather2 <- Sales %>% 
  gather(Quarters,months,-year) %>% 
  group_by(year) %>% 
  summarize(sum_years = sum(months))
##calculate the means for quarters
##use the reshaped(long)dataset
##group by QUarters
##summarize
    Avq_Q <- sales_gather %>% 
      group_by(Quarters) %>% 
      summarise(Avq_Quarters = mean(months))

 
