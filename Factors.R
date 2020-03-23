##Amandla 
##Session 5(Zoom): Factors
## Factoring is attaching value labels to string data
##There are 5 ways of factoring: One by one
#                                For loop
#                                lapply
#                                forcats::fct_relevel()
#                                purr()
library(tidyverse)
##Method 1 :One by one. This factors one variale at a time
#factor variable gender
df10 <- data %>% 
  mutate(Gender1 = factor(Gender,
         levels = c("Male","Female"),
        labels = c("Male","Female")))
levels(df10$Gender1) ##cheking for factors
## Factor variable Role
df11 <- data %>% 
  mutate(Role1 = factor(Role,levels = c("Mid","Junior","Senior"),
                             labels = c("Mid","Junior","Senior")))
levels(df11$Role1)##check for factors

##Method 2: lapply
##data1 <- data %>% 
 ## select(Gender,Role,Department,Marital_Status)
#data2 <- data1 
#  data1 <- data.frame(data1) %>% 
 #   as.data.frame(lapply(data1,function(x) factor(x),levels=c....)check.names=FALSE)

set.seed(2020)

vec <- 1:5
Questions <- paste0("Opinion_",vec)
opinion_df <- data.frame(matrix("", ncol=5, nrow=200))
names(opinion_df) <- Questions

for(i in 1: length(Questions)){
  opinion_df[,Questions[i]] <- sample(c("Strongly Agree", "Agree","Neutral","Disagree", "Strongly Disagree"),size = 200,replace = TRUE) 
}
##Factor opinion_1
opinion_df1 <- opinion_df %>% 
  mutate(opinion_11_1 = factor(Opinion_1,
                               levels = c("Strongly Agree","Agree","Neutral","Strongly Disagree", "Disagree"),
                               labels = c("Strongly Agree","Agree","Neutral","Strongly Disagree", "Disagree")))
         
levels(opinion_df1$opinion_11_1) #checking for levels
 ##Mtd 2: Using lapply

opinion_levels <- as.data.frame(lapply(opinion_df, function(x) factor(x,
                                       levels = c("Strongly Agree", "Agree","Neutral","Strongly Disagree", "Disagree"),
                                       labels = c("Strongly Agree", "Agree","Neutral","Strongly Disagree", "Diagree"))),check.names = FALSE)
##Mtd 3:forcats
library(dplyr)
library(forcats)

opinion_df6 <- opinion_df %>% 
  mutate_all(fct_relevel, "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

opinion_df5 <- opinion_df %>%
  dplyr::mutate_all(forcats::fct_relevel, "Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")