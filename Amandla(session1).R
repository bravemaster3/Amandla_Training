## Name:Caren
## Amandla session 1
## Select and mutate functions
df <- read.csv("C:/Users/admin/Desktop/wafanyikazi.csv")
library(tidyverse)
library(devtools)
colnames(df)

df1 <- df %>% 
  select(Gender,Age,Income) #select Gender,Age and Income 

df2 <- df %>%
  select(-Income,-Age) %>% #exclude income and age from the dataset then
  select(contains("e")) # select all variables with letter e

df3 <- df %>% 
  select(starts_with("a")) # select all variables starting with a

df4 <- df %>% 
  select(ends_with("e")) # select variables ending with e

#mutate: used to generate new variables from existing variables
df_propleavedays <- df %>% 
  mutate(prop_leavedays = (Leave_Days/365)*100) #calculate annual proportion of leave days
##generate new vaiable for age categories: <=5000 - underpaid
#                                          >5000 - paidwell
df_income_cat <- df %>% 
  mutate(income_cate = ifelse(Income <=5000, "underpaid", "paidwell")) 

min(df$Age)
max(df$Age)

#categorize age into: 18-25,young
#                     26-35,middle
#                     36-50,old
df_agecat <- df %>%
  mutate(age_cat = ifelse(Age <=25, "young", 
                   ifelse(Age >=26 & Age <=35, "middle","old")))

min(df$Leave_Days)
max(df$Leave_Days)

##categorize leave days into: 0-4, compensation
#                             5-9, compassionate
#                             10-14, Sick
#                             15-19, Paternal
#                             20-24, Annual

df_leave_category <- df %>% 
  mutate(leave_cat = ifelse(Leave_Days <=4,"compensation",
                        ifelse(Leave_Days >=5 & Leave_Days<=9,"compassionate",
                          ifelse(Leave_Days >=10 & Leave_Days<=14,"sick",
                            ifelse(Leave_Days>=15 & Leave_Days<=19,"paternal","Annual")))))

##generate a new variable called jinsia which rename male to mume and female to mke
df_rename <- df %>% 
  mutate(jinsia = ifelse(Gender == "Male", "mume", "mke"))