##Name:Caren
##Amandla:Session 2
## Filter,grep
library(tidyverse)
##Filter: picks variables of interest
df <- read.csv("C:/Users/admin/Desktop/wafanyikazi.csv") #read in dataset from Excel

unique(as.character(df$Gender)) #displays contents of the variable

df1 <- df %>% 
  filter(Gender == "Female") #display individuals who are female

df2 <- df %>% 
  filter(Age <50) #display individuals whose age is less than 50

df3 <- df %>% 
  filter(Age <50 & Gender == "Female") # display female individuals who are less than 50 years

df4 <- df %>% 
  filter(Age <50 & Gender == "Female" || Role == "Mid")

#Extract dataset for income>5000,in senior roles and in finance department
df5 <- df %>% 
  filter(Income >5000 & Role == "Senior" & Department == "Finance")

##grep function:Used for checking pattern
#grep("e", X, ignore.case = T, vale = T) e is the patter
#                                        X is the source data

counties <- c("kakamega","kisumu","kericho","muranga","kilifi","meru") #input counties
counties
#extract counties starting with k (^k)
grep("^k", counties, ignore.case = T, value = T)
#extract counties containing u (u)
grep("u", counties, ignore.case = T, value = T)
#extract counties ending with a (a$)
grep("a$", counties, ignore.case = T, value = T)
#create a variable called county which contains names of counties ending with a and state if it is okay or not
df6 <- df %>% 
  mutate(countyn = ifelse(County %in% grep("a$",County, ignore.case = T, value = T),
                                      "okay", "not okay"))
##generate a dataset containing counties which end with letter a
df7 <- df %>% 
  filter(County%in% grep("a$", County, ignore.case = T, value = T))

## generate a datset that contains individuals who are analysts

df_analysts <- df %>% 
  filter(Department %in% grep("analyst",Department, ignore.case = T, value = T))
##transmute():generates a new variable in a new dataset.The rest of the variables are lost
dbl_income <- df %>% 
transmute(income_dbl = Income*2)#generates a var that doubles income and loses other variables
##arange() :by default it arranges data in ascending order
#           For descending order use desc()
df8 <- df %>% 
  arrange(Age) %>% 
  arrange(desc(Age))

##arranging a dataset using 2 variables
df9 <- df %>% 
  arrange(Age,desc(County))
