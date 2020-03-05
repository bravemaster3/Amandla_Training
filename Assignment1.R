## Name: Caren Koli
## Assignment: 1
## Date: 29th February, 2020

## You are provided with the Financial Inclusion in Africa dataset, 
## which is hosted on the Zindi platform. https://zindi.africa/competitions/financial-inclusion-in-africa.
## Use the dataset to answer the questions below.

## NB: Please make your work neat by including spaces in the code e.g
## my_df <- df %>% 
##          filter(Age == 35)

##1. At the beginning of the training, we learnt how to tell a story from data.
## List down 5 or more questions that we can ask from the dataset.
##Establish if individuals in urban areas have more  access to cell phones than those in rural areas
##Establish if there is a relationship between type of location and possesion of a bank account.
##Establish of there are more males than females in rural areas.
##Establish if there is a relationship between level of educationa and marital status.
##Establish if there is a relationship between level of education and type of job.
##Establish if there is a relationship between gender and marital status.
##Establish if there is a relationship between family head and typeof job

##2. Generate a dataset that only contains observations from Kenya. We will 
## use this dataset to answer subsequent questions. Call the dataset Kenya_df.
library(dplyr)

my_df <- read.csv("C:/Users/admin/Downloads/Train.csv") ##reading in data from Excel
  Kenya_df <- my_df %>% 
    filter(country == "Kenya")

##3. Drop the "unique_id" and "year" variables from the dataset.
my_df1 <- Kenya_df %>% 
  select(-uniqueid,-year) ## drop uniqueid and year variables

##4. Generate a variable called "household_cat" that re-categorizes the "household
## size" variable into 5 groups. Group 1 (1-5), Group 2 (6-10), Group 3 (11-15),
## Group 4 (>15). 

my_df2 <- my_df1 %>% 
    mutate(household_cat = ifelse(household_size <=5, "Group 1", 
                                  ifelse(household_size>=6 & household_size<=10,"Group 2",
                                         ifelse(household_size<=11 & household_size>=15,"Group 3","Group 4" ))))

##5. Generate a variable called "age_cat" that categorizes the "age" variable into three buckets.
## Use limits of your own choice.
min(my_df2$age_of_respondent)
max(my_df2$age_of_respondent)
##age categories of choice: 16-35"Young adult"
##                          36-55 "Middle_aged"
##                          56-95 "Old"
my_df3 <- my_df2 %>% 
    mutate(age_cat = ifelse(age_of_respondent<= 35,"Young adult",ifelse(age_of_respondent>=36
                              & age_of_respondent<=55,"Middle aged","old")))

##6. Generate a variable called "relationship_with_head2", that combines the 
## "other relative" and "Other non-relatives" options of the "relationship_with_head"
## variable into a single option called "other".
my_df4 <- my_df3 %>% 
    mutate(relationship_with_head2 = relationship_with_head %in% grep("relative",
                                        relationship_with_head,ignore.case = T,value = T))
other <- my_df4 %>% 
  transmute(relationship_with_head2 = relationship_with_head %in% grep("relative",
                                           relationship_with_head,ignore.case = T,value = T))

##7. Generate a dataset that contains self employed ladies who have never been married,
## they have no cellphone access, but they have bank accounts. Call this dataset "empowered".
my_df5 <- my_df3 %>% 
    filter(gender_of_respondent == "Female",job_type == "Self employed",
           marital_status == "Single/Never Married",cellphone_access == "No",bank_account == "No")
