##Name: Caren Koli
##Amandla training: Visualization
## Line graph
library(tidyverse)
library(ggplot2)
df <- read.csv("C:\\Users\\admin\\Downloads\\wafanyikazi.csv")
##Generate summary statistics table
tab1 <- df %>% 
  group_by(Gender) %>% 
  summarize(count = n()) %>% 
  mutate(percentage = round(count/sum(count)*100,0))  
##Set theme in advance
mytheme <-  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.25,size = 14),
                axis.title = element_text(face = "italic", size = 12),
                axis.text = element_text(face = 12),
                axis.line = element_line(size = 1.5),
                plot.subtitle = element_text(hjust = 0.5,size = 12),
                plot.caption = element_text(face = "bold",size = 12),
                panel.background = element_rect(fill = NA))

##Generate bar graph
  
  bargraph1 <- ggplot(data = tab1,aes(x = Gender,y = percentage)) +
               geom_bar(stat = "identity",fill = "brown") +
               geom_text(aes(label = paste0(percentage,"%")),hjust = 0.5,vjust = -0.25)+
               mytheme+
               
               labs(title = "Gender distribution",
                    x = "Gender",
                    y = "percentage")
  bargraph1
  
 ##Generate summary statistcs for department
  
  tab2 <- df %>% 
    group_by(Department) %>% 
    summarise(count =n()) 
    
  bargraph2 <- ggplot(data = tab2,aes(x = reorder(Department,-count),y = count))+
               geom_bar(stat = "identity",fill = "blue")+
               geom_text(aes(label = count,hjust = 0.5, vjust = -0.25))+
               mytheme+
               labs(title = "Distribution of department",
                    x = "Department",
                    y = "count")
  bargraph2
##Summary statistics for marital status  
  tab3 <- df %>% 
    group_by(Marital_Status) %>% 
    summarise(count = n()) %>% 
    mutate(percentage = round(count/sum(count)*100,0))
  
  ##Bar graph for marital status
  bargraph3 <- ggplot(data = tab3,aes(x = Marital_Status, y = percentage)) +
               geom_bar(stat ="identity", fill = "red")+
               geom_text(aes(label = paste0(percentage,"%"), hjust = 0.5, vjust = -0.25))+
               mytheme+
               labs(title = "Distribution of Marital status",
                    x = "Marital status",
                    y = "percentage")
  bargraph3
  
  #plotting bargraph for more than one variable
  ## Summary statistics for department and role
  tab4 <- df %>% 
    group_by(Department,Role) %>% 
    summarise(count = n()) %>% 
    mutate(percentage = round(count/sum(count)*100,0))
  ##Bar graph for department and role
  
  bargraph4 <- ggplot(data = tab4, aes(x = Department, y = percentage, fill = Role))+
               geom_bar(stat = "identity",position = "dodge")+
               geom_text(aes(label = percentage),hjust = 0.5,vjust = 0.25,size =4,
                          position = position_dodge(width = 0.9))+
               mytheme+
  
               scale_fill_manual(values = c("pink","blue","maroon"))+
               labs(title = "Distribution\n of\n Department\n per role", 
                          x = "Department",y = "percentage")+
              facet_wrap(~Role, ncol = 3)
  bargraph4
  
  ##Bar graph to show age category vs gender
  ####Create age categories
  tab5 <- df %>% 
       mutate(age_cat = if_else(Age >=18 & Age <= 24,"Young",
                                if_else(Age >25 & Age <= 35, "middle_age","Old_aged"))) %>% 
       group_by(age_cat,Gender) %>% 
       summarize(count = n()) %>% 
       mutate(perc = round(count/sum(count)*100,0))
  ##Bar graph
   bargraph5 <- ggplot(data = tab5, aes(x = age_cat, y = perc, fill = Gender))+
                geom_bar(stat = "identity", position = "stack")+
                geom_text(aes(label = paste0(perc,"%"), hjust = 0.5, vjust = 0))+
                mytheme+
                scale_fill_manual(values = c("orange","grey"))+
     labs(title = "Distiution of age categories and gender",
             x = "Age category",y = "Percentage")
   bargraph5
 ##Generate bar graph for Marital status and gender distribution   
                     
  tab5 <- df %>% 
    group_by(Marital_Status,Gender) %>% 
    summarize(count = n()) %>% 
    mutate(percentage = round(count/sum(count)*100,0))
  
  bargraph6 <- ggplot(data = tab5, aes(x = Marital_Status, y = percentage, fill = Gender))+
              geom_bar(stat = "identity",position = "dodge")+
              geom_text(aes(label = percentage),hjust = 0.5,vjust = -0.25,
                        position = position_dodge(width = 1.0))+
    mytheme+
      scale_fill_manual(values = c("green","red"))+
    labs(title = "Distribution of Marital status per gender",
         x = "Marital status",
         y ="percentage")
  bargraph6

  

  
           
    
  
               
