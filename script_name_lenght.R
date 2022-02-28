# libraries 
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

#Part I: Exploratory Analysis

# import the dataset 
df_name <- read_csv("arabic_first_last_name.csv")

# a quick check on the dataset
summary(df_name)
sum(is.na(df_name))
# create two columns for the lengths of the last and first names
df_name_new <- df_name %>%
  mutate(first_lenght = nchar(df_name$`First Name`))%>%
  mutate(last_length = nchar(df_name$`Last Name`)) 
  
# a dot plot for the last name lengths
ggplot(df_name_new, aes(x=last_length, y=last_length)) + 
  geom_dotplot(binaxis='y', stackdir='center', fill= 'darkgreen',binwidth=0.3)+
  geom_rug()

# a dot plot for the first name lengths
ggplot(df_name_new, aes(x=first_lenght, y=first_lenght)) + 
  geom_dotplot(binaxis='y', stackdir='center', fill= 'darkgreen',binwidth=0.15)

#Part II: Regression Model

# scatter plot for last and first names length
ggscatter(df_name_new, x = "first_lenght", y = "last_length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", color ='darkgreen') 

# not part of the assignment activity
hist(df_name_new$first_lenght)
hist(df_name_new$last_length)
sum(duplicated(df_name_new))