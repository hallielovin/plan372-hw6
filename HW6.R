#clear the environment 
rm(list=ls(all=TRUE))

#load in the data 
data <- read.csv("TS3_Raw_tree_data.csv")

#load in tidyverse
library(tidyverse)

#load in dplyr
library(dplyr)

#QUESTION 1
#Rename the City column to be called location 
data <- data %>% rename("Location" = "City")

#Make a new column for city and a new column for state using the City column in the data
data$City <- sub(",.*", "", data$Location)
data$State <- sub(".*,(.*)", "\\1", data$Location)

#Determine how many records are available for each state 
state_records <- data %>% count(State)

#Make a bar plot of the records in each state 
ggplot(state_records, aes(x=State, y=n)) + geom_col() + labs(title = "Number of Records in Each State", y= "Number of Records")

#QUESTION 2
#See what the unique state names are in the data set 
unique(data$State)

#Change the names of the data points so they do not include a space before the state abbreviation starts 
data$State <- recode(data$State, " CA" = "CA", " AZ" = "AZ", " CO" = "CO", " MN" = "MN"," IN" = "IN", " NY" = "NY", " ID" = "ID", " NM" = "NM", " HI" = "HI", " SC" = "SC", " NC" = "NC", " FL" = "FL", " WA" = "WA")
  
#Filter the data to only include observations from North and South Carolina 
nc_sc <- subset(data, State == "NC" | State == "SC")

#Find the cities that data was collected from in NC and SC 
unique(nc_sc$City)
## The only cities that they collected data from in North and South Carolina is Charleston and Charlotte

#QUESTION 3 
#Write a regular expression to extract the genus from the scientific name column
nc_sc$genus <- sub("^([A-Z][a-z]+)\\s.*", "\\1", nc_sc$ScientificName)

#Sort the data so it returns the ascending order of the data based on crown diameter 
nc_sc <- nc_sc %>% arrange(desc(AvgCdia..m.))

## The platanus has the largest crown size in diameter in all of North and South Carolina. It is the largest in South Carolina, but the largest in North Carolina is the Quercus.

#EXTRA CREDIT: TREE AGE 
#Group the data by the different genus and then summarize to find the average age of each one 
genus_age <- nc_sc %>% group_by(genus) %>% summarize(avg_age = mean(Age)) %>% arrange(desc(avg_age))

#The avg ages do range, but the largest crown diameter is not the oldest average ages of trees

#use a linear regression to see if age and size are correlated 
fit1 <- lm(AvgCdia..m. ~ Age, data = nc_sc)

summary(fit1)
