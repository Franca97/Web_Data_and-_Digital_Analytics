remove(list = ls())
library("readxl") 
library("writexl")
getwd()

Loc <- read_excel("countries_Location.xlsx") 
Loc <- data.frame(Loc) #Changed the excel file into a data frame 

write.csv(Loc,file="location.csv",row.names=F)
