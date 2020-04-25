remove(list = ls())
install.packages("readxl") #Instaled this package to read in excel
install.packages("janitor") #Instaled this package to clean NULL values 
library("janitor") 
library("readxl") 
library("igraph")
getwd(). #Make sure that you have the file you need in the Working directory or you apply a proper pathway 

D1 <- read_excel("ProjectDoc.xls") 
D1 <- data.frame(D1) #Changed the excel file into a data frame 
Routes1 <- data.frame(D1$source.airport,D1$destination.apirport) #This is all the airport conections
Routes2 <- data.frame(D1$source.country,D1$destination.country) #This is all the country conections with domestic flights being changed to values of NULL 

DM1 <-as.matrix(Routes1) #Changed from data frame to matrix to work with igraph
DM2 <-as.matrix(Routes2)

NewDM2<-remove_empty(DM2, which = c("rows", "cols"), quiet = TRUE)
n1 <- graph.edgelist(DM1,directed = TRUE) 
n2 <- graph.edgelist(NewDM2,directed = TRUE) 
plot(n2)
