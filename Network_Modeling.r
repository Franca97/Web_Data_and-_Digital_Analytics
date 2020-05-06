remove(list = ls())
install.packages("readxl") #Installed this package to read in excel
install.packages("janitor") #Installed this package to clean NULL values
install.packages("writexl") #Installed this package to export results to Excel
library("janitor") 
library("readxl") 
library("writexl")
library("igraph")
getwd() #Make sure that you have the file you need in the Working directory or you apply a proper pathway 

D1 <- read_excel("ProjectDoc.xls") 
D1 <- data.frame(D1) #Changed the excel file into a data frame 
Routes1 <- data.frame(D1$source.airport,D1$destination.apirport) #This is all the airport connections
Routes2 <- data.frame(D1$source.country,D1$destination.country) #This is all the country connections with domestic flights being changed to values of NULL 

DM1 <- as.matrix(Routes1) #Changed from data frame to matrix to work with igraph
DM2 <- as.matrix(Routes2)
DM2 <- remove_empty(DM2, which = c("rows", "cols"), quiet = TRUE)

#Network visualization in Gephi
#CSV files to directly use in Gephi
write.csv(DM1,file="airports.csv",row.names=F) 
write.csv(DM2,file="countries.csv",row.names=F)
write_xlsx(as.data.frame(DM2),"countries.xlsx",col_names=T)

n1 <- graph.edgelist(DM1,directed = TRUE) 
n2 <- graph.edgelist(DM2,directed = TRUE) 

#Graph Density
density <- graph.density(n2)
density
apl <- average.path.length(n2)
apl

#Degree Centrality
ind <- degree(n2, mode="in")
ind <- sort(ind, decreasing=T)
ind <- as.data.frame(ind)
countryind <- row.names(ind)

outd <- degree(n2, mode="out")
outd <- sort(outd, decreasing=T)
outd <- as.data.frame(outd)
countryoutd <- row.names(outd)

#Betweenness Centrality
betw <- betweenness(n2, directed=T, weights=NA)
betw <- sort(betw, decreasing=T)
betw <- as.data.frame(betw)
countrybetw <- row.names(betw)

#Hubs
hubs <- hub_score(n2, weights=NA)$vector
hubs <- sort(hubs, decreasing=T)
hubs <- as.data.frame(hubs)
countryhubs <- row.names(hubs)

#Export data to Excel to make comparisons
table <- cbind(countryind,ind,countryoutd,outd,countrybetw,betw,countryhubs,hubs)
colnames(table) <- c("Highest In-degree","ind","Highest Out-degree","outd", "Highest betweenness","betw", "Highest Hub","hub")
table <- as.data.frame(table)
write_xlsx(table,"countrylist.xlsx",col_names=T)
