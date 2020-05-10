remove(list = ls())
library("readxl") 
library("writexl")
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(maps)
getwd()

Loc <- read_excel("countries_Location.xlsx") 
Loc <- data.frame(Loc) #Changed the excel file into a data frame 

colnames(Loc) <- c("from","to", "latfrom","longfrom","latto","longto")

edges <- Loc[,c(1,2)]

nodes <- Loc[,c(1,3,4)]
nodes2 <- Loc[,c(2,5,6)]
colnames(nodes2) <- c("from", "latfrom","longfrom")
nodes <- bind_rows(nodes,nodes2)
nodes <- unique(nodes)
nodes$ID <- seq.int(nrow(nodes))
nodes <- nodes[,c(4,3,2,1)]
colnames(nodes) <- c("id", "lon","lat","name")

edges$fromid <- nodes$id[match(edges$from,nodes$name)]
edges$toid <- nodes$id[match(edges$to,nodes$name)]

edgesname <- edges
edges <- edges[,c(3,4)]
colnames(edges) <- c("from", "to")
edges$weight <- 1
edges <- edges %>% group_by(from, to) %>% 
  summarise(weight = sum(weight))

  
g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))

important_edges <- edges_for_plot %>% filter(weight > 5)

nodes$weight = degree(g)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

#Plot with only countries size based on degree
jpeg("DegreeCentrality.jpg", width = 1500)

ggplot(nodes) + country_shapes +
  geom_point(aes(x = as.numeric(lon), y = as.numeric(lat), size = weight),           # draw nodes
             shape = 21, fill = 'red',
             color = 'black', stroke = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(1, 8)) +    # scale for node size
  geom_text(aes(x = as.numeric(lon), y = as.numeric(lat), label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 1.5, color = "black", fontface = "bold") +
  mapcoords + maptheme

dev.off()

#Plot with most important connections (>5)
jpeg("MainConnections.jpg", width = 1500)

ggplot(nodes) + country_shapes +
  geom_curve(aes(x = as.numeric(x), y = as.numeric(y), xend = as.numeric(xend), yend = as.numeric(yend),
                 size = weight),     # draw edges as arcs
             data = important_edges, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = as.numeric(lon), y = as.numeric(lat)),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5) +
  mapcoords + maptheme

dev.off()
