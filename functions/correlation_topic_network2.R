#Function: Correlation-Topic-network 

#Basic source: STM 
#Optimize by Carsten Schwemmer: https://github.com/cschwem2er/stminsights
#Generate a correlation network: 
#nodes: Topics
#edges: the probability that two topics in the same document 

library(stminsights)
library(shiny)
library(shinydashboard)
library(ggraph)
library(igraph)

corr_networks <- function(model.stm, df.col.topic.labels, min.correlation){
  
  #model stm 
  #df.col.topic.lable: column of dataframe of the topic-labels: example <- df.topics$labels
  #min.correlation: minimum of correlation between two nodes: numeric value 
    ## is used for the cutoff (delate edges)
  
  stm_corrs <- get_network(model = model.stm,
                         method = 'simple',
                         labels = paste(df.topic.labels$label),
                         cutoff = min.correlation, #Importent correlation cutoff!!!
                         cutiso = TRUE)
  
  #Network 1: 

graph <-ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.65) +
  scale_size(range = c(5, 13), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(2, 9)) +
  theme_graph()

ggsave("output_vis/corr_network.png", width = 50, height = 50, units = "cm", dpi = 300)
# Network 2: 
#Identify and delate bad topics 
#labels <- df.topic.labels$label
#labels
library(stringr)

#bad.topic <- labels[str_detect(labels, "xxx$")]
#print(bad.topic)
#delate bad topics (nodes) from the graph 


df.corrs2 <- igraph::as_data_frame(stm_corrs, "both")
df.corrs2$vertices$props <- df.corrs2$vertices$props*500

stm_corrs3 <- graph_from_data_frame(df.corrs2$edges, directed = F, vertices = df.corrs2$vertices)
bad.topic <- V(stm_corrs3)$name[str_detect(V(stm_corrs3)$name, "_xxx")]
stm_corrs4 <- stm_corrs3 - c(bad.topic)
#clp2 <- cluster_label_prop(stm_corrs4)
clp2 <- cluster_label_prop(stm_corrs4, weights = E(stm_corrs4)$weight)
#plot
plot_clp2 <- plot(clp2, 
                 stm_corrs4, 
                 vertex.size = V(stm_corrs4)$props,
                 vertex.label.cex=5) 
plot_clp2
#ggsave("output_vis/corr_network2.png", width = 50, height = 50, units = "cm", dpi = 300)


#ppi <- 1800
#png("output_vis/Korrelationsnetzwerk_Topic_Modularity.png", width=7*ppi, height=7*ppi, res=200)
#svg("output_vis/corr_network_moularity3.svg",width=70, height=90)
plot(clp2, 
     stm_corrs4, 
     vertex.size = V(stm_corrs4)$props,
     vertex.label.cex=5) 

#functions to plot your graph
dev.off()
#source("functions/iGraph_GexF_Exporter.R")
#saveAsGEXF(stm_corrs4, "output_data/graph.gexf")  


return(list(graph1 = graph, graph2 = stm_corrs4))

}

