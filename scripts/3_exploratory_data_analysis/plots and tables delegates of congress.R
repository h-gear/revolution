# 0. GOAL ----
# Plot an aggregated network on the data from the Letters of Delegates to Congress (1774-1789) 

# 1. LOAD IN LIBRARIES ----
library(tidyverse)
library(igraph)

# 2. LOAD PREPROCESSED DATA FOR PLOTTING ----
combined_df2 <- readRDS("delegates_plotting_data.rds")
glimpse(combined_df2)

# 3. PLOT AGGREGATED NETWORK ----

# select from and to variables
nw_data <- combined_df2[,c(12,13)] %>% 
    
    # network analysis requires information on sender_id and receiver_id 
    filter(!is.na(sender_id) & !is.na(receiver_id)) %>% 
    
    # create weight variable representing the amount of edges -> multiple becomes FALSE 
    group_by(sender_id,receiver_id) %>% 
    summarise(weight = n()) %>% 
    ungroup()  
# in case you want to focus on the founding fathers only:
# filter(sender_id %in% fathers_ids | receiver_id %in% fathers_ids)

# convert to network
net <- igraph::graph_from_data_frame(d = nw_data, directed = TRUE)

# simplify graph by removing loops and multiple edges between the same nodes
net <- igraph::simplify(net, remove.multiple = TRUE, remove.loops = TRUE)

plot(net)
# we create a smaller subset of the network of people who had a reasonable
# number of interactions (degree > 2)
subgraph <- induced_subgraph(net, vids = which(degree(net) > 20))

# Create a vector of neighbors for the main vertices
neighbor_vertices <- unlist(neighborhood(net, order = 0, nodes = table.ff$sender_id))

# Include main vertices and their first-degree neighbors
subgraph_vertices <- unique(c(table.ff$sender_id, neighbor_vertices))

# Create the induced subgraph
subgraph <- induced_subgraph(net, subgraph_vertices)

# Plot the subgraph
plot(subgraph, vertex.label = V(subgraph)$name)

# If you want the graph of founding father nodes and all their first degree neighbors
subgraph <- induced.subgraph(graph = net, 
                             vids = unlist(neighborhood(graph = net, 
                                                        order = 0, 
                                                        nodes = table.ff$sender_id)))

# Set layout options
l <- layout.fruchterman.reingold(subgraph)

# Plot subgraph
plot.igraph(x = subgraph,layout = l)

if (igraph::is_weighted(net) != TRUE) {stop("Network not weighted.")}

clp <- igraph::cluster_louvain(as.undirected(net))

# this can take some time when having a large network

# split communities by maximizing modularity
clp <- igraph::cluster_optimal(subgraph)

#members <- igraph::membership(clp)
igraph::V(subgraph)$community <- clp$membership

plot(clp, net,vertex.label.cex = 0.5)

# Create cluster list for the shaded area
clusters <- data.frame(V(net)$community, V(net)$name) %>% 
    group_by(V.net..community) %>% 
    mutate(group = paste0(V.net..name, collapse = " ")) %>% 
    summarise(group2 = first(group))

cluster_list <- clusters[["group2"]]

# # List instances
final_list = list()
i <- 1

for (group in cluster_list) {
    numbers <- str_split(group, pattern = "\\['")
    final_list[i] <- numbers
    i <- i + 1
}

e <- get.edgelist(net, names = F)
l <- qgraph::qgraph.layout.fruchtermanreingold(e, vcount = vcount(net), niter = 90000)

# Cluster colors - Custom Color Palette
my_colors <- c("#05A4C0", "#85CEDA", "#D2A7D8", "#A67BC5", "#BB1C8B", "#8D266E")

#colrs <- adjustcolor( c(my_colors, "yellowgreen", "yellowgreen", "lightblue", "lightblue",
#                        "lightgreen", "maroon"), alpha = .9)
colrs <- adjustcolor(my_colors)

# In weighted networks, we can also node strength, which is the sum of the 
# weights of edges connected to the node. Let’s calculate node strength 
# and plot the node sizes as proportional to these values
st <- igraph::graph.strength(net)

# communities
Group1 = as.numeric(V(net)[as.character(c(5,11,115,151,208,214,357,451,453,467,508,530,670,746,49,486,573,724,1461))])
Group2 = as.numeric(V(net)[as.character(c(8,24,46,139,205,233,472,507,1314))])
Group3 = as.numeric(V(net)[as.character(c(14,19,56,60,91,93,103,104,129,149,187,213,390,720,20,732,778,1363,1448,183,277,697,1081,12,543,611,840,408,425,684,432,583,928,954,254))])

plot(net, 
     edge.curved = .3, 
     layout = l,
     
     vertex.color       = colrs[V(net)$community], 
     vertex.frame.color = colrs[V(net)$community], 
     #vertex.size        = 11,
     vertex.size        = log(st), # st <- igraph::graph.strength(net)
     
     vertex.label.color = "black", 
     vertex.label.font  = 1, 
     vertex.label.cex   = 0.7,
     vertex.shape       = "circle", 
     vertex.label.dist  = 0.3, 
     edge.color         = "grey60", 
     frame              = T,
     vertex.label       = V(net)$name,
     mark.groups        = list(Group1, Group2, Group3)
)

# 4. SAVE PLOT OF NETWORK ----
#TODO: save plot of network