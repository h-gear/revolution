# 0. Goal: network analysis, community detection and centralities ----
#	- Baseline aggregate network
#   - Community detection on aggregate network
#   - Plot with visnet to visualize groups
#   - Dynamic network analysis
#   - Calculate centralities for dynamic network

# 1. Libraries ----
library(visNetwork)
library(tidyverse)
library(ndtv)
library(tsna)
library(sna)
library(networkDynamic)

# 2. Read data ----
# the dataset includes the information on the most dominant topic for each letter
letters <- read.csv("data/processed/founders/letters_with_topic_info.csv", header = TRUE)

preprocessed <- readRDS("data/processed/founders/ffc_preprocessed.rds") %>%
mutate(id = as.integer(id))

## a) select letters based on topic of interest from "semi-supervised topic modeling.R" ----
read_edges <- letters %>%
    filter(topic == "01_Liberal politics") %>%
    select(id) %>%
    left_join(preprocessed, by = c("id")) %>%
    data.frame()

## b) select letters based on shico word-occurrences from "shico-based letter selection.R" ----
#read_edges <- read.csv("links_liberal.csv")
#read_edges <- read.csv("links_republican.csv")

# 3. Create edge List ----
edgelist <- read_edges %>%
   # calculate number of times in a year an author wrote to a specific recipient
    #distinct(authors,recipients,sender_id, receiver_id, start.year, end.year) %>%
    group_by(authors,recipients, sender_id, receiver_id, year,end.year) %>%
        mutate(weight = n() %>% as.numeric()) %>%

    distinct(authors,recipients, sender_id, receiver_id, year,end.year, weight) %>%
    mutate(onset.censored    = "FALSE",
           terminus.censored = "FALSE",
           onset             = as.numeric(year),
           terminus          = as.numeric(end.year)) %>%

    group_by(authors, recipients) %>%
        # assign a unique number to each author-recipient pairing (regardless of time)
        mutate(edge.id = cur_group_id()) %>%
    ungroup() %>%

    filter(onset == terminus) %>%
    mutate(duration = terminus - onset + 1) %>%
    select(onset,terminus,authors,recipients,onset.censored,
           terminus.censored,duration,edge.id,weight,
           sender_id, receiver_id) %>%
    arrange(authors,recipients,onset) %>%
    rename(head = receiver_id,
           tail = sender_id)

# reorder columns
edgelist <- edgelist %>%
    select(onset,terminus,tail,head,onset.censored,
           terminus.censored,duration,edge.id,weight,
           authors,recipients)

# add a measure that is the combined sum of all the letters send between two
# authors: A -> B plus B -> A: useful for networks with undirected edges
edgelist <- edgelist %>%
    group_by(onset, grp = str_c(pmin(tail, head), pmax(tail, head))) %>%
    mutate(undirected = sum(weight)) %>%
    ungroup %>%
    select(-grp)

# all unique edges that have ever occurred across the whole time period
edges2 <- edgelist %>% data.frame() %>%
    distinct(tail,head, authors,recipients)

# 4. Create node list ----
# obtain all unique nodes, either author or recipient, with their corresponding ids
edges_tail <- edges2 %>% distinct(tail,authors)    %>% rename(vertex.id = tail, name = authors)
edges_head <- edges2 %>% distinct(head,recipients) %>% rename(vertex.id = head, name = recipients)

# combine these to get a list of uniquely involved persons
nodes <- rbind(edges_tail,edges_head) %>% distinct(vertex.id,name)

# Distinguish the 7 founding fathers in the network with a color
top7 <- c("Washington, George",
          "Franklin, Benjamin",
          "Adams, John",
          "Hamilton, Alexander",
          "Jay, John'",
          "Jefferson, Thomas",
          "Madison, James"
)

nodes <- nodes %>%
    mutate(onset    = 1750,
           terminus = 1833,
           color2   = ifelse(name %in% top7, "gold", "green")) %>%
    select(vertex.id,name,onset,terminus,color2) %>%
    remove_rownames() %>%
    data.frame()

# converting columns to numeric
edgelist$onset    <- as.numeric(edgelist$onset)
edgelist$terminus <- as.numeric(edgelist$terminus)
edgelist$tail     <- as.numeric(edgelist$tail)
edgelist$head     <- as.numeric(edgelist$head)
edgelist$weight   <- as.numeric(edgelist$weight)
nodes$onset       <- as.numeric(nodes$onset)
nodes$terminus    <- as.numeric(nodes$terminus)
nodes$vertex.id   <- as.numeric(nodes$vertex.id)

# 5. Aggregated network ----
nw <- network(edgelist[,c(3,4,9)], # tail,head, weight
              vertex.attr = nodes[,c(1,2,5)], #vertext.id, name, color
              vertex.attrnames = c("vertex.id", "name", "color2"),
              directed  = TRUE,
              multiple  = TRUE,
              bipartite = FALSE,
              loops     = FALSE)

# summary of the network object
summary.network(nw, print.adj = FALSE)

# Plotting the static network
plot.network(nw,
             label         = "name",
             vertex.col    = "color2", # color nodes by gender
             vertex.cex    = 2, # set node size to a fixed value
             displaylabels = F, # show the node names
             mode          = "fruchtermanreingold",
             label.pos     = 5, # display the names directly over nodes
             label.col     = "black",
             edge.lwd = get.edge.value(nw,"weight")/2, # edge width
             displayisolates = F) # remove isolate nodes from plot)

## Community detection ----
library(igraph)

# Create igraph network
net <- igraph::graph_from_data_frame(d        = edgelist[,c(3,4,9)],
                                     vertices = nodes[,c(1,2)],
                                     directed = T)

net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)

if (igraph::is_weighted(net) != TRUE) {
    stop("Network not weighted.")
}

# Apply the Louvain algorithm for community detection
clp     <- igraph::cluster_louvain(igraph::as.undirected(net))
members <- igraph::membership(clp)
igraph::V(net)$community <- clp$membership

# Create cluster list for the shaded area
clusters <- data.frame(V(net)$community, V(net)$name) %>%
    group_by(V.net..community) %>%
    as.data.frame() %>%
    rename(name = V.net..name,
           group = V.net..community)

## merge community membership with nodes ----
# make sure name is identical in order to properly merge the nodes with the community ids
nodes <- nodes %>%
    left_join(clusters, by = "name")

set.seed(123)
E(net)$width <- log(E(net)$weight)
E(net)$arrow.size <- 0.2

# Layout
e <- get.edgelist(net, names = F)
l <- qgraph::qgraph.layout.fruchtermanreingold(e, vcount = vcount(net), niter = 1000)

# Cluster colors - Custom Color Palette
my_colors <- c("#05A4C0", "#85CEDA", "#D2A7D8", "#A67BC5", "#BB1C8B", "#8D266E")
colrs <- adjustcolor(my_colors)

# In weighted networks, we can also node strength, which is the sum of the
# weights of edges connected to the node. Let’s calculate node strength
# and plot the node sizes as proportional to these values
st <- igraph::graph.strength(net)

# plot with igraph
plot(net,
     edge.curved = .3,
     layout = l,

     vertex.color       = colrs[V(net)$community],
     vertex.frame.color = colrs[V(net)$community],
     vertex.size        = log(st), # st <- igraph::graph.strength(net)

     vertex.label.color = "black",
     vertex.label.font  = 1,
     vertex.label.cex   = 0.7,
     vertex.shape       = "circle",
     vertex.label.dist  = 0.3,
     edge.color         = "grey60",
     frame              = T,
     vertex.label       = NA)

## plot with visnetwork ----
edge_viz  <- edgelist[,c(3,4,9)] %>%
    rename(from = tail, to = head, value = weight) %>%
    mutate(color  = "grey", # set color of edges
           arrows = "to", # set arrow for each edge ("to", "middle", "from ")
           smooth = F) # each edge to be curved or not

nodes_viz <- nodes[,c(1,2,6)] %>%
    rename(id = vertex.id) %>%
    mutate(shape = "dot", # customize shape of nodes : "dot", "square", "triangle"
           shadow = F, # include/exclude shadow of nodes
           title = id, # tooltip (html or character), when the mouse is above
           label = name, # add labels on nodes
           size = 20, # set size of nodes
           borderWidth = 2, # set border width of nodes
           color.background = c("slategrey", "tomato", "purple","gold","darkturquoise")[nodes$group],
           color.border = "black", # set frame color
           color.highlight.background = "orange", # set color of the selected node
           color.highlight.border = "darkred") # set frame color of the selected node

visNetwork(nodes_viz, edge_viz, width = "100%", height = 800,
           main = "Social Network Analysis Founding Fathers") %>%
    visLayout(randomSeed = 4, improvedLayout = F) %>%
    visOptions(nodesIdSelection = TRUE,
               selectedBy       = "group",
               highlightNearest = list(enabled = TRUE, degree = 1)) %>%
    visPhysics(stabilization = TRUE)

# 6. Dynamic network ----
# create the dynamic network
net.dyn <- networkDynamic(
    edge.spells = data.frame("onset"          = edgelist$onset,
                             "terminus"       = edgelist$terminus,
                             "tail verted.id" = edgelist$tail,
                             "head vertex.id" = edgelist$head,
                             "weight"         = edgelist$weight),

    vertex.spells = data.frame("onset"        = nodes$onset,
                               "terminus"     = nodes$terminus,
                               "vertex.id"    = nodes$vertex.id,
                               "color2"       = nodes$color2,
                               "name"         = nodes$name),
    create.TEAs = TRUE)

# aligning information on nodes and edges (may take a couple of minutes)
reconcile.vertex.activity(net.dyn,
                          # vertices will be modified so as to be only active
                          # when incident edges are active
                          mode = "match.to.edges",
                          #mode = "expand.to.edges",
                          edge.active.default = TRUE)

# confirm that the network has been created correctly
print(net.dyn)

# create a sub-network of people who have at least 2 connections to have a meaningful network
sub.net.dyn <- network::get.inducedSubgraph(net.dyn, v = which(degree(net.dyn) > 1))
print(sub.net.dyn)
plot(sub.net.dyn)

# check dataframe
sub.df.netdyn <-  as.data.frame(sub.net.dyn)

# check: all TRUE?
network.dynamic.check(sub.net.dyn)

detach("package:igraph", unload = TRUE)
detach("package:networkD3", unload = TRUE)
list.edge.attributes(net.dyn)
list.vertex.attributes(net.dyn)

# Plot proximity timeline
set.seed(123)
proximity.timeline(sub.net.dyn,
                   default.dist = 6,
                   mode         = 'sammon',
                   labels.at    = 17,
                   vertex.cex   = 4,
                   render.edges = FALSE,
                   chain.direction = "reverse",
                   xaxt = "n")

timeline(sub.net.dyn,
         slice.par = list(start    = 1775,
                          end      = 1825,
                          interval = 1,
                          aggregate.dur = 1,
                          rule     = 'latest'),
         plot.vertex.spells = FALSE)

# snapshot approach by examining network-slices per year
# Collapse network to look at a specific network for a particular year (e.g., 1802)
par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))
plot(network.extract(net.dyn, at = 1801), main = "Time 1801",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(net.dyn, at = 1802), main = "Time 1802",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(net.dyn, at = 1803), main = "Time 1803",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)

# Plot a specific timepoint and remove isolates
par(mfrow = c(1,2))
net.dyn %>% network.extract(at = 1801) %>% plot(displayisolates = F,displaylabels = F)
net.dyn %>% network.extract(at = 1802) %>% plot(displayisolates = F)

# 7. Centralities of dynamic network ----
##time series of centralities

## A) Graph-Level statistics through time ----
par(mfrow = c(1,1))

# Plot formation of edges over time
plot(tEdgeFormation(net.dyn, time.interval = 1))

# the tsna package provides the function tSnaStats() as way to apply
# metrics from the sna package at multiple time points over a
# networkDynamic object and produce the result as a time-series.

colpal <- RColorBrewer::brewer.pal(4, "Dark2")

# graph level density
Density <-  tSnaStats(
    nd     = net.dyn,
    snafun = 'gden', # "gden" to show graph density
    start  = 1725,
    end =    1836,
    time.interval = 1,
    aggregate.dur = 1)
plot(Density, col = colpal[2], lwd = 2, main = "Network density during the study period")

# reciprocity (ratio of reciprocated edges to asymmetric edges)
Reciprocity_prop <-  tSnaStats(
    nd= net.dyn,
    snafun ='grecip',
    measure = 'edgewise',
    time.interval = 1,
    aggregate.dur = 1)
plot(Reciprocity_prop, col = colpal[2], lwd = 2, main = "Proportion of reciprocated edges during the study period")

Reciprocity <-  tSnaStats(
    nd = net.dyn,
    snafun ='grecip',
    time.interval = 1,
    aggregate.dur = 1)
plot(Reciprocity, col = colpal[3], lwd = 2, main = "Network reciprocity")

Dyad.census <- tSnaStats(nd = net.dyn, 'dyad.census')
plot(Dyad.census, col = colpal[2], lwd = 2, main = "Network dyads during the study period")

# show number of complete, reciprocated dyads
Dynamicmutuality <- tSnaStats(
    nd = net.dyn,
    snafun = 'mutuality',
    time.interval = 1,
    aggregate.dur = 1)
plot(Dynamicmutuality,col = colpal[2], lwd = 2, main = "number of complete reciprocated dyads during the study period")

dynamicDegree <- tSnaStats(
    net.dyn,
    snafun        = "centralization",
    start         = 1770,
    end           = 1830,
    time.interval = 1,
    aggregate.dur = 20,
    FUN           = "degree"
)
plot(dynamicDegree)

## B) Node-Level statistics through time ----
dynamicVertex <- tSnaStats(
    net.dyn,
    snafun        = "degree",
    start         = 1770,
    end           = 1825,
    time.interval = 1,
    aggregate.dur = 10
)
plot(dynamicVertex)

# 8. Animation of temporal network ----
# Calculate how to plot an animated version of the dynamic network
# To reveal slower structural patterns we can make the aggregation period
# even longer, and let the slices overlap (by making interval less than
# aggregate.dur) so that the same edge may appear in sequential slices and
# the changes will be less dramatic between successive views
compute.animation(
    net = net.dyn,
    animation.mode = "kamadakawai",
    slice.par = list(
        start = 1775,
        end = 1830,
        interval = 1,
        aggregate.dur = 1,
        rule = "any") # latest
)

render.d3movie(
    net.dyn,
    usearrows       = F,
    displaylabels   = TRUE,

    label           = "name",
    label.cex       = 0.6,
    label.col       = "black",
    bg              = "white",
    main            = 'Founding Fathers interactions, 1-year aggregation',

    # information for edges
    edge.arrow.size = .4,
    edge.col        = 'grey',
    edge.lwd        = function(slice){slice %e% 'weight' / 5},

    # information for nodes
    # vertex.cex      = function(slice){(sna::betweenness(slice) + 1)},
    vertex.cex      = function(slice){log(sna::degree(as.matrix(slice, attrname = "weight")))*2},
    vertex.border   = "#333333",
    vertex.col      = "color2",
    vertex.border   = "#333333",

    # This slice function makes the labels work
    vertex.tooltip    = function(slice) {
        paste("<b>Name:</b>", (slice %v% "name"))},
    edge.tooltip      = function(slice) {
        paste("<b>Weight:</b>", (slice %e% "weight"))},

    launchBrowser = TRUE, filename = "HGEAR.html",
    render.par    = list(tween.frames = 15, show.time = F),
    slice.par     = list(start = 0, end = 100, interval = 4, aggregate.dur = 4, rule = 'any'))

