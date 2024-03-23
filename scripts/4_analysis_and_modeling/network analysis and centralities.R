# 0. Goal: network analysis, community detection and centralities ----
#	- Baseline aggregate network
#   - Igraph community detection on aggregate network
#   - Plot with visnet to quickly visualize groups

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

# network across the whole time period
nw <- network(edgelist[,c(3,4,9)], # tail,head, weight
              vertex.attr = nodes[,c(1,2,5)], #vertext.id, name, color
              vertex.attrnames = c("vertex.id", "name", "color2"),
              directed  = TRUE,
              multiple  = TRUE,
              bipartite = FALSE,
              loops     = FALSE)

# summary of the network object
summary.network(nw, print.adj = FALSE)
list.vertex.attributes(nw)
list.edge.attributes(nw)

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

#nodes$name <- lapply(nodes$name, gsub, pattern = '[^[:alnum:], ]', replacement = "")
#nodes$name <- lapply(nodes$name, function(z){ z[!is.na(z) & z != ""]})
#nodes$name <- sapply(nodes$name, trimws)
# switch first and last name
#nodes$name <- lapply(nodes$name, function(z) { gsub("(\\w+),\\s(\\w+)","\\2 \\1", z)})
#nodes$name <- as.character(nodes$name)

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
# inspiration from https://www.kaggle.com/code/andradaolteanu/covid-19-sentiment-analysis-social-networks
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
           shadow = TRUE, # include/exclude shadow of nodes
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
    visLayout(randomSeed = 4, improvedLayout = T) %>%
    visOptions(nodesIdSelection = TRUE,
               selectedBy       = "group",
               highlightNearest = list(enabled = TRUE, degree = 1)) %>%
    visPhysics(stabilization = TRUE)

# 6. Dynamic network ----
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

# check: dataframe
df.netdyn <-  as.data.frame(net.dyn)

# check: all TRUE?
network.dynamic.check(net.dyn)

detach("package:igraph", unload = TRUE)
detach("package:networkD3", unload = TRUE)
list.edge.attributes(net.dyn)
list.vertex.attributes(net.dyn)

# Collapse network to look at the specific network for a particular year
net1802 <- network.collapse(dnet = net.dyn,
                            at = 1802, # look at a single time point
                            rm.time.info = FALSE,
                            retain.all.vertices = F,
                            rule = "latest")
plot(net1802)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))
plot(network.extract(net.dyn, at = 1801), main = "Time 1801",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(net.dyn, at = 1802), main = "Time 1802",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(network.extract(net.dyn, at = 1803), main = "Time 1803",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)
plot(net.dyn, main = "Collapsed",
     displaylabels = T, displayisolates = F,label.cex = 0.6, vertex.cex = 2, pad = 0.5)

# Plot a specific timepoint and remove isolates
par(mfrow = c(1,2))
net.dyn %>% network.extract(at = 1801) %>% plot(displayisolates = F,displaylabels = F)
net.dyn %>% network.extract(at = 1802) %>% plot(displayisolates = F)

#look into the weights
get.edge.attribute.active(net1780,'weight',at = 1780)
get.edge.value.active(net.dyn, 'weight',
                      onset        = 1797,
                      terminus     = 1798,
                      dynamic.only = TRUE)

# 7. Centralities ----

# Static (per year)
# centrality scores (without weights)
sna::degree(net1802)
sna::degree(net1802,cmode = "indegree")
sna::degree(net1802,cmode = "outdegree")

# centrality scores (with weights)
sna::degree(as.edgelist.sna(net1802,attrname = "weight"))

# Dynamic (over the years)

# rescale=TRUE argument -> If the effective size of the network is changing due
# to vertex activity, this would allow the sna metric to renormalize with network
# size to reduce possible variations due to making comparisons between networks
# of varying size
prestScores <- tsna::tSnaStats(net.dyn,'gtrans')

hist(edgeDuration(net.dyn),,xlim=c(-2,2))

# the tsna package provides the function tSnaStats() as way to apply
# metrics from the sna package at multiple time points over a
# networkDynamic object and produce the result as a time-series. However,
# tSnaStats(net.dyn,'degree') does not account for edge weights. See
# also https://stackoverflow.com/questions/47403869/cant-get-edge-weights-to-count-in-temporal-network-centrality-scores

# In weighted networks, the degree centrality is calculated as the sum of
# weights assigned to the node's direct connections and represents the node strength
# See article: Node centrality in weighted networks: Generalizing degree and shortest paths

unique_nodes <- length(unique(nodes$name))
start_year     <- min(edgelist$onset)
end_year       <- max(c(edgelist$onset,edgelist$terminus))
range_of_years <- (end_year - start_year) + 1

# weighted.degrees will contain the weighted degrees per year for each node
# start with empty matrix (time-periods * nodes)
weighted.degrees <- matrix(0, range_of_years, unique_nodes)
dim(weighted.degrees)

#relmat <- matrix(0, seq(from = start_year, to = end_year), unique_nodes)
rownames(weighted.degrees) <-  seq_along(start_year:end_year) + start_year - 1
colnames(weighted.degrees) <- unique(nodes$name)

weighted.degrees <- weighted.outdegrees <- weighted.indegrees

#iterate over time periods
for (i in start_year:end_year) {
    temp <- network.collapse(net.dyn, at = i, rm.time.info = FALSE,  rule = "latest")
    #save results in predefined matrix
    weighted.degrees[i - start_year,seq(1:unique_nodes)] <- sna::degree(as.matrix(temp, attrname = "weight"))
    weighted.betweenness[i - start_year,seq(1:unique_nodes)] <- sna::betweenness(as.matrix(temp, attrname = "weight"))
    #weighted.indegrees[i - start_year,seq(1:unique_nodes)] <- sna::degree(as.matrix(temp, attrname = "weight"),cmode = "indegree")
    #weighted.outdegrees[i - start_year,seq(1:unique_nodes)] <- sna::degree(as.matrix(temp, attrname = "weight"),cmode = "outdegree")
}

# replace 0 with NA
weighted.degrees[weighted.degrees == 0]       <- NA
weighted.indegrees[weighted.indegrees == 0]   <- NA
weighted.outdegrees[weighted.outdegrees == 0] <- NA

# remove rows which  all have NA's
weighted.degrees <- weighted.degrees[rowSums(is.na(weighted.degrees)) != ncol(weighted.degrees), ]
weighted.indegrees <- weighted.indegrees[rowSums(is.na(weighted.indegrees)) != ncol(weighted.indegrees), ]
weighted.outdegrees <- weighted.outdegrees[rowSums(is.na(weighted.outdegrees)) != ncol(weighted.outdegrees), ]

#scale the weighted degrees
#tets <- t(scale(t(weighted.degrees)))

scaled.degrees    <- round(scrime::rowScales(weighted.degrees),3)
scaled.indegrees  <- round(scrime::rowScales(weighted.indegrees),3)
scaled.outdegrees <- round(scrime::rowScales(weighted.outdegrees),3)

scaled.degrees <- as.data.frame(weighted.degrees)
scaled.degrees$year <- rownames(scaled.degrees)

df <- scaled.degrees %>% gather(key = "variable", value = "value", -year)
head(df)

# plot scaled weighted degrees (in and outdegree)
# xas time, y-as weighted degrees (z-scores), with each line
# representing a FF
df7 <- df %>% filter(variable %in% top7)

ggplot2::ggplot(df7, aes(x = year, y = log(value), group=variable)) +
    geom_line(aes(color = variable)) +
    viridis::scale_fill_viridis() +
    #scale_color_manual(values = c("darkred", "steelblue","gold","yellow","orange","green","purple")) +
    theme_minimal()

# Apply the sna package’s static graph- and vertex-level network descriptive statistics at multiple time points
# Rolling- measures using Sliding Time Windows (careful here with interference from igraph; detach igraph)

# 8. Graph-Level statistics through time ----
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

# 9. Vertex-level statistics through time ----
dynamicVertex <- tSnaStats(
    net.dyn,
    snafun        = "degree",
    start         = 1770,
    end           = 1825,
    time.interval = 1,
    aggregate.dur = 10
)
plot(dynamicVertex)

# 10. Timelines and Time Prisms ----
# Plot proximity timeline
set.seed(123)

proximity.timeline(net.dyn,
                   default.dist = 6,
                   mode         = 'sammon',
                   labels.at    = 17,
                   vertex.cex   = 4,
                   render.edges = FALSE,
                   #vertex.col = mycol[factor(sp_attr$SWSN_MacroGroup)],
                   chain.direction = "reverse",
                   xaxt = "n")

timeline(net.dyn,
         slice.par = list(start    = 1775,
                          end      = 1825,
                          interval = 1,
                          aggregate.dur = 1,
                          rule     = 'latest'),
         plot.vertex.spells = FALSE)


# 11. Density over the years ----
classNets <- get.networks(net.dyn,
                          start=1770,
                          end=1826,
                          time.increment=1,
                          rule='latest')

classDensity <- sapply(classNets, network.density)

plot(classDensity,type='l',xlab='network slice #',ylab='density')

colpal <- RColorBrewer::brewer.pal(4, "Dark2")
tSnaStats(net.dyn, snafun = "gden") %>% # "gden" to show graph density
    plot(col = colpal[2], lwd = 2, main = "Network density during the study period")

# Plot formation of edges over time
plot(tEdgeFormation(net.dyn, time.interval = 1))

plot(tSnaStats(net.dyn, snafun = 'gtrans'))

tSnaStats(net.dyn, snafun = "grecip") %>%
    plot(main = "Network Reciprocity", col = colpal[3], lwd = 2)

# 12. Animation ----
filmstrip(net.dyn, displaylabels=F, mfrow=c(2, 5),
          slice.par=list(start=1775, end=1824, interval=5,
                         aggregate.dur=5, rule='any'))

## Plot static “filmstrip” sequence ----
filmstrip(net.dyn,
          displaylabels = FALSE,
          #mfrow=c(1,6),
          #frames = 5,
          slice.par = list(start = 1780,
                           end = 1805,
                           interval = 1,
                           aggregate.dur = 1,
                           rule = 'all'))

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
    #tween.frames    = 30,
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
    # vertex.cex      = scales::rescale(degree(n1), to = c(1, 5))
    # vertex.cex      = function(slice){slice%v%'size' * 3},
    # vertex.cex      = function(slice){(degree(slice)+1)/5},
    # vertex.cex      = (net.dyn %v% "size.active"),

    vertex.col      = "color2",
    vertex.border   = "#333333",

    # This slice function makes the labels work
    vertex.tooltip    = function(slice) {
        paste("<b>Name:</b>", (slice %v% "name"))},
    edge.tooltip      = function(slice) {
        paste("<b>Weight:</b>", (slice %e% "weight"))},

    #output.mode   = "htmlWidget",
    launchBrowser = TRUE, filename = "HGEAR.html",
    render.par    = list(tween.frames = 15, show.time = F),
    slice.par     =list(start=0, end=100, interval = 4, aggregate.dur = 4, rule='any'))

# 13. Reachability ----

# https://github.com/AFMessamore42/the-civic-elite/blob/main/long_2010s.Rmd
# https://github.com/AFMessamore42/the-civic-elite/blob/f53dfa0a6b54cfdef1ff49875148b08ddf59e5e1/long_2010s.Rmd
# https://github.com/jalapic/SNA_workshop/blob/c42f47887cbf574b8d92e772c5156c1a82941bc1/day4/027_tsna.R
# https://github.com/statnet/tsna/blob/9e07a36cb0055d2659a5bdebc68f2294737e9147/vignettes/tsna_vignette.Rmd




# The reachability of a person help us to identify important nodes in the network
# that may have a key role in the spread of ideas. Forward reachability ("fwd")
# refers to all the nodes than can be reached from a given node and backward
# reachability ("bkwd") refers to those nodes that reach a given node $v_i$.
# We can visualize the distribution of the forward and backward reachability
# using boxplots

# see https://cadms-ucd.github.io/spatialnetworks_ws/Lab4.html

#forward path metrics
#(see https://statnet.org/Workshops/ndtv_workshop.html#forward-path-metrics)

# Calculate and store the sizes of forward and backward reachable sets for each node
#TODO: check graph.step.time option (set to 1?)
fwd_reach  <- tReach(net.dyn, direction = "fwd",graph.step.time = 0)
bkwd_reach <- tReach(net.dyn, direction = "bkwd",graph.step.time = 0)

# what fraction of the network could each vertex reach?
tReach(net.dyn)/network.size(net.dyn)

max_reachable_sets <- max(fwd_reach)

par(mar=c(2,4,2,2))
boxplot(fwd_reach, bkwd_reach, col = "lightgreen", horizontal = T,
        yaxt = 'n', main = "Reachability of persons")
axis(2, at = c(1,2), labels = c("Fwd", "Bwd"), las=2, lty = 0)

#Let's have a look at the persons with the highest forward reachability
all_fwd_bkwd <- data.frame("fwd"         = fwd_reach,
                           "bkwd"        = bkwd_reach,
                           "name"        = nodes$name,
                           "id"          = nodes$vertex.id)

all_fwd_bkwd %>% arrange(desc(fwd)) %>% head(10)

## Compare forward and backward reachability ----
# Any interesting relationships between both types of reachability?
plot(fwd_reach, bkwd_reach, xlab = "Fwd", ylab = "Bwd", pch = 16,
     col = rgb(0, 155, 50, max = 255, alpha = 100))

# The following code generates a plot comparing the forward
# and backward reachability of each author. Authors with
# exceptionally large forward or backward reachability are
# labelled for convenience

ggplot(all_fwd_bkwd, aes(x = fwd,y = bkwd)) +
    geom_point(color = "black", position = "jitter") +
    ggrepel::geom_label_repel(
        data = all_fwd_bkwd %>% filter(fwd > 15 | bkwd > 40 | (fwd > 15 & bkwd > 30)),
        aes(label = name),
        #nudge_x = 1, nudge_y = 0.5,
        check_overlap = T
    ) +
    hrbrthemes::theme_ipsum(
        axis_title_size = 12,
        axis_text_size = 11,
        plot_margin = margin(0, 0, 0, 0),
        grid_col = "#cccccc",
        grid = TRUE,
        axis_col = "#000000",
        axis = TRUE,
        ticks = FALSE
    ) +
    theme(
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 10,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    ggtitle("Backward- vs Forward-Reachable Sets") +
    ylab("size of backward-reachable set") +
    xlab("size of forward-reachable set") +
    scale_x_continuous(breaks = seq(0,270, by = 15))

# Calculate and plot the forward reachable paths of specific person
# TODO: check settings

network::get.vertex.attribute(net.dyn, "vertex.names")[1]

# Path Visualizations
# Examining node id = 1
v1FwdPath <- tPath(net.dyn,
                   v = 1,
                   direction = "fwd",
                   start = 1770,
                   #start = min(nodes$onset),
                   end = max(nodes$terminus))

is.tPath(v1FwdPath)
print(v1FwdPath)

# Examining node id = 4
v4FwdPath <- tPath(net.dyn,
                   v = 4,
                   direction = "fwd",
                   start = 1770,
                   #start = min(nodes$onset),
                   end = max(nodes$terminus))

## Static visualizations

plot(v1FwdPath,displaylabels = TRUE)
#plot(v1FwdPath, edge.lwd = 1, layout.par = list(gv.engine = 'dot'))

# Visualize the paths in the full network
coords <- plot(v1FwdPath, main="Forward Reachable Set from John Adams", cex.main = .5)

par(mfrow=c(1,2)) # set up side-by-side plot
plotPaths(net.dyn, v1FwdPath,
          coord = coords,
          main = "fwd path from v1 Adams",
          label.cex = .5, cex.main=2,
          vertex.cex = 2, displaylabels = F,
          vertex.col = colpal[1])

plotPaths(net.dyn, v4FwdPath,
          coord = coords,
          main = "fwd path from v4 Jefferson",
          label.cex = .5, cex.main=2,
          vertex.cex = 2, displaylabels = F,
          vertex.col = colpal[1])

par(mfcol=c(1,1)) # turn off side-by-side plots

# A better way to visualize the paths trough the time period is using the
# function transmissionTimeline(). In this visualization, the x axis
# represents the time when the event happened and the y axis represents
# the generation or steps that happened to reach that node
# list.vertex.attributes(net.dyn)

# check time period -> due to timing in nodes df?
transmissionTimeline(v1FwdPath,
                     #                    time.attr = net.dyn,
                     jitter = T,
                     main   = 'Earliest forward path from vertex 1[John Adams]',
                     displaylabels = T)

# We can also see the backward reachability, which represents the set of nodes
# that reach a specific node over a time period
# Calculate and plot the backward reachable paths of node 6
v1BkwdPath <- tPath(net.dyn, v = 1, direction = "bkwd",  type = 'latest.depart')
v4BkwdPath <- tPath(net.dyn, v = 4, direction = "bkwd",  type = 'latest.depart')
plot(v1BkwdPath, edge.col = colpal[3])

plotPaths(net.dyn,v1BkwdPath, path.col = rgb(0, 97, 255, max = 255, alpha = 166),
          displaylabels = TRUE,
          edge.label.col = rgb(0,0,0,0),
          vertex.col = "white"
)


# Another way to visualize the network activity is to show the time points
# when each of the nodes was active. In the following plot, the upper part
# represents the presence of the node in the network and the lower part
# represents the activity of the node in the network.
timeline(net.dyn)

## Dynamic Visualization

# pull out the vertex.ids and tdist (onset must be greater than this)
actor <- as.data.frame(v1FwdPath) %>%
    mutate(vertex.id = row_number()) %>%
    filter(tdist != 'Inf') %>%
    select(vertex.id, tdist)

# tail must be an element of actor$vertex.id and
# the corresponding onset must be greater or equal to the tdist associated with that vertex.id
es.actor <- edgelist %>%
    right_join(actor, by = c("tail" = "vertex.id")) %>%
    filter(onset>=tdist)

# filter nodes--keep only those in edgelist
vs2 <- nodes %>%
    filter(vertex.id %in% es.actor$tail | vertex.id %in% es.actor$head) %>%
    mutate(vertex.id = as.numeric(factor(name)),
           att = ifelse(onset<10,0,1))

# assign new node ids (must be inceasing start with 1)
node_ids2 <- vs2 %>% select(vertex.id, name) %>% unique()

# match edgelist with new node ids
es2 <- es.actor %>%
    select(-head, -tail) %>%
    left_join(node_ids2, by=c('authors'='name')) %>%
    rename(head = vertex.id) %>%
    left_join(node_ids2, by=c('recipients'='name')) %>%
    rename(tail = vertex.id) %>%
    filter(head %in% node_ids2$vertex.id & tail %in% node_ids2$vertex.id) %>%
    select(onset,terminus,tail,head,authors,recipients,text,edge_type)


# create dynamic network
net2.dyn <- networkDynamic(edge.spells=es2,
                           vertex.spells=vs2,
                           create.TEAs=TRUE,  # create dynamic attributes
                           edge.TEA.names=c('from','to','text','edge_type'),  # list dynamic edge attributes in order starting w/ col 5 of es
                           vertex.TEA.names=c('username','description','location','verified','follower_count','indegree','att')) # list dynamic node attributes in order starting w/ col 4 of vs


# length of these edgelists needs to match
nrow(es2)
nrow(as.data.frame(net2.dyn))


# set time interval
net2.dyn <- compute.animation(net2.dyn,
                              slice.par=list(start=min(es2$onset),
                                             end=max(es2$onset),
                                             interval=1,  # currently daily, set to 7 for weekly
                                             aggregate.dur=1,  # currently daily, set to 7 for weekly
                                             rule="earliest"))

# dynamic visualization
render.d3movie(net2.dyn,
               usearrows = FALSE,
               edge.lwd = 2,
               edge.col = 'grey',
               edge.tooltip = function(slice) {     # display edge attributes when you click on an edge
                   paste("<b>From:</b>", (slice %e% "from"),      "<br>",
                         "<b>To:</b>",   (slice %e% "to"),        "<br>",
                         "<b>Type:</b>", (slice %e% "edge_type"), "<br>",
                         "<b>Text:</b>", (slice %e% "text"))
               },
               vertex.cex = .3, # add sizing by attrbute
               # vertex.cex = .3, # add sizing by attrbute
               vertex.col =  function(slice) {
                   case_when((slice %v% "verified" == TRUE) ~ 'green',  # verified nodes are green, all others are grey
                             (slice %v% "verified" == FALSE) ~ 'grey',
                             is.na(slice %v% "verified") ~ 'grey')
               },
               vertex.sides =  function(slice) {
                   case_when((slice %v% "name" == "John Adams") ~ 3,   # John Adams is a triangle
                             (slice %v% "name" != "John Adams") ~ 50)  # everyone else is a circle
               },  # change node shapes by attribute
               vertex.tooltip = function(slice) {     # display node attributes when you click on an node
                   paste("<b>name:</b>",    (slice %v% "name") ,    "<br>",
                         "<b>Location:</b>",    (slice %v% "location") ,    "<br>",
                         "<b>Description:</b>", (slice %v% "description") , "<br>",
                         "<b>Followers:</b>",   (slice %v% "follower_count"))
               },
               main = 'John Adams forward paths',
               output.mode = 'htmlWidget')

# function to extract the transmission tree as a directed network
# https://statnet.org/workshop-ndtv/ndtv_workshop.html

transTree <- function(net){
    # for each vertex in net who knows
    knowers <- which(!is.na(get.vertex.attribute.active(net,
                                                        'knowsRumor',at=Inf)))
    # find out who the first transmission was from
    transTimes<-get.vertex.attribute.active(net,"heardRumorFrom",
                                            onset=-Inf,terminus=Inf,return.tea=TRUE)
    # subset to only ones that know
    transTimes<-transTimes[knowers]
    # get the first value of the TEA for each knower
    tellers<-sapply(transTimes,function(tea){tea[[1]][[1]]})
    # create a new net of appropriate size
    treeIds <-union(knowers,tellers)
    tree<-network.initialize(length(treeIds),loops=TRUE)
    # copy labels from original net
    set.vertex.attribute(tree,'vertex.names',treeIds)
    # translate the knower and teller ids to new network ids
    # and add edges for each transmission
    add.edges(tree,tail=match(tellers,treeIds),
              head=match(knowers,treeIds) )
    return(tree)
}

windTree <- transTree(net.dyn)

plot(windTree,
     displaylabels = TRUE)

