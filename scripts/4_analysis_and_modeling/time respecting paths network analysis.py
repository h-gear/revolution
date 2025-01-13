# ----------------------------------------------------------------------------------------
# PURPOSE:
# The purpose of this script is to analyse time-respecting paths of a letter correspondence
# network of writers in the era of the American Revolution. We aim to identify key persons/
# writers and patterns of communication in the transmission of political ideas through 
# letters. We distinguish between two types of political thinking: liberal ideology and 
# republican ideology. By employing a temporal network approach, the chronological order
# of the edges within the network are preserved. Unlike traditional aggregated networks
# that disregard the temporal information that is implicit in the letter data, this 
# approach significantly mitigates the bias in topological network measures.
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# This code is adapted from and builds on the code by Dr. Ramona Roller as part of her 2023
# article "Tracing the Footsteps of Ideas: Time-respecting Paths Reveal Key Reformers and 
# Communication Pathways in Protestant Letter Networks."
# https://doi.org/10.3929/ethz-b-000651473
# ----------------------------------------------------------------------------------------

# 0. load libraries ----------------------------------------------------------------------
%reload_ext autoreload
%autoreload 2
from datetime import datetime
import pathpy as pp
import pandas as pd
import numpy as np
import warnings
import igraph
import seaborn as sns
from tqdm import tqdm_notebook as tqdm_note
from scipy.stats import chi2
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
sns.set()

warnings.filterwarnings("ignore")

# --------------------------------------------------------------------------------------
# 0. Import homogeneous subset of the data 
# --------------------------------------------------------------------------------------

# Homogeneous data is based on either: 
# 1) "shico-based letter selection.R"   : subset of letters is made based on year-dependent wordoccurences from shico
# 2) "semi_supervised_topic_modeling.R" : subset of letters is made based on topic probability 

# in both cases, we have a dataset inclusing the variables:
# "authors","recipients","sending_date","time","sender_id","receiver_id"

# Set global paths to read in the data and to store the results
path_to_data    = '../../data/processed/founders/'
path_to_results = '../../output/figures/'

# --------------------------------------------------------------------------------------
# 1. Prepare data for analyzing aggregated networks and path-respecting networks
# --------------------------------------------------------------------------------------

# Loading the letter data on liberal politics or on republican politics. Both datasets
# are homogenous subsets of the original dataset. The original dataset contains letters
# of writers on a large variety of topics. We can use either the results from:
# 
# 1) the shico analysis through word counts
# 2) semi-supervised topic analysis
#
# We select those letters that are centered on a specific political ideology, either by 
# filename or by selecting the appropriate topic number. 

# select dataset: liberal or republican
ideology = "_" + "republican"
#ideology = "_" + "liberal"

# If 1) SHICO ---- 
# Run the following analysis separatedly on either 'links_liberal' or 'links_republican'
#file_path = path_to_data + 'links{}.csv'.format(ideology)
#links = pd.read_csv(file_path)
#display(links.head())

# If 2) TOPIC ANALYSIS ----
file_path = path_to_data + 'letters_with_topic_info.csv'

# read in all letters
letters = pd.read_csv(file_path)
display(letters.head(3))

# make subset of letters using topic columnn as a document-level variable indicating to which topic a letter belongs
#links = letters[letters['topic'] == "01_Liberal politics"] 
links = letters[letters['topic'] == "02_Republican politics"] 
display(links.head(1)) # the first observation of the subset
display(links.tail(1)) # the last observation of the subset

links.info()

nr42 = letters[letters['sender_id'] == 1696] 
len(nr42)

# amount of letters
len(links.index)  # "01_Liberal politics": ca. 2265
                  # "02_Republican politics": ca. 5781

# Drop links without a sending date
data = links[['sender_id', 'receiver_id', 'sending_date']]
data = data[~data['sending_date'].isna()]

data.info()
# Since pathpy uses unix timestamps which can only work with dates after 1st January 1970,
# we convert the sending dates of letters into time differences
def get_time_diff(d):

    '''
    Convert sending dates to time difference between
    earliest sending date in the data and current sending date. 
    
    @ params
    d [datetime]: Sending date of current letter.
    
    @returns
    time_diff [int]: Number of days between current sending date and earliest sending date.
    '''
    # adapted for format of date information of fo and delegates 
    earliest_date = datetime.strptime(data['sending_date'].iloc[0], '%Y-%m-%d')
    this_date     = datetime.strptime(d, '%Y-%m-%d')
         
    time_diff = (this_date - earliest_date).days
    return time_diff
    
# Store time differences in separate column called 'time' as required by pathpy
data['time'] = data['sending_date'].apply(lambda d: get_time_diff(d))

# Rename columns to comply with pathpy's requirements
data.rename(columns={'sender_id':'source', 'receiver_id':'target'}, inplace=True)

# Remove original sending dates of letters
data.drop('sending_date', axis = 1, inplace = True)

# We now have source, target and time columns as required by pathpy
display(data.head())

# Check amount of letters again
len(data.index) # 10043 letters

# save data to file
#data.to_csv(path_to_data + 'data_pathpy.csv')
data.to_csv(path_to_data + 'data_pathpy{}.csv'.format(ideology))

# Prepare data for aggregated network
data.drop('time', axis = 1, inplace = True)

# Save multi-edge network (multiple edges between the same two nodes is possible)
# data.to_csv(path_to_data + 'data_multiedge.csv', header = False, index = False, sep = '\t')
data.to_csv(path_to_data + 'data_multiedge{}.csv'.format(ideology), header = False, index = False, sep = '\t')

# Weighted network
grouped = data.groupby(['source', 'target'])
edges   = [name for name, group in grouped]
weights = [len(group) for name, group in grouped]
sources = [edge[0] for edge in edges]
targets = [edge[1] for edge in edges]

df = pd.DataFrame(list(zip(sources, targets, weights)), columns =['source', 'target', 'weight'])
display(df.head())

# save weighted network to file
df.to_csv(path_to_data + 'data_weighted{}.csv'.format(ideology), sep=' ', header = False, index = False)

# --------------------------------------------------------------------------------------
# 2. Compute time-respecting paths from the letter correspondence data 
# --------------------------------------------------------------------------------------

# Paths require the parameter 'delta t', corresponding to the maximum time to be
# passed between two consecutive edges. For example, when  delta t = 49, a path will be
# broken when more than 49 days have passed between two consecutive edges in a path.

# Compute a temporal network ----
# A temporal network provides edges with time stamps (the sending dates of letters)
# and orders edges according to these time-stamps. Note that the temporal network
# only accounts for the temporal sequence of edges, ignoring whether their senders
# and recipients form time-respecting paths (see below).
temp_net = pp.TemporalNetwork.read_file(path_to_data + 'data_pathpy{}.csv'.format(ideology), separator = ',', directed = True)

# Basic summary statistics of the temporal network, incl. the number of nodes, edges, and time stamps
print(temp_net)

# Generate path objects for different delta t. The unit of delta t is the same as the time unit in the data,
# i.e., in our case days. 

# my delta t's covering 1 to 7 weeks 
delta_ts = [7,14,21,28,35,42,49,56]

paths = []

pbar = tqdm_note(total = len(delta_ts))

# Since the computation of paths takes some time, we save paths as separate objects
# to load them for later analyses.
for delta_t in delta_ts:
    paths = pp.path_extraction.paths_from_temporal_network(temp_net, delta = delta_t)
    pp.Paths.write_file(paths, path_to_results + 'paths_deltaT_{}.ngram'.format(delta_t), separator = ',')
    pbar.update()
pbar.close()

# --------------------------------------------------------------------------------------
# 3. Compute betweenness centrality of nodes in time-respecting paths and in
#    aggregated network
# --------------------------------------------------------------------------------------

# In time-respecting paths, information can only flow forward in time because the
# temporal ordering of edges is preserved. In contrast, an aggregated network 
# ignores time which allows information to also flow backwards in time. 
# In communication networks, the betweenness of a node indicates to what extent
# that node controbutes to the transmission of ideas in the network.

# we base our results on delta t = 49 (7 weeks)
delta_t = 49

nodes = pd.read_csv(path_to_data + 'nodes_ff.csv')
paths = pp.Paths.read_file(path_to_results + 'paths_deltaT_{}.ngram'.format(delta_t), separator = ',')

display(nodes.head())

# overview of different path lengths
print(paths) # The results suggest the most frequent path length = 2.

# verify type (pathpy.classes.paths.Paths)
type(paths)

# get an overview of all the different paths of length k
for l in paths.paths:
    for p in paths.paths[l]:
        if paths.paths[l][p][1]>0:
            print('{0} -> {1}'.format(p, paths.paths[l][p][1]))

# As an example, look to a group of paths with a specific length
type(paths.paths[11])
paths.paths[7]

# create a network of paths
network_graph = pp.Network.from_paths(paths)
print(network_graph)

style = {'width': 300, 
          'height': 300,
          'node_size': 18.0,
          'edge_width' : 4.0,
          'node_color' : {'a': '#aacc99', 'b': '#aacc99', 'd': '#aacc99', 'e': '#aacc99', 'c': '#cc6666'},
          'edge_color' : '#ffaaaa',
          'edge_arrows': False,
          'label_color': '#000000',
          'label_opacity': 1,
          'label_offset': [0,0],
          'label_size': '20px',
          'edge_opacity': 1, 
          'force_charge': -10.0, 
          'force_repel': -550, 
          'force_alpha': 0.01
         }
pp.visualisation.plot(network_graph, **style)

## A) Betweenness for time-respecting paths ------------------------------------------

# Compute node betweenness on time-respecting paths
# Note that we work with individual paths -> no need to work with weights 
btw_path = pp.algorithms.centralities.betweenness(paths)

# Sort betweennesses by value
btw_path = {k: v for k, v in sorted(btw_path.items(), reverse=True, key=lambda item: item[1])}
btw_path

# Compute path betweenness as percentage of theoretically possible paths in temporal network
# theoretically possible: all nodes communicate at least indirectly with each other

# Number of theoretically possible paths
paths_theor_count = (temp_net.vcount())**2 - temp_net.vcount()

print('Number of theoretically possible paths in temporal network:', paths_theor_count)

# Betweenness as percentage of theoretically possible paths
btw_path_perc = {}
for k,v in btw_path.items():
    btw_path_perc[k] = (v/paths_theor_count)*100

# Display betweenness values as percentage of theoretical possible paths
btw_path_perc

## B) Betweenness centralities for aggregated network ---------------------------------

# Build aggregated network
# separator should be a whitespace and no header allowed
g_weighted = igraph.Graph.Read_Ncol(path_to_data + 'data_weighted{}.csv'.format(ideology), names = True, directed = True, weights = True)

# Compute node betweennesses in aggregated network
btw_agg_ = g_weighted.betweenness(weights=g_weighted.es['weight'])

# Store betweennesses in dictionary
btw_agg = {}

for node, btw in zip(g_weighted.vs(), btw_agg_):
    node['btw'] = btw
    btw_agg[node['name']] = btw

# Sort betweennesses by value
btw_agg = {k: v for k, v in sorted(btw_agg.items(), reverse=True, key=lambda item: item[1])}
btw_agg

# Compute betweenness as percentage of theoretically possible paths in aggregated network
# 'theoretically possible': all nodes communicate at least indirectly with each other

# Number of theoretically possible paths
agg_theor_count = (g_weighted.vcount())**2 - g_weighted.vcount()
print('Number of theoretically possible paths in aggregated network: ',  agg_theor_count)

# Betweenness as percentage of theoretically possible paths
btw_agg_perc = {}

for k,v in btw_agg.items():
    btw_agg_perc[k] = (v/agg_theor_count)*100

# Display betweenness values as percentage of theoretical possible paths
btw_agg_perc

# Prepare betweenness data for plotting ----------------------------------------------
# Store betweenness scores in dataframe
df = pd.DataFrame(list(zip(list(btw_path_perc.keys()), list(btw_path_perc.values()), \
                           list(btw_agg_perc.keys()), list(btw_agg_perc.values()))),
               columns =['paths_writer_id', 'paths_btw', 'agg_writer_id', 'agg_btw'])

df['paths_writer_id'] = df['paths_writer_id'].astype('int64')
df['agg_writer_id']   = df['agg_writer_id'].astype('int64')
display(df.head())

nodes = nodes[['person_id', 'last_name']]
display(nodes.head())

# Combine betweenness dataframe with nodes dataframe to assign writers' names to betweenness scores
df = df.merge(nodes, how='inner', left_on='paths_writer_id', right_on='person_id')
df.rename(columns={'last_name':'paths_writer_name'}, inplace=True)
df = df[['paths_writer_id', 'paths_writer_name', 'paths_btw', 'agg_writer_id', 'agg_btw']]
df = df.merge(nodes, how='inner', left_on='agg_writer_id', right_on='person_id')
df.rename(columns={'last_name':'agg_writer_name'}, inplace=True)
df = df[['paths_writer_id', 'paths_writer_name', 'paths_btw', 
         'agg_writer_id', 'agg_writer_name', 'agg_btw']]
df.head()

# Plot betweenness differences ------------------------------------------------------

# Parameters for plot
n_writers        = 10
font_color       = 'black'
facecolor        = 'white'
index = list(range(0,n_writers+1))
left_col_paths   = df['paths_btw'].loc[:n_writers]
right_col_agg    = df['agg_btw'].loc[:n_writers]
labelcolor       = 'black'
left_title_paths = 'Time-respecting paths'
right_title_agg  = 'Aggregated network'
colorSG          = '#A8322D'
customG          = '#218380'

fig, axes = plt.subplots(figsize=(13,7), facecolor=facecolor, ncols=2, sharey=True)

# Put here, otherwise there's a white gap between the subplots
fig.tight_layout()

# Plot the betweenness values
bar_paths = axes[0].barh(index, left_col_paths, align='center', color=colorSG, zorder=1)
axes[0].set_title(left_title_paths, fontsize=14, pad=15, color=colorSG)
bar_agg = axes[1].barh(index, right_col_agg, align='center', color=customG, zorder=1)
axes[1].set_title(right_title_agg, fontsize=14, pad=15, color=customG)

# Invert x-axis on the left plot for positive numbers
axes[0].invert_xaxis() 

# Show data from highest to lowest
plt.gca().invert_yaxis()
    
# Hide y-ticks
axes[0].set_yticklabels([])

# Customise x-axis: set values on x-axis
xticks_path = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35]
xticks_agg = list(range(0,26, 5))
axes[0].set_xticks(xticks_path)
axes[1].set_xticks(xticks_agg)
axes[0].set_xticklabels(xticks_path, fontsize=12)
axes[1].set_xticklabels(xticks_agg, fontsize=12)
axes[0].set_xlabel('Betweenness scores as percentage of number of theoretical paths', fontsize=14)
# Adjust x-axis label position 
axes[0].xaxis.set_label_coords(1, -.1)

# Label bars:
# Right side: agg betweenness
for rect, y in zip(bar_agg, index):
    height = y
    width = rect.get_width()
    
    if width > 10:
        axes[1].text(rect.get_x() + rect.get_width() / 2.0, height, df.iloc[y]['agg_writer_name'], 
                     ha='center', va='center',  color=labelcolor, weight='bold', fontsize=10)
    else:
        axes[1].text(rect.get_x() + rect.get_width() + 1, height, df.iloc[y]['agg_writer_name'], 
                     ha='left', va='center',  color=labelcolor, weight='bold', fontsize=10)

# Left side: path betweenness
for rect, y in zip(bar_paths, index):
    height = y
    width = rect.get_width()
    if width > 0.15:
        axes[0].text(rect.get_x() + rect.get_width() / 2.0, height, df.iloc[y]['paths_writer_name'], 
                     ha='center', va='center',  color=labelcolor, weight='bold', fontsize=10)
    else:
        axes[0].text(rect.get_x() + rect.get_width() + 0.01, height, df.iloc[y]['paths_writer_name'], 
                     ha='right', va='center',  color=labelcolor, weight='bold', fontsize=10)


# Close space on y-axis between 2 subplots
plt.subplots_adjust(wspace=0, top=0.85, bottom=0.1, left=0.18, right=0.95)

fig.savefig(path_to_results + 'fig_betweenness_paths_vs_agg{}.png'.format(ideology), dpi = 300, bbox_inches = "tight")
plt.show()

# --------------------------------------------------------------------------------------
# 4. Robustness check: Compute betweenness centrality of nodes for different values of
#    delta t and check their stability
# --------------------------------------------------------------------------------------

# Compute number of 'theoretically possible' paths in temporal network
# i.e., that all nodes communicate at least indirectly with each other

paths_theor_count = (temp_net.vcount())**2 - temp_net.vcount()
print('Number of theoretically possible paths in temporal network:', paths_theor_count)

# Read in path objects for different delta t, and For each path object compute betweenness of nodes
dfs = []

pbar = tqdm_note(total = len(delta_ts))

for delta_t in delta_ts:
    # Read in paths
    paths = pp.Paths.read_file(path_to_results + 'paths_deltaT_{}.ngram'.format(delta_t), separator = ',')
    
    # Compute node betweenness on time-respecting paths
    btw_path = pp.algorithms.centralities.betweenness(paths)

    # Sort betweennesses by value
    btw_path = {k: v for k, v in sorted(btw_path.items(), reverse=True, key=lambda item: item[1])}
    
    # Betweenness as percentage of theoretically possible paths
    btw_path_perc = {}
    for k,v in btw_path.items():
        btw_path_perc[k] = (v/paths_theor_count)*100

    # Store betweenness scores in data frame
    df = pd.DataFrame()

    # node ID
    df['writer'] = btw_path.keys()

    # absolute betweenness value
    df['abs_btw'] = btw_path.values()

    # percentage of betweenness of theoretically possible paths
    df['perc_btw'] = btw_path_perc.values()

    # rank writers based on betweenness score per delta t
    df['btw_rank'] = list(range(1, df.shape[0]+1))

    # Value of delta t used to compute time-respecting paths
    df['delta_t'] = [delta_t] * len(btw_path.keys())
    
    # Append all dataframes (one for each delta t) to a list
    dfs.append(df)
 
    pbar.update()
pbar.close()

# Merge individual dataframes
df_all = pd.concat(dfs)

# Inspect the betweenness dataframe
# writer  : node ID
# abs_btw : absolute betweenness value
# perc_btw: percentage of betweenness of theoretically possible paths
# btw_rank: rank writers based on betweenness score per delta t
# delta_t : value of delta t used to compute time-respecting paths
df_all.head()

# Save results
df_all.to_csv(path_to_results + 'betweenness_different_deltaT{}.csv'.format(ideology), index = False)

# --------------------------------------------------------------------------------------
# 5. Visualise betweenness scores of nodes for different values of delta t
# --------------------------------------------------------------------------------------

# Load betweenness data
btws  = pd.read_csv(path_to_results + 'betweenness_different_deltaT{}.csv'.format(ideology))
btws.tail()

# Combine betweenness scores and nodes dataframe
btws = btws.merge(nodes, left_on='writer', right_on='person_id')
btws = btws[['person_id', 'last_name', 'abs_btw', 'perc_btw', 'btw_rank', 'delta_t']]
btws.rename(columns={'person_id':'writer_id', 'last_name':'writer_last_name'}, inplace=True)

btws.head()

# Get top 10 ranked revolutionary writers
n_top_ranked = 10
top_sources = btws[btws["delta_t"] == btws["delta_t"].max()].nsmallest(n_top_ranked, "btw_rank")
top_sources

# Make bump chart: Plot rank of betweenness across several delta t
fig, ax = plt.subplots(figsize=(10, 5), subplot_kw=dict(ylim=(0.5, 0.5 + n_top_ranked)))

ax.xaxis.set_major_locator(ticker.MultipleLocator(7))
ax.yaxis.set_major_locator(ticker.MultipleLocator(1))

# add 2nd y-axis for writers' names
yax2 = ax.secondary_yaxis("right")
yax2.yaxis.set_major_locator(ticker.FixedLocator(top_sources["btw_rank"].to_list()))
yax2.yaxis.set_major_formatter(ticker.FixedFormatter(top_sources["writer_id"].to_list()))

# Plot the betweenness ranks per writer across values of delta t
for i, j in btws.groupby("writer_id"):
    ax.plot("delta_t", "btw_rank", "o-", data=j, mfc='w')

# Customize the axes
ax.invert_yaxis()
ax.set_xlabel(r"$\delta t$", fontsize=16, fontweight='bold')
#ax.set_xticks(ax.get_xticks()[0::7])
ax.set_ylabel("Rank of betweenness score", fontsize=16, fontweight='bold')
ax.grid(axis="x")
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
ax.tick_params(axis = 'both', labelsize = 14)
yax2.set_yticklabels(top_sources['writer_last_name'])
yax2.tick_params(axis = 'y', labelsize = 14)
yax2.set_ylabel('Writer', fontsize=16, fontweight='bold')

plt.tight_layout()
fig.savefig(path_to_results + 'fig_betweenness_scores_across_deltaT_bump_chart{}.png'.format(ideology), dpi=300, bbox_inches="tight")

# --------------------------------------------------------------------------------------
# 6. Robustness check: compute path length frequencies for different values of delta t
# --------------------------------------------------------------------------------------

# We check whether paths lengths occur equally often across  different values of delta t.
# 1) Read in path objects of different delta t
# 2) Per path object compute path frequencies
# 3) Store them in dataframe

dfs = []

pbar = tqdm_note(total = len(delta_ts))
for delta_t in delta_ts:
    paths = pp.Paths.read_file(path_to_results + 'paths_deltaT_{}.ngram'.format(delta_t), separator = ',')
    
    freqs_paths_counts = []
    path_lengths = []

    # Path length frequencies can be accessed via a pathpy function 
    for key, vals in paths.path_lengths().items():
        path_lengths.append(key)
        freqs_paths_counts.append(vals[1])

    # Each entry gives the number of paths of that length as the entry with the same index in path_lengths
    # E.g., freqs_paths_counts[0]=9 means that there are 9 paths of length 0 in the data
    freqs_paths_counts = [int(freq) for _,freq in sorted(zip(path_lengths, freqs_paths_counts))]
    
    # Each entry corresponds to a different path length
    path_lengths = sorted(path_lengths)
    
    # A path length occurs as often as it occurs in the data
    # E.g., [1, 1, 1, 2, 2, 2, 2, ...]: there are 3 paths of length 1 and 4 paths of length two in the data
    freqs_paths = [[path_length]*freq for path_length, freq in zip(path_lengths, freqs_paths_counts)]
    
    # Remove sublists
    freqs_paths = [item for sublist in freqs_paths for item in sublist]
    
    # Store results
    df = pd.DataFrame()
    df['path_lengths'] = freqs_paths
    df['delta_t'] = [delta_t]*len(freqs_paths)
    dfs.append(df)
    
    pbar.update()
pbar.close()

# Merge path frequencies from different delta Ts
data_all = pd.concat(dfs, ignore_index = True)

# Inspect data
data_all.head()

# save to file
data_all.to_csv(path_to_results + 'path_length_freqs_different_deltaT{}.csv'.format(ideology), index = False)

# --------------------------------------------------------------------------------------
# 7. Visualise path length frequency distribution for delta t = 49
# --------------------------------------------------------------------------------------

# Load path frequencies
path_freqs = pd.read_csv(path_to_results + 'path_length_freqs_different_deltaT{}.csv'.format(ideology))
path_freqs = path_freqs.loc[path_freqs['delta_t']==49]
path_freqs.head()

# Plot frequency distribution of longest pathlengths (i.e., subpaths excluded)
fig, ax = plt.subplots(figsize=(10,5))

colorSG = '#A8322D'
sns.histplot(data=path_freqs, x="path_lengths", stat="probability", kde=True, discrete=True, ax=ax, binwidth=1,
             color=colorSG, edgecolor=None,
             line_kws = {'linewidth':'2'})

# Customize Axes
ax.set_xticks([1] + list(range(10, 15, 10)))
ax.set_xticklabels([1] + list(range(10, 15, 10)), fontsize=14)
ax.set_xlim(0, 15)
ax.set_xlabel('Path lengths', fontsize=16, fontweight='bold')
ax.set_yticks(np.linspace(0, 0.5, 9))
ax.set_yticklabels(np.linspace(0, 50, 9), fontsize=14)
ax.set_ylabel('Percentage', fontsize=16, fontweight='bold')
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)

fig.tight_layout()
fig.savefig(path_to_results + 'fig_path_lengths_freqs_deltaT49{}.png'.format(ideology), dpi = 300, transparent = False, bbox_inches = 'tight')

plt.show()

# --------------------------------------------------------------------------------------
# 8. Compute significant path length (optimal order in multi-order model) in time-respecting paths
# --------------------------------------------------------------------------------------

# Rrobustness check: We check whether the same paths length remains significant  across 
# different values of delta t. Note that, in dynamic social networks, we often find that
# a second-order model is  sufficient to explain causal paths.  We attribute this to an
# inherent "one-step context memory" of humans, which means that depending on with whom
# we interacted last, we decide with whom to interact next. It is more difficult to 
# imagine similar mechanisms that introduce higher-order correlations in a distributed way.

# the larger max_order is, the longer the computation takes since more layers need to be
# computed for each delta t
max_order = 3 

optimal_orders = []

# Read in path objects for different delta t. For each path object, compute multi-order 
# network model and select optimal order. Store optimal orders in dataframe.
pbar = tqdm_note(total = len(delta_ts))
for delta_t in delta_ts:
    # Read in paths
    paths = pp.Paths.read_file(path_to_results + 'paths_deltaT_{}.ngram'.format(delta_t), separator=',')
    
    # Build multi-order model by stacking max_order higher-order models
    multi_order = pp.MultiOrderModel(paths, max_order = max_order)
    print(multi_order)
        
    # Model selection with maximum likelihood
    best_order = multi_order.estimate_order(paths)
    optimal_orders.append(best_order)
    
    d = multi_order.degrees_of_freedom(max_order=2) -  multi_order.degrees_of_freedom(max_order=1)
    x = - 2 * (multi_order.likelihood(paths, log=True, max_order=1) -  multi_order.likelihood(paths, log=True, max_order=2))
    p = 1 - chi2.cdf(x, d)
    
    print('p value of null hypothesis that data has maximum order 1 = {0}'.format(p))   
    print('Optimal order = ', multi_order.estimate_order(paths))
    print('Optimal maximum order K is {0}'.format(multi_order.estimate_order()))
    print('Significant path length is {}'.format(best_order))
    
    pbar.update()
pbar.close()

# Store optimal orders from multi-order models in dataframe
df = pd.DataFrame()
df['delta_t'] = delta_ts
df['optimal_order'] = optimal_orders
df['max_order'] = [max_order] * len(delta_ts)
df.tail()

# Save optimal orders to file
df.to_csv(path_to_results + 'MOM_optimal_order_different_deltaT_maxOrder3{}.csv'.format(ideology), index = False)

# --------------------------------------------------------------------------------------
# 9. Robustness check: visualise significant path lengths for different values of delta t
# --------------------------------------------------------------------------------------

# Read in optimal order data
df = pd.read_csv(path_to_results + 'MOM_optimal_order_different_deltaT_maxOrder3{}.csv'.format(ideology))
df.head(6)

# Plot optimal orders across delta ts
fig, ax = plt.subplots(figsize=(10,2))

colorSG = '#A8322D'
ax.scatter(df['delta_t'], df['optimal_order'], color = colorSG, s = 60)

ax.xaxis.set_major_locator(ticker.MultipleLocator(7))

# To what extent do significant path lengths change as a function of delta t?
# Axes
ax.set_xlabel(r'$\delta t$', fontsize=16, fontweight='bold')
ax.set_yticks(range(1, 3))
ax.set_yticklabels(range(1, 3), fontsize=14)
ax.set_ylim(0.75,2.25)
ax.set_ylabel('Optimal order', fontsize=16, fontweight='bold')
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)

plt.tight_layout()
fig.savefig(path_to_results + 'fig_optimal_order_across_deltaTs{}.png'.format(ideology), dpi = 300, bbox_inches = "tight")

plt.show()