# Key methods


## 1. Shifting concepts in time (shico)
 
Based on Kenter et al. (2015), Martinez-Ortiz et al. (2016) developed the ShiCo tool: a system for illustrating the evolving meanings of words over time and thus mapping the conceptual change of a concept (here: *liberal political ideology* and  *republican political ideology*). This approach creates semantic spaces through the training of word2vec word embeddings across different time spans (e.g., 5 or 10 year sliding windows).

* First, an initial set of terms provided by the user serves as a seed. Words with high semantic similarity to this seed set are identified based on computed similarity values from word embeddings. A semantic graph is constructed using these terms, and central terms are determined with centrality measures.

* Subsequently, these central terms can become the seed set for the next iteration. In this step, the word lists generated in each iteration are combined to create the final word lists for user presentation.

The visualization includes several complementary graphs: a stream graph and a series of network graphs. The stream graph displays color-coded streams for each term, where stream sizes indicate the term's relative importance in a given period. Importance is measured either by term count or the sum of similarities to seed terms. The network graphs for each measured time interval depict the relationships between terms during that period.

For more information ,see:
* Kenter, T., Wevers, M, Huijnen, P., and de Rijke, M.(2015). Ad hoc monitoring of vocabulary shifts over time. In: *Proceedings of the 24th ACM international Conference on Information and Knowledge Management (CIKM’15)*

* Martinez-Ortiz, C., Kenter, T., Wevers, M., Huijnen, P., Verheul, J., and van
Eijnatten, J. (2016). “Design and implementation of ShiCo: visualising shifting
concepts over time,” in *Proceedings of the 3th Histoinformatics Conference*, eds
M. During, A. Jatowt, A. van den Bosch, and J. Preiser-Kappeller (Krakow)

* https://research-software-directory.org/projects/mining-shifting-concepts-through-time-shico

* https://github.com/NLeSC/ShiCo

* link to Erik's document

## 2. Temporal social network analysis
To conduct a temporal social network analysis on our datasets (most notably the Founding Father dataset), we took a two-step approach. First, because of the heterogeneity of the data, a semi-supervised topic modeling approach allowed to filter those letters whose central topic were on liberal and/ or republican politics. Based on the obtained homogeneous subsets of data, we then proceeded with the network analyses.

### 2a. Semi-supervised topic analysis
We used [*Seeded LDA (Latent Dirichlet Allocation)*](https://koheiw.github.io/seededlda/index.html) to deductively identify the pre-defined political topics in the corpus of letters based on a number of central seed words derived from theory. In addition, we also allowed for unseeded topics to be present; its exact amount based on criteria such as the divergence metric, which maximizes when the chosen number of topic k is optimal. In addition, we used the [*keyATM*](https://keyatm.github.io/keyATM/index.html) package to explore if and how the prevalence of topics change over time.

### 2b. Path-based temporal network
[*Pathpy*](https://www.pathpy.net/0.0.2-dev/) is an open source python package used for network analytics for time series data as part of complex temporal graphs. Time-stamped correspondence data like we used in this project is well suited to be analyzed with pathpy. It allows to calculate paths in dynamic social networks.

In the current project, a *path* represents a chain of letters sent between people that acounts for the order of the sending dates while at the same allowing to set a maximum time difference between dates which can be used as a cutoff (i.e., when leters are no longer assumed to be related to each other).

Taken together, these tools have proven effective for identifying key themes within large bodies of correspondence letter data and then for charting temporal networks and pinpointing important actors within those networks. This project will offer the research community a powerful set of (improved) methods, tools, and data for the study of conceptual change and the transmission of ideas in historical texts and networks.

For more information, see:

* Hackl, J. et al.(2021). Analysis and visualisation of time series data on networks with pathpy. In Companion Proceedings of the Web Conference, WWW ’21, 530–532 (Association for Computing Machinery,New York, NY, USA)
* Scholtes, Ingo (2017). When is a network a network? Multi-order graphical model selection in pathways and temporal networks. In: *Proceedings of the 23rd ACM SIGKDD international conference on knowledge discovery and data mining*, 1037– 1046
* Scholtes, I., Wider, N. & Garas, A. (2016). Higher-order aggregate networks in the analysis of temporal networks:path structures and centralities. *The European Physical Journal B 89*, 61.
* Watanabe, K., & Baturo, A. (2023). Seeded Sequential LDA: A Semi-Supervised Algorithm for Topic-Specific Analysis of Sentences. *Social Science Computer Review*. https://doi.org/10.1177/08944393231178605

## 3. Animated wordcloud
An animated adaptation of the static wordcloud is designed here for the letter correspondence data. Unlike the conventional word cloud, which doesn't account for temporal changes in text data, the animated version addresses this limitation. More specifically, it builds further on the results from the topic analysis by visually depicting how the words as part of a selected topic change in frequency (i.e., informing on the change in relative importance of a word as part of a topic) through time. The animated worcloud can be used both within R (**animated_wc.R**) or accessed via a shiny app (**wordcloud_app.R**). Based on the derived topics underlying the Founders Online dataset, the shiny app allows to easily select a topic of interest (e.g., "02_Republican politics") from a menu, after which the animated wordcloud is shown.

This animated wordcloud serves as an alternative approach to the way Shico presents its findings. Simply put, whereas **shico** accounts for the *flexible* way in which a concept (e.g.,liberal politics) is denoted by showing a continously evolving wordset through time, the **animated wordcloud** is based on a *fixed* wordset derived from topic analysis, and then shows how the relative importance of each word within the fixed wordset changes over time. As such, the animated wordcloud is based on the topic-words frequency, providing a slighty different view on understanding the conceptual change of topics.