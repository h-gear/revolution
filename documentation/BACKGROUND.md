# The H-GEAR project
The H-GEAR project is unraveling the intellectual tapestry of America's founders by using digital methods to chart shifts in the character and structure of political discourse during the American Revolution. The project is centered on a large database of time-stamped letters that were exchanged between seven so-called “founding fathers” and other key figures of that time. The ambition of the project is to understand how political discourse shifted during the Revolutionary era in the United States, to reveal which people had influence and power, and to discover where and how these changes spread.  
 
1) The <u>first phase</u> of the project involves content-based analysis with **word2vec-based Shico software**. We aim to trace how liberal and republican concepts and ideologies shifted in the period between 1725-1835, uncovering the subtle nuances in language usage that shaped political discourse. Given a small set of user-provided keywords (e.g., freedom, liberty, right), ShiCo gives insight into changes in the vocabulary used to denote the concept by examining the semantic relations between words in different years.  
 
2) The <u>second phase</u> employs the combination of **semi-supervised topic modeling** with *seededLDA* and **temporal social network analysis** with *pathpy* to establish links between the writers through time. Through the analysis of letters exchanged, the project seeks to identify the most influential figures and map the network that propagated these political ideologies.  
 
Fndings underline that **Thomas Jefferson** emerges as the central figure of this period – not surprising given his leading role in drafting the Declaration of Independence. However, as is common to historical network analyses, because the corpus of letters is biased with its focus on the founding fathers, the researchers acknowledge that the results are equally biased.  
 
With anticipation of the 250th anniversary of the Declaration of Independence in 2026, we believe interest in the American Revolution will garner even more attention.

<br>


# Research Questions
- How did the political discourse centered around the two concepts of **liberal** and **republican** ideologies develop during the Revolutionary era?
- Who initiated and drove the spread of these ideas on political ideology?
- How did these ideas on political ideologies spread accross the network of interconnected people?
- Can we identify specific moments in time and places where these shifts are particularly prominent?  

<br>

# Methods


## 1. Shifting concepts in time (shico)
 
Martinez-Ortiz et al. (2016) developed the ShiCo tool, a system for illustrating the evolving meanings of words over time. Based Kenter et al. (2015), this approach creates semantic spaces through the training of word2vec word embeddings across different time spans (e.g., 5 or 10 years each).

* First, an initial set of terms provided by the user serves as a seed. Words with high semantic similarity to this seed set are identified based on computed similarity values from word embeddings. A semantic graph is constructed using these terms, and central terms are determined with centrality measures.

* Subsequently, these central terms can become the seed set for the next iteration. In this step, the word lists generated in each iteration are combined to create the final word lists for user presentation.

The visualization includes several complementary graphs: a stream graph and a series of network graphs. The stream graph displays color-coded streams for each term, where stream sizes indicate the term's relative importance in a given period. Importance is measured either by term count or the sum of similarities to seed terms. The network graphs for each measured time interval depict the relationships between terms during that period.

For more information ,see:
* C. Martinez-Ortiz, T. Kenter, M. Wevers, P. Huijnen, J. Verheul, J. Van Eijnatten, M. During, A. Jatowt, J. Preiser-Kappeller, A. v. Den Bosch ¨ et al., “Design and implementation of shico: Visualising shifting concepts over time,” HistoInformatics 2016, vol. 1632, pp. 11–19, 2016.

* https://research-software-directory.org/projects/mining-shifting-concepts-through-time-shico

* https://github.com/NLeSC/ShiCo

* link to Erik's document

## 2. Social network analysis

### 2a. Semi-supervised topic analysis

### 2b. Path-based temporal network
[*Pathpy*](https://www.pathpy.net/0.0.2-dev/) is an open source python package used for network analytics for time series data as part of complex temporal graphs. Time-stamped correspondence data like we used in this project is well suited to be analyzed with pathpy. It allows to calculate paths in dynamic social networks. 

In the current project, a *path* represents a chain of letters sent between people that acounts for the order of the sending dates while at the same allowing to set a maximum time difference between dates which can be used as a cutoff (i.e., when leters are no longer assumed to be related to each other).

For more information, see:

* rr
* rr