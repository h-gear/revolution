# Historiographing the Era of the American Revolution <img src="documentation/img/logo.jpg" width="120" align="right" />
=====================================================

## Acronym: HGEAR

GOAL
The goal of the HGEAR project is to use digital methods to chart shifts in the character and
structure of political discourse during the era of the American Revolution. By comparing shifts
in language usage (via ShiCo: Shifting Concepts Through Time) with changing formations in 
correspondence networks, we are able to measure how networks of communication within the 
Founders Online corpus shaped the character, distribution, and spread of political ideas 
during the Revolutionary era. This project and associated analyses offer powerful new methods
for addressing longstanding debates in the field of early American history. Also, by 
means of additional collected data (i.e., the Letters of Delegates to Congress), we show how the 
here included scripts and analyses can be easily accomodated to include other letter
correspondence sources as well. Ultimately, this allows researchers to get a more complete picture
of the fluxes in the political discourse during the American Revolution.

KEYWORDS
ShiCo, word vectors, natural language processing, text analysis, social network analysis

RESEARCH QUESTIONS
- How did the political discourse centered around the two concepts of liberal and republican ideologies develop during the Revolutionary era?
- Who initiated/ drove the spread of these ideas on political ideology?
- How did these ideas on political ideologies spread accross the network of interconnected people?
- What was the role of each of the founding fathers in terms of how they spread information?
- Can we identify specific moments in time and places where these shifts are particularly prominent?  


## Project Organization

    ├── LICENSE
    ├── README.md           <- The top-level README for developers and users using this project.
    ├── data
    │   ├── external        <- Data from third party sources.
    │   ├── interim         <- Intermediate data that has been transformed.
    │   ├── processed       <- The final, canonical data sets for modeling.
    │   └── raw             <- The original, immutable data dump.
    │
    ├── models              <- word2vec models to be used with shico software.
    │   ├── shico_delegates <- Delegates of Congress.
    │   ├── shico_ecco      <- Eighteenth Century Collections Online.
    │   ├── shico_evans     <- Evans Early American Imprints.
    │   └── shico_founders  <- Founders Online.
    │
    ├── notebooks           <- Jupyter and/ or Rmd notebooks.
    │
    ├── documentation       <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── output              <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures         <- Generated graphics and figures to be used in reporting.
    │   ├── logs            <- Place for logs.
    │   ├── tables          <- Location for documents describing results of data exploration, including tables.
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment
    │
    ├── scripts            <- Source code for use in this project.
    │   ├── 1. data_collection           <- Including scraping data.
    │   ├── 2. data_preparation          <- Data cleaning and preprocessing to enhance the data quality by organizing raw data in a suitable format.
    │   ├── 3. exploratory_data_analysis <- Getting a first and deeper understanding of the data.
    │   └── 4. analysis_and_modeling     <- Data analysis and modeling.
    │   └── 5. deployment                <- Shiny Application.
    │   └── 6. functions                 <- Applied accross the project and sourced in the code.
    │
    ├── tests               <- Test files (for functions)
   

The project is organized according to a data science structure covering all the phases from data acquisition to deployment: 

<img src="documentation/img/Data science lifecycle overview.jpg" />

The project integrates (dynamic) topic analysis, word2vec-based shico results, and temporal social network analysis, in addition
to providing geographical insights into the spreading of ideas and the combination of different datasources.  

<img src="documentation/img/hgear overview2.jpg" />
 
## Authors

- [Thijs Vroegh](https://www.esciencecenter.nl/)
- [Erik Tjong Kim Sang](https://www.esciencecenter.nl/)

## Impact and citation

This code has been used in the following research article:

- Vroegh, T., Tjong Kim Sang, E.,Olthof, J., & Thompson, M.L. (in progress). Examining drivers and dynamics of liberal and republican thought during the American Revolution.


Please cite if you use it. You can do this easily with one of these options:

- The `cite this repository` menu in the right tab.
- The [citation file](./CITATION.cff).
- If you prefer to copypaste, here you have the APA and BibTex strings:

`Vroegh, T., & Tjong Kim Sang, E. h-gear [Computer software]. https://github.com/h-gear/revolution`

---

`@software{Vroegh_hgear,author = {Vroegh, Thijs and Tjong Kim Sang, Erik},license = {Apache},title = {{h-gear}},url = {https://github.com/h-gear/revolution}}`


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

## Prerequisites

What things you need to install the software and how to install them

```
Give examples
```

## Setup and Installing

Here, we give a step by step instructions that tell you how to get you up and running.
The code is written in R and Python. For R, [*RStudio*](https://www.rstudio.com/) is recommended, while for Python, we
recommend anaconda.

First, Create a Conda/virtual environment.

```
Give the example
```

And repeat

```
until finished
```

## Little demo

End with an example of getting some data out of the system or using it for a little demo


## Running the tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Contributing

Please submit pull requests to us in case you want to participate.

## Versioning



## License

This project is licensed under the Apache License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments
* Ramona Roller
* Cristian A. Marocico
* Thomas Hoekstra
* Finn-Ole Höner

## Other valuable links:
* Jay Jimm
* People of the founding era: https://pfe.upress.virginia.edu/
* Thomas Hoekstra github
