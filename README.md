# Historiographing the Era of the American Revolution

<!--[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)-->


[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Apache License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
      <img src="documentation/img/logo.jpg" alt="Logo" 
      width="400" height="280">

<br />
<br />
<div align="left">

<!-- TABLE OF CONTENTS -->
<details open>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#documentation">Documentation</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#data">Data</a></li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#other-sources">Sources</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#authors">Authors</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>


## About the Project

The historical concept of democracy, liberry and freedom and its development through time is important to our understanding of the past. The goal of the HGEAR project is to chart shifts in the character and structure of political discourse during the era of the American Revolution. By comparing shifts
in language usage (via [**ShiCo**](https://github.com/h-gear/ShiCo/) *(Shifting Concepts Through Time*) with changing formations in 
correspondence networks, we are able to measure how networks of communication within the [**Founders Online**](https://founders.archives.gov/) corpus and other resources such as the [**Letters of Delegates to Congress**](https://memory.loc.gov/ammem/amlaw/lwdg.html) shaped the character, distribution, and spread of political ideas 
during the Revolutionary era. This project and associated analyses offer powerful new methods
for addressing longstanding debates in the field of early American history. Also, by 
means of additional collected data, we show how the 
here included scripts and analyses can be easily accomodated to include other letter
correspondence sources as well. Ultimately, this allows researchers to get a more complete picture
of the fluxes in the political discourse during the American Revolution.

The project is organized as follows:

<img src="documentation/img/Data science lifecycle overview.jpg" />

## Documentation
Here's the link to learn more about the [**project's background**](documentation/BACKGROUND.md). 

## Repository Structure

    ├── LICENSE
    ├── README.md           <- The top-level README for developers and users using this project.
    ├── data
    │   ├── external        <- Data from third party sources.
    │   ├── interim         <- Intermediate data that has been transformed.
    │   ├── processed       <- The final, canonical data sets for modeling.
    │   └── raw             <- The original, immutable data dump.
    │
    ├── w2v_models          <- word2vec models to be used with shico software.
    │   ├── delegates       <- Delegates of Congress.
    │   ├── ecco            <- Eighteenth Century Collections Online.
    │   ├── evans           <- Evans Early American Imprints.
    │   └── founders        <- Founders Online.
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
    │   ├── 2. data_preparation          <- Data cleaning/preprocessing to enhance the data quality by organizing raw data in a suitable format.
    │   ├── 3. exploratory_data_analysis <- Getting a first and deeper understanding of the data.
    │   └── 4. analysis_and_modeling     <- Data analysis and modeling.
    │   └── 5. deployment                <- Geographical analysis.
    │   └── functions                    <- Applied accross the project and sourced in the code.
    │
    ├── notebooks               <- Example of analysis)
   

## Getting Started

### Prerequisites
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

To install hgear from this GitHub repository, go to a directory where you want to install your files. From there, go to a command prompt in Windows (e.g., powershell), for example by right-clicking the mouse and select 'Open in terminal'. Next, paste the following after the prompt:

```console
git clone git@github.com:h-gear/revolution.git
```
This will download all the files from github

### Installation
Next, we give a step by step instructions that tell you how to get you up and running.
The code is written mostly in R and parts in Python. For R , [*RStudio*](https://posit.co/download/rstudio-desktop/) is recommended, while for Python, we recommend [*anaconda*](https://www.anaconda.com/download).


#### **Setting up the Virtual Environment for R**
To ensure a consistent and reproducible environment, this project uses the venv package in R. The following will install the renv package, create and activate the virtual environment, and install the required R packages as specified in renv.lock. 

_1 Navigate to the project directory:_
```
cd revolution
```

_2 Install renv (if not already installed):_

Open R or RStudio and install renv:

```
install.packages("renv")
```

_3 Initialize or load renv:_

In the RStudio console, navigate to the project directory (<repository-directory>) and activate renv:

```
renv::activate()
```

_4 Restore the environment:_

Restore the project environment using the renv.lock file:

```
renv::restore()
```
This will:

- Automatically download and install all required packages as specified in the renv.lock file.
- Use the appropriate versions for compatibility.

_5 Verify setup:_

After restoration, check for any missing packages using:

```
renv::status()
```
Test whether the installtion is succesful by loading some key packages to ensure the environment works as expected:
```
library(dplyr)  # Replace with any other package name you just installed
```

_6 Run the project:_

Once renv is fully set up, you are ready to run the code within the virtual environment. Execute your R scripts or run your Rmarkdown documents as usual.


_7 Deactivate the Virtual Environment:_

When you are done, deactivate the virtual environment:
```
venv::deactivate()
```

#### **Python: Setting up a Virtual Environment**
We recommend installing hgear in a new virtual environment to avoid dependency conflicts.

Run the following commands to create a virtual environment:
For _Windows_:
```
python -m venv ./.venv
```

For _Unix or MacOS_:
```
python3 -m venv ./.venv
```

_Activate the Virtual Environment:_
For _Windows_:
```
.venv\Scripts\activate.bat
```

For _Unix or MacOS_:
```
source .venv/bin/activate
```

_Install Dependencies:_
Once the virtual environment is created and activated you can install the dependencies by running:
```
pip install -r requirements.txt
```
At this stage, you should be able to run the scripts.

_Deactivate the Virtual Environment:_
When you are done, deactivate the virtual environment:

```
deactivate
```

## Data
If you are interested in learning more about the data used in the project, then go [**here**](documentation/DATA.md).  

<!-- USAGE EXAMPLES -->
## Usage
The code for the analyses can be found in the subfolder **scripts**. It is structured in parallel to the structure explained above:

* [1_data_collection](https://github.com/h-gear/revolution/tree/main/scripts/1_data_collection)
* [2_data_preparation](https://github.com/h-gear/revolution/tree/main/scripts/2_data_preparation)
* [3_exploratory_data_analysis](https://github.com/h-gear/revolution/tree/main/scripts/3_exploratory_data_analysis)
* [4_analysis_and_modeling](https://github.com/h-gear/revolution/tree/main/scripts/4_analysis_and_modeling)
* [5_deployment](https://github.com/h-gear/revolution/tree/main/scripts/5_deployment)
* [functions](https://github.com/h-gear/revolution/tree/main/scripts/functions) (applied accross the project and sourced in the code)

In adition, the code and procedures to create the word2vec models needed as input for running shico 
can be found in the subfolder **w2v_models/delegates**. The code is applied to the data from the delegates of congress, but can be easily adapted and eqaully applied to create word2vec models for all other datasets in this project as well.


## Contributing
If you want to contribute to the development of hgear, have a look at the [contribution guidelines](CONTRIBUTING.md).Please submit pull requests to us in case you want to participate. Suggestions, improvements, and edits are most welcome!

## License
This project is licensed under the Apache License 2.0 - see the [licence](LICENSE.md) file for details. This means that this project can be used, modified and redistributed for free, even for commercial purposes.

## Other sources

#### Founders Online
* Jay Timm: https://github.com/jaytimm/founders-online-corpus
* People of the founding era: https://pfe.upress.virginia.edu/
* Thomas Hoekstra: https://github.com/Rohrym/letters_of_the_revolution_project

#### Shifting Concepts in Time (Shico)
* Shico: https://github.com/NLeSC/ShiCo
* Shico deployment: https://github.com/c-martinez/ShiCo-deploy
* Shico updated for Python 3: https://github.com/h-gear/ShiCo/

## Contact
If you find any bugs or experience difficulties when using the code in this project, please create a issue on this Github page. If you have any specific questions with respect to our research, or methodology used, please email Thijs Vroegh t.vroegh@esciencecenter.nl

## Authors
- [Thijs Vroegh](https://www.esciencecenter.nl/team/thijs-vroegh/)

## Impact and citation
This code has been used in the upcoming research article:

- Vroegh, T., Tjong Kim Sang, E.,Olthof, J., & Thompson, M.L. (in progress). Drivers and dynamics of liberal and republican thought: Social Networks in the American Revolution.

Please cite if you use it. You can do this easily with one of these options:

- The `cite this repository` menu in the right tab.
- The [citation file](CITATION.cff).
- If you prefer to copypaste, here you have the APA and BibTex strings:

`Vroegh, T. h-gear [Computer software]. https://github.com/h-gear/revolution`

---

`@software{Vroegh_hgear,author = {Vroegh, Thijs},license = {Apache 2.0},title = {{h-gear}},url = {https://github.com/h-gear/revolution}}`

## Acknowledgments
This code is being developed by the Netherlands eScience Center in collaboration with the faculty of American Studies at the University of Groningen. Our thanks go out to the authors that shared or open-sourced their code, enabling us to create this project that can hopefully be of service to many. Particularly, we like to thank the following people:
* Cristian Marocico
* Thomas Hoekstra
* Ramona Roller

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/h-gear/revolution.svg?style=for-the-badge
[contributors-url]: https://github.com/h-gear/revolution/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/h-gear/revolution.svg?style=for-the-badge
[forks-url]: https://github.com/h-gear/revolution/network/members
[stars-shield]: https://img.shields.io/github/stars/h-gear/revolution.svg?style=for-the-badge
[stars-url]: https://github.com/h-gear/revolution/stargazers
[issues-shield]: https://img.shields.io/github/issues/h-gear/revolution.svg?style=for-the-badge
[issues-url]: https://github.com/h-gear/revolution/issues
[license-shield]: https://img.shields.io/github/license/h-gear/revolution.svg?style=for-the-badge
[license-url]: https://github.com/h-gear/revolution/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/thijsvroegh
[product-screenshot]: images/screenshot.png
[Next.js]: https://img.shields.io/badge/next.js-000000?style=for-the-badge&logo=nextdotjs&logoColor=white
[Next-url]: https://nextjs.org/
[React.js]: https://img.shields.io/badge/React-20232A?style=for-the-badge&logo=react&logoColor=61DAFB
[React-url]: https://reactjs.org/
[Vue.js]: https://img.shields.io/badge/Vue.js-35495E?style=for-the-badge&logo=vuedotjs&logoColor=4FC08D
[Vue-url]: https://vuejs.org/
[Angular.io]: https://img.shields.io/badge/Angular-DD0031?style=for-the-badge&logo=angular&logoColor=white
[Angular-url]: https://angular.io/
[Svelte.dev]: https://img.shields.io/badge/Svelte-4A4A55?style=for-the-badge&logo=svelte&logoColor=FF3E00
[Svelte-url]: https://svelte.dev/
[Laravel.com]: https://img.shields.io/badge/Laravel-FF2D20?style=for-the-badge&logo=laravel&logoColor=white
[Laravel-url]: https://laravel.com
[Bootstrap.com]: https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white
[Bootstrap-url]: https://getbootstrap.com
[JQuery.com]: https://img.shields.io/badge/jQuery-0769AD?style=for-the-badge&logo=jquery&logoColor=white
[JQuery-url]: https://jquery.com 

<br><hr>
[🔼 Back to top](#about-the-project)
