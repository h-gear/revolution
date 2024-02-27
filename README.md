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


## About the project
The goal of the HGEAR project is to chart shifts in the character and structure of political discourse during the era of the American Revolution. By comparing shifts
in language usage (via [*ShiCo*](https://github.com/NLeSC/ShiCo/) (Shifting Concepts Through Time) with changing formations in 
correspondence networks, we are able to measure how networks of communication within the [*Founders Online*](https://founders.archives.gov/) corpus and other resources such as the [*Letters of Delegates to Congress*](https://memory.loc.gov/ammem/amlaw/lwdg.html) shaped the character, distribution, and spread of political ideas 
during the Revolutionary era. This project and associated analyses offer powerful new methods
for addressing longstanding debates in the field of early American history. Also, by 
means of additional collected data, we show how the 
here included scripts and analyses can be easily accomodated to include other letter
correspondence sources as well. Ultimately, this allows researchers to get a more complete picture
of the fluxes in the political discourse during the American Revolution.


### Documentation
Here's the link to learn more about the project's [background](BACKGROUND.md).

## Project Organization

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
   
The project is organized according to a data science structure covering all the phases from data acquisition to deployment:

<img src="documentation/img/Data science lifecycle overview.jpg" />



## Getting Started

### Prerequisites
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

To install hgear from this GitHub repository, do:

```console
git clone git@github.com:https://github.com/h-gear/revolution.git
```

### Installation
Here, we give a step by step instructions that tell you how to get you up and running.
The code is written mostly in R and parts in Python. For R , [*RStudio*](https://posit.co/download/rstudio-desktop/) is recommended, while for Python, we recommend [*anaconda*](https://www.anaconda.com/download).


#### **R: Setting up the Virtual Environment with venv**
To ensure a consistent and reproducible environment, this project uses the venv package in R. Follow these steps to set up the virtual environment and install the necessary packages:

_Navigate to the Project Directory:_
```
cd revolution
```

_Run the Setup Script:_

Execute the provided setup script (setup.bat) to automatically create and activate the virtual environment, and install the necessary packages:
```
setup.bat
```
This script will install the renv package, create and activate the virtual environment, and install the required R packages specified in renv.lock. 

By running the provided setup script, Windows users can easily set up the required environment without worrying about manually installing packages. This approach streamlines the process and ensures that users have the correct dependencies in place.

Note: Ensure that the script is executed with administrative privileges. If prompted, right-click on setup.bat and choose "Run as administrator."

_Run Your Code:_

After the setup script completes, you are ready to run the code within the virtual environment. Execute your R scripts or run your Rmarkdown documents as usual.

_Deactivate the Virtual Environment:_

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
```
pip install -r requirements.txt
```
_Deactivate the Virtual Environment:_
When you are done, deactivate the virtual environment:

```
deactivate
```

## Data
This project is based on four datasets:
- Ecco
- Evans
- Founders Online
- Delegates of Congress

Further information on these data sources can be found here:
* Founders Online: https://founders.archives.gov/
* Letters of Delegates to Congress: https://memory.loc.gov/ammem/amlaw/lwdg.html
* Eighteenth Century Collections Online (ECCO) TCP: https://textcreationpartnership.org/tcp-texts/ecco-tcp-eighteenth-century-collections-online/
* Evans Early American Imprints (Evans) TCP: https://textcreationpartnership.org/tcp-texts/evans-tcp-evans-early-american-imprints/

The corresponding data can be retrieved here: 
* Founders Online: https://github.com/jaytimm/founders-online-corpus
* Letters of Delegates of Congress: in this repository 
* Ecco: https://www.dropbox.com/sh/inhwjphw682i2gf/AAC8NixNye8Gp0smYBTly2Y9a?dl=0
* Evans: https://graphics.cs.wisc.edu/WP/vep/vep-tcp-collection/

<!-- USAGE EXAMPLES -->
## Usage
The code for the analyses can be found in the subfolder **scripts**. It is structured in parallel to the structure of analysis as explained above:

* 1_data_collection
* 2_data_preparation
* 3_exploratory_data_analysis
* 4_analysis_and_modeling
* 5_deployment
* functions (# applied accross the project and sourced in the code)


## Contributing
If you want to contribute to the development of hgear, have a look at the [contribution guidelines](CONTRIBUTING.md).
Please submit pull requests to us in case you want to participate. Suggestions, improvements, and edits are most welcome!

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
This code is being developed by the Netherlands eScience Center in collaboration with the faculty of of American Studies at the University of Groningen. Our thanks go out to the authors that shared or open-sourced their code, enabling us to create this project that can hopefully be of service to many. Particularly, we like to thank the following people:
* Cristian Marocico
* Thomas Hoekstra
* Cristian A. Marocico
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