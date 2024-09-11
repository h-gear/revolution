# Contributing Guidelines

Thank you for considering contributing to our project! We appreciate any form of contribution, from asking questions to submitting bug reports or proposing new features. Please review our Code of Conduct before engaging with the project.

## Types of Contributions

### 1. Asking Questions
- Use the search functionality to check for existing issues.
- If no relevant results are found, create a new issue.
- Apply the "Question" label and other relevant labels as needed.

### 2. Reporting Bugs
- Search for existing issues related to the bug.
- If not found, create a new issue with sufficient details (including commit SHA, dependency info, and OS details).
- Apply relevant labels to the newly created issue.

### 3. Making Code Changes

**(Important)** Before making changes, announce your plan through a new issue.

1. Wait for community consensus on the proposed idea.
2. Fork the repository and create a feature branch.
3. Stay updated with the main branch by pulling changes.
4. Install dependencies.
5. Add new tests if necessary and update/expand documentation.
6. Push your feature branch to your fork.
7. Create a pull request.

If you feel you've made a valuable contribution but need help with tests or documentation, submit the pull request, and we'll assist you.

### 4. Providing Experience, Feedback, or Suggestions
- Use the search function to check for similar experiences or suggestions.
- Open a new issue to share your experience, provide feedback, or suggest improvements/features.

## Getting started with development

### Setup for R Development

To contribute to our project, you'll need to set up your development environment for R. Follow these steps:

1. **Install R and R Studio:**
   - Download and install R from [CRAN](https://cran.r-project.org/).
   - Download and install R Studio from [here](https://www.rstudio.com/products/rstudio/download/).

2. **Clone the Repository:**
   - Fork the repository to your GitHub account.
   - Clone the forked repository to your local machine:
     ```bash
     git clone https://github.com/your-username/repository.git
     ```

3. **Create a Virtual Environment for R:**
   - R projects often use `.Renviron` or `.Rprofile` for custom environment settings. Check the project's documentation for any specific configurations.
   - In R, the equivalent to Python's requirements.txt is typically a file named renv.lock. This file contains information about the dependencies required in a R project,
     including the versions of R and all installed packages.
   - Set up a virtual environment using R's `renv` package. Navigate to the project directory and run:
     ```R
     install.packages("renv")
     library(renv)
     renv::init()
     ```

 When you have cloned the project and run renv::init(), renv will use the information in the 'renv.lock' file to recreate the exact environment we used in the project.
   - Install project dependencies:
     ```R
     renv::install()
     ```

### Setup for Python Development

To contribute to our project in Python, follow these steps:

1. **Install Python:**
   - Download and install Python from [here](https://www.python.org/downloads/).

2. **Clone the Repository:**
   - Fork the repository to your GitHub account.
   - Clone the forked repository to your local machine:
     ```bash
     git clone https://github.com/your-username/repository.git
     ```

3. **Create a Virtual Environment for Python:**
   - Navigate to the project directory and create a virtual environment:
     ```bash
     python -m venv venv
     ```

   - Activate the virtual environment:
     - On Windows: `venv\Scripts\activate`
     - On Unix or MacOS: `source venv/bin/activate`

   - Install project dependencies:
     ```bash
     pip install -r requirements.txt
     ```


