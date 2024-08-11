# Installation

![Rev](../logo.jpg)

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

To install hgear from this GitHub repository, do:

```console
git clone git@github.com:https://github.com/h-gear/revolution.git
```

### Installation
Here, we give a step by step instructions that tell you how to get you up and running.
The code is written mostly in R and parts in Python. For R , [*RStudio*](https://posit.co/download/rstudio-desktop/) is recommended, while for Python, we recommend [*anaconda*](https://www.anaconda.com/download).


#### **Setting up the Virtual Environment for R**
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