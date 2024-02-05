@echo off

REM Install venv package
Rscript -e "install.packages('venv')"

REM Create and activate virtual environment
Rscript -e "library(venv); venv::create(); venv::activate()"

REM Install required packages
for /f %%i in (packages.txt) do (
  Rscript -e "library(venv); venv::install(%%i)"
)

echo Virtual environment setup complete. You can now run your R scripts.
