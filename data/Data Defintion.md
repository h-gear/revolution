# Data and Feature Definitions

This document provides a central hub for the raw data sources, the processed/transformed data, and feature sets. More details of each dataset is provided in the data summary report. 

For each data, an individual report describing the data schema, the meaning of each data field, and other information that is helpful for understanding the data is provided. If the dataset is the output of processing/transforming/feature engineering existing data set(s), the names of the input data sets, and the links to scripts that are used to conduct the operation are also provided. 

When applicable, the Interactive Data Exploration, Analysis, and Reporting (IDEAR) utility developed by Microsoft is applied to explore and visualize the data, and generate the data report. Instructions of how to use IDEAR can be found [here](). 

For each dataset, the links to the sample datasets in the _**Data**_ directory are also provided. 

_**For ease of modifying this report, placeholder links are included in this page, for example a link to dataset 1, but they are just placeholders pointing to a non-existent page. These should be modified to point to the actual location.**_

# Folder for hosting the main data sources, including background, links and download locations (if available). In addition, this folder holds relevant data for running the shico application.

This project is based on four datasets:
- Founders online: ca. 185.000 letters
- Delegates of Congress: ca. 15.000 letters
- EccoTCP: 3.101 books
- EvansTCP: 5.012 books


## Founders Online Corpus

internet adress:

[Founders Online](https://founders.archives.gov/)

Description:

Founders Online is a [National Archives](https://www.archives.gov/) resource that makes available \~180K writings/letters of the Founders of the United States of America. 
All documents have been extracted and are stored in a collection of R-based `rds` files. See also:
-   [RDS Files](https://github.com/jaytimm/founders_archive_corpus/tree/master/data).


## Delegates of Congress

internet adress: https://memory.loc.gov/ammem/amlaw/lwdg.html

Delegates Data Download: this guthub repository

Description:
- 
- 
- 


## Ecco

internet adress: https://textcreationpartnership.org/

Ecco Data Download: https://www.dropbox.com/sh/inhwjphw682i2gf/AAC8NixNye8Gp0smYBTly2Y9a?dl=0

Description:
- Eighteenth Century Collections Online TCP (ECCO-TCP).
- Full text of about 3,000 books available freely to everyone.
- period ca. 1700-1800.

All data is present in the folders, including: 
-	Rawd data
-	Data cleaning
-	Data preprocessing
-	W2v model computation )used in shico)


## TCP: Text Creation Partnership

internet adress: https://textcreationpartnership.org/

Evans Data Download:

Evans Early American Imprints TCP (Evans-TCP)
Full text of about 5,000 books available to everyone  [5000 early American texts]
Pre-1800 American imprints.




## Raw Data Sources


| Dataset Name | Original Location   | Destination Location  | Data Movement Tools / Scripts | Link to Report |
| ---:| ---: | ---: | ---: | -----: |
| Dataset 1 | Brief description of its orignal location | Brief description of its destination location | [script1.py](link/to/python/script/file/in/Code) | [Dataset 1 Report](link/to/report1)|
| Dataset 2 | Brief description of its orignal location | Brief description of its destination location | [script2.R](link/to/R/script/file/in/Code) | [Dataset 2 Report](link/to/report2)|


* Dataset1 summary. <Provide brief summary of the data, such as how to access the data. More detailed information should be in the Dataset1 Report.>
* Dataset2 summary. <Provide brief summary of the data, such as how to access the data. More detailed information should be in the Dataset2 Report.> 

## Processed Data
| Processed Dataset Name | Input Dataset(s)   | Data Processing Tools/Scripts | Link to Report |
| ---:| ---: | ---: | ---: | 
| Processed Dataset 1 | [Dataset1](link/to/dataset1/report), [Dataset2](link/to/dataset2/report) | [Python_Script1.py](link/to/python/script/file/in/Code) | [Processed Dataset 1 Report](link/to/report1)|
| Processed Dataset 2 | [Dataset2](link/to/dataset2/report) |[script2.R](link/to/R/script/file/in/Code) | [Processed Dataset 2 Report](link/to/report2)|

* Processed Data1 summary. <Provide brief summary of the processed data, such as why you want to process data in this way. More detailed information about the processed data should be in the Processed Data1 Report.>
* Processed Data2 summary. <Provide brief summary of the processed data, such as why you want to process data in this way. More detailed information about the processed data should be in the Processed Data2 Report.> 

## Feature Sets

| Feature Set Name | Input Dataset(s)   | Feature Engineering Tools/Scripts | Link to Report |
| ---:| ---: | ---: | ---: | 
| Feature Set 1 | [Dataset1](link/to/dataset1/report), [Processed Dataset2](link/to/dataset2/report) | [R_Script2.R](link/to/R/script/file/in/Code) | [Feature Set1 Report](link/to/report1)|
| Feature Set 2 | [Processed Dataset2](link/to/dataset2/report) |[SQL_Script2.sql](link/to/sql/script/file/in/Code) | [Feature Set2 Report](link/to/report2)|

* Feature Set1 summary. <Provide detailed description of the feature set, such as the meaning of each feature. More detailed information about the feature set should be in the Feature Set1 Report.>
* Feature Set2 summary. <Provide detailed description of the feature set, such as the meaning of each feature. More detailed information about the feature set should be in the Feature Set2 Report.> 
