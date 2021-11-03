# Description
This repository contains the code to replicate the results and tables for paper "Does Self-Admitted Technical Debt impact Code-level Technical Debt? An Empirical Evaluation". It does not contain the necessary data, which needs to be downloaded separately.

The project is written completely in R, and requires following packages: dplyr, data.table, reshape2, stringr, Hmisc, and lme4.

## How to use

### Downloading the necessary data files
To use this code, you need to download the data needed for it. The link to the data is hosted in Figshare (DOI: https://doi.org/10.6084/m9.figshare.16922824), and it is packed in zip-format.

After downloading the data, unzip it to the main folder of the project, where it will create
folder "data" containing folders "comments" and "tdd_10".

The "comments" folder includes the paired data either on file or commit level including
the data from SonarQube's Issues. The folder "tdd_10" includes the data from Sonar measures
including Sqale Index, and the two Remediation Efforts.

### Running the code
Running the code in R should be self-explanatory. Just start the main_script.R and follow the instructions. Extra comments hae been added to some sections to notify which part of the paper that specific pieve of code refers to.

## Citing
If you are using this script or data, please cite the following paper:

Leevi Rantala, Valentina Lenarduzzi, Mika V. Mäntylä. Does Self-Admitted Technical Debt impact Code-level Technical Debt? An Empirical Evaluation.

