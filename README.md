# Background
The inspiration behind tracking a dataset from the 2019 Yearbook of Immigration Statistics
(Department of Homeland Security) was from the Netflix documentary called “Immigration Nation”.
The documentary takes a deep look into recent U.S. immigration policies and its effect on
undocumented immigrants and their families. The 2020 documentary highlights everything related
to U.S. immigration, from the duties of an Immigration and Customs Enforcement (ICE) officer
to the criminalization of undocumented immigrants.

# Table of Contents
## Dataset
The following tables are taken from the [2019 Yearbook of Immigration Statistics](https://www.dhs.gov/immigration-statistics/yearbook/2019):
- [Table 33. Aliens Apprehended: Fiscal Years 1925 to 2019](https://www.dhs.gov/immigration-statistics/yearbook/2019/table33):
  - **Description**: The number of immigrants apprehended each year from ICE or the U.S. Border Patrol.
  - **CSV File**: [immigration_apprehended.csv](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/immigration_apprehended.csv)
- [Table 39. Aliens Removed or Returned: Fiscal Years 1892 to 2019](https://www.dhs.gov/immigration-statistics/yearbook/2019/table39)
  - **Description**: Lists the number of immigrants deported. This table has two columns: (1) immigrants deported based on an order of removal and
  (2) immigrants deported not based on an order of removal. What an order of removal means is undocumented immigrants that went through immigration
  court to make a plea and it wasn't approved.
  - **CSV File**: [immigration_deported.csv](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/immigration_deported.csv)

## Preliminary Analysis

This analysis was done in August 2020 right before starting the Big Data Analytics (BDA) Program at
SDSU. So I just began practicing with Python and Jupyter Notebooks. Both notebooks below are the same content.
- **Kaggle Notebook**: [Immigration Nation Analytics](https://www.kaggle.com/ekayfabio/immigration-nation-analytics)
- **IPython Notebook in GitHub**: [undocumented_immigration_project.ipynb](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/undocumented_immigration_project.ipynb)

## Multivariate Time Series Analysis
This analysis was done for MIS-748 Seminar in Applied Multivariate Analytics in the Spring 2021 semester.
- [immigration.R](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/immigration.R): Most updated R script containing time series plots, ARIMA models, forecasting,
intervention analysis, and impulse response plots.
- [immigration.ipynb](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/immigration.ipynb): An IPython notebook of the R script version. However,
this version is outdated. The notebook was created for easier visualization when meeting up with the professor about our preliminary analysis.
- [MIS 748 Proposal.docx](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/MIS%20748%20Proposal.docx): Project proposal listing the project summary and the datasets used.
- [MIS748_Final_Project_Report.pdf](https://github.com/ohkaaaaay/Immigration_Nation/blob/master/MIS748_Final_Project_Report.pdf): The detailed report on the multivariate
time series analysis of immigrants apprehended and deported.
- [Report_Latex Folder](https://github.com/ohkaaaaay/Immigration_Nation/tree/master/Report_Latex): The LaTex report source code.
