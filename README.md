# Match Getty AAT

Match Getty AAT is a prototype app that matches terms in a file to the Getty Art & Architecture Thesaurus using their Linked Open Data portal.

The app tries to find the best match by using a set of keywords included with each row, when available, to try to disambiguate the usage. For terms where many matches are found, the app allows the user to select the best one. Once the process is completed, the results file can be downloaded for further processing or importing to the CIS or other database.

This app was made by the Digitization Program Office, OCIO.

The AAT is queried using their Linked Open Data SPARQL endpoint: http://vocab.getty.edu/queries

![General information and instructions for the [input file](#input-file).](docs/AATScreen1.png)

## Steps and screenshots

### Step 1. Batch Matching of Subjects: Automated match based on the term and keywords

![The term in each row is matched to the AAT. If there are more than one matches, the user can select the best one in the next step.](docs/AATScreen2.png)

### Step 2. Manual Matching of Subjects: Select the appropiate match by executing a full text search

![Select the best AAT term for the row.](docs/AATScreen3.png)

### Step 3. Download Results: Download a CSV or Excel file with the rows from the input file with the match from the AAT

![Download the results file in CSV or Excel.](docs/AATScreen4.png)

## Input File

To use this app, upload a CSV or Excel file.

 * If the input file is a CSV file (.csv), the file must be comma-separated
 * If the input file is an Excel file (.xlsx), only the first sheet is used

The input file must be encoded using UTF-8 and have at least these 2 columns:

 * id - ID for the row
 * term - term to match to the AAT

Two columns are optional:

 * keywords - words or phrases to filter matching terms
 * linked_aat_term - AAT term from previous efforts

Any other columns in the input file will be ignored but returned in the results file.

## Requirements to run the app

Install the required packages:

install.packages(c("shiny", "DT", "dplyr", "stringr", "jsonlite", "futile.logger", "RSQLite", "shinyWidgets", "shinycssloaders", "WriteXLS", "openxlsx", "tokenizers", "stopwords"))
