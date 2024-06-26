
# metadatachecker

<!-- badges: start -->
<!-- badges: end -->

RWE-metadatachecker is an R package with a Shiny app for meta data file comparison to check the consistencies. The goal is to diagnose the inconsistencies between the meta data files before running the analytical pipeline.

## Installation

You can install the development version of metadatachecker (or the latest stable release) like so:

**Option 1.** Using devtools (recommended):

Download and unzip (in your working enviroment) or clone the repo. Let's say your folder is here:

- on Mac, the *"path_to_the_directory_of_the_package"* looks something like this: "/Users/zkurkcuo/Desktop/metadatachecker"
- on Windows, the *"path_to_the_directory_of_the_package"* looks something like this: "C:/Users/zkurkcuo/Desktop/metadatachecker" (it should be **"/"**, and not "\\")

and on R console, type the following:
``` r
# If you don't have devtools, install it
install.packages("devtools")
#Install metadatachecker (with vignettes) by typing this on R console
devtools::install("path_to_the_directory_of_the_package", dependencies = TRUE, build_vignettes = T)
```
If it asks about updating the packages that are available in your system, I usually skip it and hope that I won't break anything. So far it worked, but please check it.

**Option 2.** Building from source (not recommendend, but if you really have to, then it is in the following way):

Download or clone the repo, then open RStudio>Open Project, select metedatachecker folder. 
From top right panel of RStudio, select "Build" tab and press Install. If you encounter with
an error related with a missing library, install that library. These are the 
needed libraries for metadatachecker:

- kableExtra (>= 1.3.4),
- knitr (>= 1.43),
- rmarkdown (>= 2.23),
- shiny (>= 1.7.5),
- shinythemes (>= 1.2.0)
- shinyvalidate (>= 0.1.3)

## Example

Here is how you can use metadatachecker: 


On R console, type the following:
``` r
library(metadatachecker)
```

After importing the library, write the following on the console:

``` r
metadatachecker_app()
```

A Shiny app will launch in a new window or in your default browser (if you set it as "Run External" from the menu next to "Run app"). 

In the app:

- First tab is for comparing study variables "VarName" with the variables names resulting from the combination of events/drugs/vaccines/algorithms(NEWCONCEPT)/additional concepts. The aim is to check the consistency of the study variables file. 
You need to select and upload the correct files. After pressing "Check consistency" button:

  - If uploaded files are not in the expected format, an error message 
will appear. You need to upload the correct file with expected delimiter and column names.
After uploading the correct file(s), press on the button again.

  - If the files are in correct format, a data table will appear and its final column 
will show the variables that are not present in the combined file.

- Second tab is for comparing any metadata file's selected column with study variables' "VarName" column. This is to check the consistency of meta data files with study variables file. After selecting the column name of the meta data file, a data table will appear and its final column will show the variables that are not present in the study variables file. Here, an error check for file selection is not performed yet (too much trust in user :P), and if the files are in wrong format, the app will crash.

- Third tab is for generating an html report for all the comparison performed in first and second tabs. The results are combined and a downloadable html file is generated. Last selected column is taken into account for the same meta data file.

It is also possible to use check_study_variables function to compare two files, without launching the Shiny app. For example:

``` r
library(metadatachecker)
check_study_variables("Documents/study_var.csv","VarName",
"Documents/metadata_file_example.csv",
"CONCEPT")
```

Above code will return a list of variables that are present in "metadata_file_example.csv", but not present in "study_vars.csv" 
