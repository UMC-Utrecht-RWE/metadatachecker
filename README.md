
# metadatachecker

<!-- badges: start -->
<!-- badges: end -->

RWE-metadatachecker is an R package with a Shiny app for meta data file comparison to check the consistencies. The goal is to diagnose the inconsistencies between the meta data files before running the analytical pipeline.

## Installation

You can install the development version of metadatachecker like so:

**Option 1.** Using devtools (recommended):

Download or clone the repo and:
``` r
# If you don't have devtools, install it
install.packages("devtools")
#Install metadatachecker (with vignettes) by typing this on R console
devtools::install("<directory of the package>", dependencies = TRUE, build_vignettes = T)
```

**Option 2.** Building from source:

Download or clone the repo, then open RStudio>Open Project, select metedatachecker folder. 
From top right panel of RStudio, select "Build" tab and press Install. If you encounter with
an error related with a missing library, install that library. These are the 
needed libraries for metadatachecker:

- kableExtra (>= 1.3.4),
- knitr (>= 1.43),
- rmarkdown (>= 2.23),
- shiny (>= 1.7.5),
- shinythemes (>= 1.2.0)


## Example

Here is how you can use metadatachecker

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
You need to select and upload the correct files (no error checking is performed yet). After pressing "Check consistency" button, a data table will appear and its final column will show the variables that are not present in the combined file.

- Second tab is for comparing any metadata file's selected column with study variables' "VarName" column. This is to check the consistency of meta data files with study variables file. After selecting the column name of the meta data file, a data table will appear and its final column will show the variables that are not present in the study variables file. Again, an error check for file selection is not performed yet (too much trust in user :P)

- Third tab is for generating an html report for all the comparison performed in first and second tabs. The results are combined and a downloadable html file is generated. Last selected column is taken into account for the same meta data file.

It is also possible to use check_study_variables function to compare two files, without launching the Shiny app. For example:

``` r
library(metadatachecker)
check_study_variables("Documents/study_var.csv","VarName",
"Documents/metadata_file_example.csv",
"CONCEPT")
```

Above code will return a list of variables that are present in "metadata_file_example.csv", but not present in "study_vars.csv" 
