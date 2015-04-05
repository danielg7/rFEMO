# About

This R package seeks to aid the production of fire effects monitoring reports through the use of standard tools and techniques. The aim is to create easy, streamlined, reproduceable reports and assist in data management.

# Installation

If you are entirely new to R, it's first necessary to install R and, for the sake of easing the learning curve, R Studio. Follow the instructions at [Getting Started With R](http://scs.math.yorku.ca/index.php/R:_Getting_started_with_R) to walk you through the basics. Once you have done that, make sure you have the `devtools` package installed (`install_packages("devtools")`) before moving forward.


```r
library("devtools")
install_github("danielg7/rFEMO", build=FALSE, dependencies=c("DEPENDS", "IMPORTS"))
```

# Usage

Basic workflow:

1. After installing, enable package (`library(rFEMO)`)
2. Set your working directory to where you want to create your project folder for your report. If you are using R Studio, you can do this using the Session -> Set Working Directory -> Choose Directory menu item. 
3. At the command prompt (`> `), type the command `createReportFolder(ProjectName = "YourProjectName")`, and replace `YourProjectName` with the name of the fire / project. If you want to save this folder elsewhere, you can use `createReportFolder(Folder = "C:/PathToYourIntendedDirectory/", ProjectName = "YourProjectName")`. This will create and populate a folder with a sample report document and a datasheet in `YourProjectName/Data/Weather`.
4. Populate the datasheet in `YourProjectName/Data/Weather`
5. Open the RMarkdown document in `YourProjectName/`. Make edits within this to reflect the activities and objectives of the project.
6. Make sure that the file pointers in the RMarkdown document point to your data.
7. Compile the document by typing `knit(input = "YourRMarkdownDocument.Rmd")`

# Development

## Current features:
* Easily generate wind vector charts interpreted from standard cardinal directions
* Generate standard folder structures and create report framework in R.

## Future developments:
* A vignette extensively documenting workflow and the resulting report.
* Make it easy to scrape from a standardized Google Sheet to allow quicker data entry
* Create functions to automatically create RH and Temperature plots
* Create options to automatically add prescription boundary parameters for graphs (i.e., RH < 25, WS > 20 mph, etc.)
* Make inclusion of photos easier.
* Write custom .css to make an easier to read report.
* Add mapping component including a KMZ output tool
* Make Ecological Metadata Language (EML) output tool to wrap FEMO/FOBS data in a convenient metadata.

