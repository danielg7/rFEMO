#' Create a folder structure for a FEMO report.
#' 
#' \code{createReportFolder} creates the barebones of a FEMO report
#' 
#' @param Folder A folder to store the report structure. Defaults to your working directory.
#' @param ProjectName The name of the project. This will be used to name the folder in which the structure is created.
#' @return Nested structures to ease data management.
#' @examples
#' 
#' createReportFolder(Folder = "FEMO_Reports/", ProjectName = "BearMountainFire")
#' 
#' @export

# TODO:
# Make dummy data copy an option.
# 

createReportFolder <- function(Folder = getwd(), ProjectName)
  {
  if(ProjectName == NULL) stop("You need to give a project name.")
  
  outputDir <- file.path(Folder,ProjectName) 
  
  if(file.exists(outputDir)) {stop("Your output directory already exists!")} 
 
  dir.create(path = outputDir)
  
  dir.create(path = file.path(outputDir,"Data"))
  dir.create(path = file.path(outputDir,"Data/GPS"))
  dir.create(path = file.path(outputDir,"Data/Photopoints"))
  dir.create(path = file.path(outputDir,"Data/Weather"))
  dir.create(path = file.path(outputDir,"Data/FirelineObservations"))
  
  file.copy(from = system.file("extdata", "ExampleFEMOReport.Rmd", package = "rFEMO"),
            to = outputDir)
  
  file.rename(from = file.path(outputDir,"ExampleFEMOReport.Rmd"),
              to = file.path(outputDir,paste(ProjectName,"_","Report",".Rmd",sep = "")))
  
  file.copy(from = system.file("extdata", "YYMMDD_FireName_FirelineWeatherObservations", package = "rFEMO"),
            to = file.path(outputDir,"Data/Weather"))
  
  file.rename(from = file.path(outputDir,"YYMMDD_FireName_FirelineWeatherObservations"),
              to = file.path(outputDir,paste(ProjectName,"_","Report",".Rmd",sep = "")))
  
  }
