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

# Need to add checks and errors.

createReportFolder <- function(Folder = getwd(), ProjectName)
  {
  outputDir <- file.path(Folder,ProjectName) 
  dir.create(path = outputDir)
  
  file.copy(from = system.file("extdata", "ExampleFEMOReport.Rmd", package = "rFEMO"),to = outputDir)
  file.rename(from = file.path(outputDir,"ExampleFEMOReport.Rmd"),to = file.path(outputDir,paste(ProjectName,"_","Report",".Rmd",sep = "")))
  
  dir.create(path = file.path(outputDir,"Data"))
  dir.create(path = file.path(outputDir,"Data/GPS"))
  dir.create(path = file.path(outputDir,"Data/Photopoints"))
  dir.create(path = file.path(outputDir,"Data/Weather"))
  }
