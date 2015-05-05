#' Create a folder structure for a FEMO report.
#' 
#' \code{createReportFolder} creates the barebones of a FEMO report
#' 
#' @param Folder A folder to store the report structure. Defaults to your working directory.
#' @param ProjectName The name of the project.
#'        This will be used to name the folder in which the structure is created.
#' @param ProjectDate The data of the project in 'YYYYMMDD'
#' @return Nested structures to ease data management.
#' @examples
#' 
#' createReportFolder(Folder = "FEMO_Reports/", 
#'                    ProjectName = "BearMountainFire",
#'                    ProjectDate = 20150716)
#' 
#' @export


createReportFolder <- function(Folder = getwd(), ProjectName, ProjectDate)
  {
  if(is.null(ProjectName)) stop("You need to give a project name.")
  if(is.null(ProjectName)) stop("You need to give a project date.")
  
  outputDir <- file.path(Folder,paste(ProjectDate,"_",ProjectName,sep=""))
  
  if(file.exists(outputDir)) {stop("Your output directory already exists!")} 
 
  dir.create(path = outputDir)
  
  dir.create(path = file.path(outputDir,"Data"))
  dir.create(path = file.path(outputDir,"Data/GPS"))
  dir.create(path = file.path(outputDir,"Data/Photos"))
  dir.create(path = file.path(outputDir,"Data/WeatherObservations"))
  dir.create(path = file.path(outputDir,"Data/FirelineObservations"))
  dir.create(path = file.path(outputDir,"Data/Maps"))
  
  file.copy(from = system.file("extdata", "ExampleFEMOReport.Rmd", package = "rFEMO"),
            to = outputDir)
  
  file.rename(from = file.path(outputDir,"ExampleFEMOReport.Rmd"),
              to = file.path(outputDir,paste(ProjectDate,"_",ProjectName,"_","Report",".Rmd",sep = "")))
  
  wx <- file.path(outputDir,"Data/WeatherObservations/")
  
  file.copy(from = system.file("extdata", "YYMMDD_FireName_FirelineWeatherObservations.csv", package = "rFEMO"),
            to = file.path(wx))
  
  file.rename(from = file.path(wx,"YYMMDD_FireName_FirelineWeatherObservations.csv"),
              to = file.path(wx,paste(ProjectDate,"_",ProjectName,"_","FirelineWeatherObservations",".csv",sep = "")))
  
  photos <- file.path(outputDir,"Data/Photos/")
  
  file.copy(from = system.file("extdata", "ExamplePhoto.JPG", package = "rFEMO"),
            to = file.path(photos))
    
  maps <- file.path(outputDir,"Data/Maps/")
  
  file.copy(from = system.file("extdata", "ExampleMap.png", package = "rFEMO"),
            to = file.path(maps))
   
  }
