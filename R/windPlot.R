#' Process wind data and return a plot of wind speed over time.
#' 
#' \code{windPlot} processes a vector of cardinal wind directions and returns degrees
#' 
#' @param windDegrees A vector of degree wind directions (i.e., 30, 45, 48)
#' @param DateTime A vector of POSIXct objects corresponding to the time the data were collected in format yyyy:mm:dd hh:mm.
#' @param windSpeed A string that designates light and variable wind values. Default is "L/V"
#' @return A ggplot of wind speed and direction over time.
#' @examples
#' 
#' windCardinal <- c("SE","SSE","NE","L/V")
#' Degrees <- windConvert(windCardinal, LightAndVariable = "L/V")
#' Speed <- c(5, 2 , 3, "L/V")
#' Time <- lubridate::ymd_hm(c("2015/02/15 18:00","2015/02/15 18:30","2015/02/15 18:45","2015/02/15 19:00"))
#' 
#'  @export
#' 

windPlot <- function(DateTime,windDegrees,windSpeed){
  cleanTheme <- ggthemes::theme_tufte() +
    ggplot2::theme(
      text = ggplot2::element_text(family="sans",size=10),
      axis.line = ggplot2::element_line(size = .3)
    )
  
  if(!is.POSIXct(DateTime)) stop("DateTime must be POSIXct. Try using lubridate to clean it up.")
  if(!is.numeric(windDegrees)) stop("windDegrees must be numeric. Try using windConvert to clean it up.")
  if(!is.numeric(windSpeed)) stop("windSpeed must be numeric. Replace any L/V with NA.")
    
  xend <- seconds(960) * sin(windDegrees * (pi/180)) + DateTime
  yend <- windSpeed * .2 * cos(windDegrees * (pi/180)) + windSpeed
  
  windDF <- data.frame(DateTime,windDegrees,windSpeed,xend,yend)
  
  gg_WindPlot <- ggplot2::ggplot(data = windDF, aes(x = DateTime, y = windSpeed))+
    cleanTheme+
    ylab("Wind Speed (mph)")+
    xlab("Time")+
    geom_segment(size = 1, aes(x = DateTime,
                               xend = xend,
                               y = windSpeed,
                               yend = yend),
                 arrow = arrow(type = "closed",angle = "15",length = unit(.25,"cm")))
  
  return(gg_WindPlot)
}