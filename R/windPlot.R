#' Read wind data and return a plot of wind speed over time.
#' 
#' \code{windPlot} processes a vector of cardinal wind directions and returns degrees
#' 
#' @param windDegrees A vector of degree wind directions (i.e., 30, 45, 48)
#' @param DateTime A vector of POSIXct objects corresponding
#'                to the time the data were collected in format yyyy:mm:dd hh:mm.
#' @param windSpeed A string that designates light and variable wind values. Default is "L/V"
#' @param Wind_LowBoundary A scalar value of the low boundary parameters for the wind Defaults to NULL.
#' @param Wind_HighBoundary A scalar value of the high boundary parameters for the wind Defaults to NULL.
#' @return A ggplot of wind speed and direction over time.
#' @examples
#' 
#' windCardinal <- c("SE","SSE","NE","L/V")
#' Degrees <- windConvert(windCardinal, LightAndVariable = "L/V")
#' Speed <- c(5, 2 , 3, NA) # Replace light and variable speeds with NA
#' Time <- lubridate::ymd_hm(c("2015/02/15 18:00",
#'                            "2015/02/15 18:30",
#'                            "2015/02/15 18:45",
#'                            "2015/02/15 19:00"))
#' windPlot(Time,
#'          Degrees,
#'          Speed,
#'          Wind_LowBoundary = 1,
#'          Wind_HighBoundary = 10)
#' 
#'  @export
#' 

windPlot <- function(DateTime,windDegrees,windSpeed, Wind_LowBoundary = NULL, Wind_HighBoundary = NULL){
  cleanTheme <- ggthemes::theme_tufte() +
    ggplot2::theme(
      text = ggplot2::element_text(family="sans",size=12),
      axis.line = ggplot2::element_line(size = .3)
    )
  
  if(!lubridate::is.POSIXct(DateTime)) stop("DateTime must be POSIXct. Try using lubridate to clean it up.")
  if(!is.numeric(windDegrees)) stop("windDegrees must be numeric. Try using windConvert to clean it up.")
  if(!is.numeric(windSpeed)) stop("windSpeed must be numeric. Replace any L/V with NA.")
  
  if(is.null(Wind_HighBoundary)) Wind_HighBoundary <- -50
  if(is.null(Wind_LowBoundary)) Wind_LowBoundary <- -50
    
  xend <- lubridate::seconds(960) * sin(windDegrees * (pi/180)) + DateTime
  yend <- windSpeed * .2 * cos(windDegrees * (pi/180)) + windSpeed
  
  windDF <- data.frame(DateTime,windDegrees,windSpeed,xend,yend)
  
  gg_WindPlot <- ggplot2::ggplot(data = windDF, ggplot2::aes(x = DateTime, y = windSpeed), environment = environment())+
    cleanTheme+
    ggplot2::ylab("Wind Speed (mph)")+
    ggplot2::xlab("Time")+
    ggplot2::geom_segment(size = 1, ggplot2::aes(x = DateTime,
                               xend = xend,
                               y = windSpeed,
                               yend = yend),
                 arrow = grid::arrow(type = "closed",angle = "15",length = grid::unit(.25,"cm")))+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = Wind_HighBoundary, yend = Wind_HighBoundary))+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = Wind_LowBoundary, yend = Wind_LowBoundary))+
    ggplot2::ylim(0,
                  plyr::round_any(max(windSpeed),f=ceiling,accuracy=10)+10)+
    cleanTheme

  
  return(gg_WindPlot)
}