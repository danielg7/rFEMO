#' Process temperature data and plot it over time.
#' 
#' \code{tempPlot} processes a vector of temperatures and time and returns a plot.
#' 
#' @param Temp A vector of temepratures
#' @param DateTime A vector of POSIXct objects
#'                  corresponding to the time the data were collected in format yyyy:mm:dd hh:mm.
#' @param Temp_LowBoundary A scalar value of the low boundary parameters for temperature RH. Defaults to NULL.
#' @param Temp_HighBoundary A scalar value of the high boundary parameters for the temperature. Defaults to NULL.
#' @return A ggplot of temperature over time.
#' @examples
#' 
#' myTemp <- c(72,68,52,48)
#' Time <- lubridate::ymd_hm(c("2015/02/15 18:00",
#'                            "2015/02/15 18:30",
#'                            "2015/02/15 18:45",
#'                            "2015/02/15 19:00"))
#' df <- data.frame(myTemp,Time)
#' 
#' High <- 70
#' Low <- 50
#' 
#' tempPlot(Temp = df$myTemp,
#'          DateTime = df$Time,
#'          Temp_LowBoundary = Low,
#'          Temp_HighBoundary = High)
#' 
#'  @export
#' 

tempPlot <- function(Temp,DateTime,Temp_LowBoundary = NULL, Temp_HighBoundary = NULL){
  cleanTheme <- ggthemes::theme_tufte() +
    ggplot2::theme(
      text = ggplot2::element_text(family="sans",size=12),
      axis.line = ggplot2::element_line(size = .3)
    )
  
  if(!lubridate::is.POSIXct(DateTime)) stop("DateTime must be POSIXct. Try using lubridate to clean it up.")
  if(!is.numeric(Temp)) stop("Temp must be numeric.")
  if(!is.null(Temp_LowBoundary))
    if(!is.numeric(Temp_LowBoundary))
      stop("Temp_LowBoundary must be numeric.")
  if(!is.null(Temp_HighBoundary))
    if(!is.numeric(Temp_HighBoundary))
      stop("Temp_HighBoundary must be numeric.")
  if(is.null(Temp_HighBoundary)) Temp_HighBoundary <- -50
  if(is.null(Temp_LowBoundary)) Temp_LowBoundary <- -50
  
  TempLow <<- Temp_LowBoundary
  TempHigh <<- Temp_HighBoundary
  
  plotDF <- data.frame(DateTime, Temp)
  
  gg_RHPlot <- ggplot2::ggplot(data = plotDF, ggplot2::aes(x = DateTime, y = Temp))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::scale_x_datetime()+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = TempLow, yend = TempLow))+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = TempHigh, yend = TempHigh))+
    ggplot2::ylim(x = plyr::round_any(min(Temp),f=floor,accuracy=10)-10,plyr::round_any(max(Temp),f=ceiling,accuracy=10)+10)+
    cleanTheme+
    ggplot2::ylab("Temperature")+
    ggplot2::xlab("Time")
  
  
  return(gg_RHPlot)
}