#' Process RH data and plot it over time.
#' 
#' \code{rhPlot} processes a vector of cardinal wind directions and returns degrees
#' 
#' @param RH A vector of relative humidities
#' @param DateTime A vector of POSIXct objects corresponding
#'        to the time the data were collected in format yyyy:mm:dd hh:mm.
#' @param RH_LowBoundary A scalar value of the low boundary parameters for the RH. Defaults to NULL.
#' @param RH_HighBoundary A scalar value of the high boundary parameters for the RH. Defaults to NULL.
#' @return A ggplot of RH over time.
#' @examples
#' 
#' myRH <- c(64,58,70,76)
#' Time <- lubridate::ymd_hm(c("2015/02/15 18:00",
#'                            "2015/02/15 18:30",
#'                            "2015/02/15 18:45",
#'                            "2015/02/15 19:00"))
#' df <- data.frame(myRH,Time)
#' 
#' High <- 70
#' Low <- 20
#' 
#' rhPlot(RH = df$myRH,
#'        DateTime = df$Time,
#'        RH_LowBoundary = Low,
#'        RH_HighBoundary = High)
#' 
#'  @export
#' 

rhPlot <- function(RH,DateTime,RH_LowBoundary = NULL, RH_HighBoundary = NULL){
  cleanTheme <- ggthemes::theme_tufte() +
    ggplot2::theme(
      text = ggplot2::element_text(family="sans",size=12),
      axis.line = ggplot2::element_line(size = .3)
    )
  
  if(!lubridate::is.POSIXct(DateTime)) stop("DateTime must be POSIXct. Try using lubridate to clean it up.")
  if(!is.numeric(RH)) stop("RH must be numeric.")
  if(!is.null(RH_LowBoundary))
    if(!is.numeric(RH_LowBoundary))
      stop("RH_LowBoundary must be numeric.")
  if(!is.null(RH_HighBoundary))
    if(!is.numeric(RH_HighBoundary))
      stop("RH_HighBoundary must be numeric.")
  if(is.null(RH_HighBoundary)) RH_HighBoundary <- -50
  if(is.null(RH_LowBoundary)) RH_LowBoundary <- -50
  
  RHLow <<- RH_LowBoundary
  RHHigh <<- RH_HighBoundary
  
  plotDF <- data.frame(DateTime, RH)
  
  gg_RHPlot <- ggplot2::ggplot(data = plotDF, ggplot2::aes(x = DateTime, y = RH))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::scale_x_datetime()+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = RHLow, yend = RHLow))+
    ggplot2::geom_segment(linetype = "dashed", ggplot2::aes(x = min(DateTime), xend = max(DateTime),y = RHHigh, yend = RHHigh))+
    ggplot2::ylim(0,100)+
    cleanTheme+
    ggplot2::ylab("Relative Humidity (%)")+
    ggplot2::xlab("Time")
    
    
  return(gg_RHPlot)
}