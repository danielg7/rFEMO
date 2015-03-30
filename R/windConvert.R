#' Process a vector of cardinal wind directions
#' 
#' \code{windConvert} processes a vector of cardinal wind directions and returns degrees
#' 
#' @param cardinalWind A vector of cardinal wind directions (i.e., NNE, SSE, SSW)
#' @param LightAndVariable A string that designates light and variable wind values. Default is "L/V"
#' @return A vector of values given the degree angle of the wind.
#' @examples
#' 
#' windCardinal <- c("SE","SSE","NE","L/V")
#' windDegrees <- windConvert(windCardinal)

windConvert <- function(cardinalWind = vector(), LightAndVariable = "L/V")
{
  ifelse(test = !cardinalWind %in% c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW",LightAndVariable),
         yes = stop("One or more variables are not cardinal or light/variable."), NA)

  cardinalWind[cardinalWind == LightAndVariable] <- NA 
  
  cardinalWind <- mapvalues(x = cardinalWind,
                          from = c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"),
                          to = seq(0,337.5,22.5), warn_missing = FALSE)

  return(cardinalWind)
}
