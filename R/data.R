#' 1918 Influenza mortality data
#'
#' Weekly influenza deaths per 100,000 during the 1918 pandemic for New York City.
#' 
#' Data is from Supplementary Table 1 of Mills et al 2004 Nature:
#' https://www.nature.com/articles/nature03063 
#'
#' See this article and citations therein for more details on the data.
#' Note that only a subset of the data is present here and the data 
#' are meant to be used for illustrative purposes only. 
#' For a proper re-analysis of these data, use the source mentioned above or 
#' check out data available through project Tycho: https://www.tycho.pitt.edu/
#'
#' @format A data frame with these variables:
#' \describe{
#' \item{Date}{Week of reporting}
#' \item{Deaths}{New deaths per week per 100,000}
#' }
#'
"flu1918data"

#' Cases of norovirus during an outbreak
#'
#' Norovirus case data from an outbreak among children on a school trip
#'
#'The data are from Kuo 2009 Wien Klin Woch: 
#'"A non-foodborne norovirus outbreak among school children during a skiing holiday, Austria, 2007"
#' Specifically, the data comes from figure 1 of this article.
#' The total number of susceptibles was 284.
#' 
#' See this article and citations therein for more details on the data.
#' Note that only a subset of the data is present here and the data 
#' are meant for illustrative purposes only. 
#' For a proper re-analysis of these data, use the source mentioned above.
#' 
#' @format A data frame with these variables:
#' \describe{
#' \item{Date}{Day of outbreak, all in December 2007}
#' \item{Cases}{New cases for the specified date}
#' }
#'
"norodata"