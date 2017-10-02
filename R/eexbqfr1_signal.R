#' Trade signal for German Power Futures. Front Base Quarter contract
#'
#' @param eexbq an xts object. this is the front base quarter contract traded on EEX.
#'   https://www.eex.com/en/market-data/power/futures/phelix-deat-futures#!/2017/09/08
#'
#'
#' @param api2q an xts object. This is the front quarter contract traded/
#'   cleared on ICE.
#'   https://www.theice.com/products/243/API2-Rotterdam-Coal-Futures/data
#'
#' @return a logical object. (1 or -1) the trade signal.
#' @export
#' @import tidyverse
#' @import xts
#' @import lubridate
#' @import ranger
#' @import TTR
#' @import PerformanceAnalytics
#' @importFrom magrittr "%>%"
#'
#' @examples
debqfr1_model <- function (eexbq,api2q){
  #source("functions.R")

  api2q <- tecA2(api2q,"api2q")
  eexbq <- tecA2(eexbq,"eexbq")

  p <- dplyr::left_join(eexbq,api2q,by="Day")
  p <- tail(p,1)

  p$R23 <- stats::predict(model23,p)

  if (tail(p$R23,1)==1) {
    print("EEXBFRQ1 will close higher the next trading day")
    return(1)
  } else {
    print("EEXBFRQ1 will close lower the next trading day")
    return(-1)
  }

}
