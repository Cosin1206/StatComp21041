#' @title Using R to give a transform between MOS and PESQ
#' @description \code{PESQ_mos} and \code{MOS_lqo} are vital indexes in the period of Speech Separation with a funtcional relationship
#' @param type determine the received type of index,"MOS_lqo" or "PESQ_mos"(don't forget double quotes)
#' @param value the value of input index, should between (1,4.5)
#' @return a transformed value of index (\code{PESQ_mos} or \code{MOS_lqo})
#' @import microbenchmark
#' @import stats
#' @import graphics
#' @import Rcpp
#' @import MASS
#' @import RANN
#' @import boot
#' @import Ball
#' @import energy
#' @import bootstrap
#' @import knitr
#' @examples
#' \dontrun{
#' SS("MOS_lqo",seq(1, 4.5, by=0.1))
#' }
#' @export
SS <- function(type, value) {
  x <- c("MOS_lqo")
  y <- c("PESQ_mos")
  if(type == x) {
    value_t <- 46607 / 14945 - (2000 * log(1/(value/4 - 999/4000) - 1)) / 2989
    plot(value, value_t, main = "MOS_lqo to PESQ_mos", 
         xlab = "MOS_lqo", xlim = range(value), ylab = "PESQ_mos", ylim = c(0, 5))
    cat("\nMOS_lqo score:", value)
    cat("\nPESQ_mos score:", value_t)
    cat("\nMOS_lqo score:", mean(value))
    cat("\nPESQ_mos score:", mean(value_t))
  }
  else if(type == y){
    value_t <- 0.999 + ( 4.999-0.999 ) / ( 1 + exp(-1.4945*value + 4.6607) )
    plot(value, value_t, main = "PESQ_mos to MOS_lqo", 
         xlab = "PESQ_mos", xlim = range(value), ylab = "MOS_lqo", ylim = c(0, 5))
    cat("\nPESQ_mos score:", value)
    cat("\nMOS_lqo score:", value_t)
    cat("\nPESQ_mos score:", mean(value))
    cat("\nMOS_lqo score:", mean(value_t))
  }
  else{
    print("Unrecognizid type of index")
  }
}