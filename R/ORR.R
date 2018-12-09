##' Objective Response Rate and Binomial Confidence interval
##' 
##' @title Objective Response Rate and Binomial Confidence interval
##' @param x a number indicating total number of subjects with Partial Response or Complete Response
##' @param n a number indicating total number of subjects
##' @return Objective Response Rate and 95% binomial confidence interval
##' @author Jon
##' @export
##' @examples 
##' ORR(5,13)
ORR <- function(x, n) {
    ORRate <- (x/n) * 100
    ORRateCI <- binom.confint(x, n, conf.level = 0.95, methods = "exact")
    out <- list(ORR = ORRate, ORRCI = ORRateCI)
    return(out)
}
