##' Disease Control Rate and Binomial Confidence interval
##' 
##' @title Disease Control Rate and Binomial Confidence interval
##' @param x a number indicating total number of subjects with Stable Disease, Partial Response, or Complete Response
##' @param n a number indicating total number of subjects
##' @return disease control rate and 95% binomial confidence interval
##' @author Jon
##' @export
##' @examples 
##' DCR(7,13)
DCR <- function(x, n) {
    DCRate <- (x/n) * 100
    DCRateCI <- binom.confint(x, n, conf.level = 0.95, methods = "exact")
    out <- list(DCR = DCRate, DCRCI = DCRateCI)
    return(out)
}
