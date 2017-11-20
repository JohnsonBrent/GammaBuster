#' Find the parameters of a gamma distribution that best fit a given expected minumum and maximum
#'
#' @param lower An expected lower bound
#' @param upper An expected upper bound
#' @param pct One's confidence in the above bounds, e.g., .95
#' @param start Starting values for the shape and scale parameters of a gamma function
#' @return A list of output.  The first list item $par contains the parameters describing the shape
#'  and scale of a gamma distribution.  The remaining items describe the convergence of
#'  the search algorithm.
#' @examples
#' GbusterMinMax(20, 30)
#' GbusterMinMax(400, 1500, pct=.80, start=c(200,2))
#'
#' # Note that this function won't operate on vectors of inputs.  So, if you're trying to operate on a
#' # data frame such as the following...
#' min <- c(20, 30, 40)
#' max <- c(40, 35, 80)
#' df <- data.frame(min, max)
#' # # ...then you'll need to apply this function row-by-row such as in the following loop (or via a creative
#' # application of something from the apply() family):
#' for(i in 1:nrow(df)) {
#' gamma.parameters <- GbusterMinMax(df$min[i], df$max[i])  # Find the parameters of a gamma distribution for agiven min and max
#' df$Freq[i] <- gamma.parameters$par[1] / gamma.parameters$par[2]
#' df$Weight[i] <- gamma.parameters$par[2]
#' }
#' @export

GbusterMinMax <- function(lower, upper, pct=.95, start) {
    if (upper < lower) {
      stop("GblusterMinMax sees the lower bound exceeding the upper. Check your inputs.")
    }
    if (pct == 1) {
      stop("You need to use a pct value less than 1 such as .999999.")
    }
    if (pct == 0) {
      stop("You need to use a pct value greater than zero.")
    }
    range <- upper - lower
    mean <- (upper + lower) / 2
    beta <- mean / (range / 4) ^ 2  # define a start value for beta
    alpha <-  beta * mean         # define a start value for alpha
    start <- c(alpha, beta)
    GammaOpt <-
      function(start) {
        (lower - stats::qgamma((1 - pct) / 2, start[1], start[2])) ^ 2 +
          (upper - stats::qgamma(pct + (1 - pct) / 2, start[1], start[2])) ^ 2
      }
    solution <- stats::optim(start, GammaOpt)
    return(solution)
  }

