#' Finds the parameters of a gamma distribution that best fit a given expected mean and range
#'
#' @param mean The expected value
#' @param range The expected range containing the mean
#' @param pct One's confidence in the above range, e.g., .95
#' @param start Starting values for the shape and scale parameters of a gamma function
#' @return A list of output.  The first list item $par contains the parameters describing the shape
#'  and scale of a gamma distribution.  The remaining list items describe the convergence of
#'  the search algorithm.
#' @examples
#' GbusterMean(20, 30)
#' GbusterMean(mean=200, range=400, pct=.80, start=c(10,.05))
#' @export


GbusterMean <- function(mean, range, pct = .975, start) {
  if (pct == 1) {
    stop("You need to use a pct value less than 1 such as .999999.")
  }
  if (pct == 0) {
    stop("You need to use a pct value greater than zero.")
  }
  beta <- mean / (range / 4) ^ 2  # define a start value for beta
  alpha <-  beta * mean         # define a start value for alpha
  start = c(alpha, beta)             # starting values assembled

  func <- function(start) {
    abs(start[1] / start[2] - mean) + abs(start[1] / start[2] ^ 2 - (range /
                                                                       2 / qnorm(pct)) ^ 2)
  }

  solution <- optim(start, func)
  return(solution)
}

