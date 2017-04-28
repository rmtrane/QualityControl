#' Unfolded Kappa
#'
#' Caculates the kappa statistic with an option of weights
#'
#' @param x vector of first grades
#' @param y vector of second grades
#' @param lev vector with levels of x and y grades
#' @param nlev number of levels. If NULL, length of lev is used
#' @param wgt weights to be used. If NULL, nlev is used
#' @param cantgrade string which specifies 'Cannot Grade'
#'
#' @export

unfolded.kappa <- function(x, y, lev = unique(c(levels(x), levels(y))),
                           nlev = NULL, wgt = NULL, cantgrade = cantgrade) {

  lev <- lev[!lev %in% c("", cantgrade)]

  if (is.null(nlev))
    nlev = length(lev)

  if (is.null(wgt))
    wgt = diag(1, nlev)

  x1 <- x[!is.na(x) & !is.na(y)]
  y1 <- y[!is.na(x) & !is.na(y)]
  ntot <- length(x1)  # tabulate observed agreement
  p.obs <- matrix(NA, nlev, nlev)
  for (i in 1:nlev) {
    for (j in 1:nlev) {
      p.obs[i, j] <- sum(x1 == lev[i] & y1 == lev[j])/ntot
    }
  }
  # find cell margins
  x.marg <- vector("numeric", nlev)
  y.marg <- vector("numeric", nlev)
  for (i in 1:nlev) {
    x.marg[i] <- sum(x1 == lev[i])/ntot
    y.marg[i] <- sum(y1 == lev[i])/ntot
  }
  # tabulate expected agreement
  p.exp <- matrix(NA, nlev, nlev)
  for (i in 1:nlev) {
    for (j in 1:nlev) {
      p.exp[i, j] <- x.marg[i] * y.marg[j]
    }
  }
  # calculate kappa statistic
  p.obs.wgt <- sum(wgt * p.obs)
  p.exp.wgt <- sum(wgt * p.exp)
  k <- (p.obs.wgt - p.exp.wgt)/(1 - p.exp.wgt)
  # calculate standard error of k statistic
  wgt.row <- vector("numeric", nlev)
  wgt.col <- vector("numeric", nlev)
  for (i in 1:nlev) {
    wgt.row[i] <- sum(y.marg * wgt[i, ])
    wgt.col[i] <- sum(x.marg * wgt[, i])
  }
  var.k <- 0
  for (i in 1:nlev) {
    for (j in 1:nlev) {
      var.k <- var.k + p.obs[i, j] * (wgt[i, j] - (wgt.row[i] + wgt.col[j]) *
                                        (1 - k))^2
    }
  }
  var.k <- var.k - (k - p.exp.wgt * (1 - k))^2
  var.k <- var.k/((1 - p.exp.wgt)^2 * ntot)
  if (nlev == 1) {
    return(list(kappa = NA, se = NA, ci = c(NA, NA)))
  } else {
    return(list(kappa = k, se = sqrt(var.k), ci = pmax(0, pmin(1, k + c(-1.96, 1.96) * sqrt(var.k)))))
  }
}

