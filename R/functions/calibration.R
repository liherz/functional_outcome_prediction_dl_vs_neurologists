# Obtain the data for calibration plots (mainly written by Andrea Götschi)

cal_ens <- function(pdf, y_true, cuts = 12, cumulative = TRUE, pool = TRUE, emp = FALSE,
                    custom_cuts_fun = function(x) quantile(x, c(0.5, 0.9, 0.99, 0.999))
) {
  #cdf <- as.matrix(cdf)
  pdf <- as.matrix(pdf)
  K <- ncol(pdf)
  #pdf <- t(apply(cbind(0, cdf), 1, diff))
  lys_cal <- list()
  start <- 1
  if (cumulative) end <- K-1 else end <- K
  if (K == 2) start <- end <- 2
  for (cl in start:end) {
    if (cumulative) {
      yt <- factor(apply(y_true, 1, function(x) sum(x[start:cl])), levels = c(0, 1)) #levels = c(1, 0)) P(Y<y_k)
      yp <- 1 - apply(pdf, 1, function(x) sum(x[start:cl]))
    } else {
      yt <- factor(y_true[, cl], levels = c(0, 1)) #levels = c(1, 0))
      yp <- 1 - pdf[, cl]
    }
    df <- data.frame(true = yt, pred = yp)
    if (emp) {
      cuts <- custom_cuts_fun(yp)
    }
    if (K == 2) {
      lys_cal <- list(calibration(true ~ pred, data = df, cuts = cuts)$data)
    } else {
      lys_cal[[cl]] <- calibration(true ~ pred, data = df, cuts = cuts)$data
    }
  }
  tmp <- lapply(lys_cal, function(x) {
    as.matrix(x[, c("Percent", "Lower", "Upper", "Count", "midpoint")])
  })
  # ensure that the intervals with zero observations are marked with NA
  # otherwise we include those intervals (with prop = 0) into the mean
  for (i in 1:length(tmp)) {
    k <- which(tmp[[i]][,"Count"] == 0)
    tmp[[i]][k, "Percent"] <- NA
    tmp[[i]][k, "Lower"] <- NA
    tmp[[i]][k, "Upper"] <- NA
  }
  avg <- apply(simplify2array(tmp), 1:2, function(x) mean(x, na.rm = T))
  
  if (pool) {
    ret <- data.frame(bin = lys_cal[[1]]$bin,
                      prop = avg[, "Percent"]/100,
                      lwr = avg[, "Lower"]/100,
                      upr = avg[, "Upper"]/100,
                      cases = avg[, "Count"],
                      midpoint = avg[, "midpoint"]/100)
    ret$prop <- ifelse(ret$cases == 0, NA, ret$prop)
    ret$lwr <- ifelse(ret$cases == 0, NA, ret$lwr)
    ret$upr <- ifelse(ret$cases == 0, NA, ret$upr)
  } else {
    ret <- lys_cal
  }
  return(ret)
}
