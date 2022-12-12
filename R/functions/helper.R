library(plyr)
library(reshape2)

# Some helper functions

# Change format for plotting the bootstrap results
long_format <- function(dat, metric){
  dat_tmp <- dat[, c(metric, paste0(metric, "l"),
                     paste0(metric, "m"), paste0(metric, "u"), 
                     "data", "method", "rater")]
  newnames <- c("boot", "ci_lower", "median", "ci_upper", "data", "method", "rater")
  arg <- setNames(newnames, names(dat_tmp))
  dat_tmp <- rename(dat_tmp, arg)
  dat_tmp$metric <- metric
  return(dat_tmp)
}


# Get the estimates of the models with linear shift terms
# (Standardized coefficients)
get_estimates <- function(DIR_MOD){
  est <- data.frame()
  for(i in 0:4){
    esti <- read.table(paste0(DIR_MOD, "/fold", i, "/estimates.csv"), 
                       sep =",", header = T)
    esti <- exp(esti)
    esti <- esti[,!grepl("intercept", colnames(esti))]
    esti$fold = i
    est <- rbind(est, esti)
  }
  lest <- reshape2::melt(est, id.vars = c("fold"))
  # get average prediction
  mest <- apply(est[,1:(ncol(est)-1)], 2, mean)
  mest <- data.frame(mest)
  
  lest <- merge(lest, mest, by.x = "variable", by.y = 0)
  lest <- lest[order(abs(lest$mest), decreasing = TRUE),]
  
  lest$variable <- factor(lest$variable, levels = unique(lest$variable))
  lest$variable <- revalue(lest$variable, replace = c("nihss_bl" = "NIHSS",
                                                      "age" = "Age",
                                                      "sys_bloodpressure_bl" = "SBP",
                                                      "time_to_groin_puncture" = "Time",
                                                      "rf_diabetes" = "Diabetes",
                                                      "rf_hypertonia" = "Hypertension",
                                                      "rf_tia_stroke" = "Prior stroke",
                                                      "lyse" = "IVT",
                                                      "rf_smoker" = "Smoking"))
  lest$fold <- factor(lest$fold)
  return(lest)
}
