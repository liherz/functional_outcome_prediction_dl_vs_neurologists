library(scoring)
library(pROC)
library(vcd)


# Metrics to compute

# Quadratic Weighted Kappa
qwk <- function(dat, pred, true){
  nlevels <- length(levels(as.factor(dat[, true])))
  if(nlevels > 2){
    tab <- table(factor(dat[, pred], levels = c(0:6)), factor(dat[, true], levels = c(0:6)))
  } else{
    tab <- table(factor(dat[, pred], levels = c(0:1)), factor(dat[, true], levels = c(0:1)))
  }
  res <- Kappa(tab)
  return(res$Weighted["value"])
}


# Accuracy
acc <- function(dat, pred, true){
  acc <- mean(dat[, pred] == dat[, true])*100
  return(acc)
}

# Sensitivity
sens <- function(dat, pred, true){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat <- table(dat[, true], dat[, pred])
  sens <- conf_mat[2,2]/sum(conf_mat[2,])
  return(round(sens*100, 2))
}

# Specificity
spec <- function(dat, pred, true){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat <- table(dat[,true], dat[,pred])
  spec <- conf_mat[1,1]/sum(conf_mat[1,])
  return(round(spec*100,2))
}

# Area under the Receiver Operating Characteristics Curve
auc <- function(dat){
  res <- roc(dat[,"y_true"], dat[, "y_pred1_fav"], levels = c(0,1), direction = "<")
  return(res$auc)
}

# Negative Log Likelihood
nll <- function(dat, nlevels){
  if(nlevels > 2){
    # ensure that values sum to exactly 1 (problem wegen zu vielen Nachkommastellen)
    dat[, "y_pred6"] <- 1-(dat[, "y_pred0"] + dat[, "y_pred1"] + dat[, "y_pred2"] + dat[, "y_pred3"] + dat[, "y_pred4"] + dat[, "y_pred5"])
    nll <- mean(logscore((dat[, "y_true"] + 1) ~ dat[, "y_pred0"] + dat[, "y_pred1"] + dat[, "y_pred2"] + dat[, "y_pred3"] + dat[, "y_pred4"] + dat[, "y_pred5"] + dat[, "y_pred6"])) 
  } else{
    nll <- mean(logscore(dat[, "y_true"] ~ (1-dat[, "y_pred1_fav"]) + dat[, "y_pred1_fav"]))
  }
  return(nll)
}


# Brier Score
bs <- function(dat, nlevels){
  if(nlevels > 2){
    bs <- NA
  } else{
    bs <- mean(brierscore(dat[, "y_true"] ~ (1-dat[, "y_pred1_fav"]) + dat[, "y_pred1_fav"]))
  }
  return(bs)
}