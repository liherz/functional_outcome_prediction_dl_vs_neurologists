# Metrics which will be calculated with the bootstrapping results
source(paste0(DIR0, "code/R/functions/metrics.R"))

# bootstrapping
bootstrap_results <- function(dat, pred, true, rater = c("rater", "model"), 
                              data_modality = c("clinical", "image", "clinical_and_image"),
                              nsamples = 1000, prob = NULL){
  # datasets = list of datasets
  # nsample = number of bootstrap samples
  acc_boot <- rep(NA, 1000)
  qwk_boot <- rep(NA, 1000)
  nll_boot <- rep(NA, 1000)
  auc_boot <- rep(NA, 1000)
  bs_boot <- rep(NA, 1000)
  sens_boot <- rep(NA, 1000)
  spec_boot <- rep(NA, 1000)
  nlevels <- length(levels(as.factor(dat[, true])))
  
  set.seed(123)
  for(i in 1:nsamples){
    # get bootstrap sample (n = ntest)
    idx <- sample(1:nrow(dat), size = 500, replace = T)
    boots <- dat[idx,]
    
    # get metrics
    acc_boot[i] <- acc(boots, pred, true)
    qwk_boot[i] <- qwk(boots, pred, true)
    if(nlevels<=2){
      sens_boot[i] <- sens(boots, pred, true)
      spec_boot[i] <- spec(boots, pred, true)
    }
    if(rater == "model"){
      nll_boot[i] <- nll(boots, nlevels)
      bs_boot[i] <- bs(boots, nlevels)
      if(nlevels<=2){
        auc_boot[i] <- auc(boots)
      }
    }
  }
  
  # get the confidence intervals
  if(rater == "rater"){
    acc_out <- round(quantile(acc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    qwk_out <- round(quantile(qwk_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    nll_out <- nll_boot
    auc_out <- auc_boot
    bs_out <- bs_boot
    sens_out <- sens_boot
    spec_out <- spec_boot
    if(nlevels<=2){
      sens_out <- round(quantile(sens_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      spec_out <- round(quantile(spec_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    }
  } else{
    acc_out <- round(quantile(acc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    qwk_out <- round(quantile(qwk_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    nll_out <- round(quantile(nll_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    auc_out <- auc_boot
    bs_out <- bs_boot
    sens_out <- sens_boot
    spec_out <- spec_boot
    if(nlevels <= 2){
      auc_out <- round(quantile(auc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      bs_out <- round(quantile(bs_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      sens_out <- round(quantile(sens_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      spec_out <- round(quantile(spec_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    }
  }
  
  return(data.frame("acc" = acc_boot, 
                    "accl" = rep(acc_out[1], nsamples), 
                    "accm" = rep(acc_out[2], nsamples), 
                    "accu" = rep(acc_out[3], nsamples),
                    "qwk" = qwk_boot, 
                    "qwkl" = rep(qwk_out[1], nsamples), 
                    "qwkm" = rep(qwk_out[2], nsamples), 
                    "qwku" = rep(qwk_out[3], nsamples),
                    "nll" = nll_boot, 
                    "nlll" = rep(nll_out[1], nsamples), 
                    "nllm" = rep(nll_out[2], nsamples), 
                    "nllu" = rep(nll_out[3], nsamples),
                    "auc" = auc_boot, 
                    "aucl" = rep(auc_out[1], nsamples), 
                    "aucm" = rep(auc_out[2], nsamples), 
                    "aucu" = rep(auc_out[3], nsamples),
                    "bs" = bs_boot, 
                    "bsl" = rep(bs_out[1], nsamples), 
                    "bsm" = rep(bs_out[2], nsamples), 
                    "bsu" = rep(bs_out[3], nsamples),
                    "sens" = sens_boot, 
                    "sensl" = rep(sens_out[1], nsamples), 
                    "sensm" = rep(sens_out[2], nsamples), 
                    "sensu" = rep(sens_out[3], nsamples),
                    "spec" = spec_boot, 
                    "specl" = rep(spec_out[1], nsamples), 
                    "specm" = rep(spec_out[2], nsamples), 
                    "specu" = rep(spec_out[3], nsamples),
                    "data" = rep(data_modality, nsamples)))
}