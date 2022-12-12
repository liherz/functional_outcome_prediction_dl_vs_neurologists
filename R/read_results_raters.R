rm(list=ls())
library(openxlsx)

DIR0 <- "C:/Users/Lisa/Dropbox/PhD/Stroke/Stroke_perfusion/"
#DIR0 <- "C:/Users/hezo/Dropbox/PhD/Stroke/Stroke_perfusion/"

# read data ---------------------------------------------------------------

# directories from raters
dirs_raters <- c(paste0(DIR0, "prediction_challenge/rater1"),
                 paste0(DIR0, "prediction_challenge/rater2"),
                 paste0(DIR0, "prediction_challenge/rater3"),
                 paste0(DIR0, "prediction_challenge/rater4"),
                 paste0(DIR0, "prediction_challenge/rater5"))

# other variables
dat_all <- read.table(paste0(DIR0, "data/data_bern_25_11_2020_dwi.csv"),
                      sep = ",", header = T)
dat_all$mrs_3months_binary <- ifelse(dat_all$mrs_3months <= 2, 1, 0)
dat_all$mrs_3months <- factor(dat_all$mrs_3months, levels = c(0:6))
dat_all$mrs_3months_binary <- factor(dat_all$mrs_3months_binary, ordered = T, levels = c(0:1))




# read rater predictions -------------------------------------------------------

# folder names of images do not match with the correct ID!
correct_image_ids <- read.xlsx(paste0(DIR0, "prediction_challenge/correct_IDs_image_data.xlsx"))

dat_clinical <- data.frame()
dat_image <- data.frame()
dat_clinical_and_image <- data.frame()
# p_id_clinical <- c()
# p_id_image <- c()
# p_id_clinical_and_image <- c()

for(i in 1:length(dirs_raters)){
  print(paste0("rater: ",i))

  path_clinical <- list.files(paste0(dirs_raters[i], "/clinical_data"), recursive = T)
  path_clinical <- path_clinical[grepl(".xlsx", path_clinical)]
  path_clinical <- paste0(dirs_raters[i], "/clinical_data/", path_clinical)
  
  path_image <- list.files(paste0(dirs_raters[i], "/image_data"), recursive = T)
  path_image <- path_image[grepl(".xlsx", path_image)]
  path_image <- paste0(dirs_raters[i], "/image_data/", path_image)
  
  path_clinical_and_image <- list.files(paste0(dirs_raters[i], "/clinical_and_image_data"), recursive = T)
  path_clinical_and_image <- path_clinical_and_image[grepl(".xlsx", path_clinical_and_image)]
  path_clinical_and_image <- paste0(dirs_raters[i], "/clinical_and_image_data/", path_clinical_and_image)

  # read the files
  pred_clinical <- c()
  pred_image <- c()
  pred_clinical_and_image <- c()
  for(j in 1:50){
    #print(j)
    # clinical
    dat <- read.xlsx(path_clinical[j])
    pred_clinical <- c(pred_clinical, dat$mRS_predicted)
    #print(paste0("img", j))
    # image
    dat <- read.xlsx(path_image[j])
    pred_image <- c(pred_image, dat$mRS_predicted)
    #print(paste0("clin+img", j))
    # clinical and image
    dat <- read.xlsx(path_clinical_and_image[j]) 
    pred_clinical_and_image <- c(pred_clinical_and_image, dat$mRS_predicted[1]) #some rater added comments -> consider only the prediction
  }
  
  # rater three used values "2 to 3" etc.
  # did not really change performance when using the smaller or larger class
  if(i == 3){
    pred_clinical <- as.numeric(unlist(lapply(strsplit(pred_clinical, "to"), head, n=1)))
    pred_image <- as.numeric(unlist(lapply(strsplit(pred_image, "to"), head, n=1)))
  }
  
  # checked if we always have the same ID per rater: YES
  #p_id_clinical <- cbind(p_id_clinical, unlist(lapply(strsplit(path_clinical, "/"), `[[`, 11)))
  #p_id_image <- cbind(p_id_image, unlist(lapply(strsplit(path_image, "/"), `[[`, 11)))
  #p_id_clinical_and_image <- cbind(p_id_clinical_and_image, unlist(lapply(strsplit(path_clinical_and_image, "/"), `[[`, 11)))
  p_id_clinical <- as.numeric(list.files(paste0(dirs_raters[2], "/clinical_data"), recursive = F))
  p_id_image <- as.numeric(list.files(paste0(dirs_raters[2], "/image_data"), recursive = F))
  p_id_clinical_and_image <- as.numeric(list.files(paste0(dirs_raters[2], "/clinical_and_image_data"), recursive = F))
  
  # replace the image IDs with the correct IDs!
  all(p_id_image == correct_image_ids$image.ID) # do the IDs match?
  p_id_image <- correct_image_ids$correct.ID
  
  # define dataset and merge with other important variables
  dat_out <- data.frame("p_id_clinical" = p_id_clinical,
                        "p_id_image" = p_id_image, 
                        "p_id_clinical_and_image" = p_id_clinical_and_image,
                        "pred_clinical" = pred_clinical,
                        "pred_image" = pred_image,
                        "pred_clinical_and_image" = pred_clinical_and_image,
                        "pred_clinical_bin" = ifelse(pred_clinical<=2, 1, 0),
                        "pred_image_bin" = ifelse(pred_image<=2, 1, 0),
                        "pred_clinical_and_image_bin" = ifelse(pred_clinical_and_image<=2, 1, 0),
                        "rater" = i)
  
  
  # for current rater
  dat_clinical_tmp <- merge(dat_out, dat_all, by.x = "p_id_clinical", by.y = "p_id")
  dat_image_tmp <- merge(dat_out, dat_all, by.x = "p_id_image", by.y = "p_id")
  dat_clinical_and_image_tmp <- merge(dat_out, dat_all, by.x = "p_id_clinical_and_image", by.y = "p_id")
  
  # for all raters
  dat_clinical <- rbind(dat_clinical, dat_clinical_tmp)
  dat_image <- rbind(dat_image, dat_image_tmp)
  dat_clinical_and_image <- rbind(dat_clinical_and_image, dat_clinical_and_image_tmp)

}


# save data --------------------------------------------------------------------

save(dat_clinical, file = paste0(DIR0, "code/data/dat_clinical_raters.R"))
save(dat_image, file = paste0(DIR0, "code/data/dat_image_raters.R"))
save(dat_clinical_and_image, file = paste0(DIR0, "code/data/dat_clinical_and_image_raters.R"))

write.csv(dat_clinical, file = paste0(DIR0, "code/data/dat_clinical_raters.csv"))
write.csv(dat_image, file = paste0(DIR0, "code/data/dat_image_raters.csv"))
write.csv(dat_clinical_and_image, file = paste0(DIR0, "code/data/dat_clinical_and_image_raters.csv"))

