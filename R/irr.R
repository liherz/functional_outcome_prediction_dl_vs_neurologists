rm(list = ls())
library(reshape2)
library(irr)



DIR0 <- "C:/Users/Lisa/Dropbox/PhD/Stroke/Stroke_perfusion/"


# Load data --------------------------------------------------------------------

# dat_clinical
load(paste0(DIR0, "/code/data/dat_clinical_raters.R"))
# dat_image
load(paste0(DIR0, "/code/data/dat_image_raters.R"))
# dat_clinical_and_image
load(paste0(DIR0, "/code/data/dat_clinical_and_image_raters.R"))


# # IRR example ----------------------------------------------------------------
# 
# data("diagnoses", package = "irr")
# head(diagnoses[, 1:3])
# str(diagnoses)


# IRR raters -------------------------------------------------------------------

# Ordinal

# Adapt data structure for function kappam.fleiss
irr_clinical <- reshape2::dcast(dat_clinical, 
                                p_id_clinical ~ rater, 
                                value.var = "pred_clinical")[,-1]
irr_image <- reshape2::dcast(dat_image, 
                             p_id_image ~ rater, 
                             value.var = "pred_image")[,-1]
irr_clinical_and_image <- reshape2::dcast(dat_clinical_and_image, 
                                          p_id_clinical_and_image ~ rater, 
                                          value.var = "pred_clinical_and_image")[,-1]


# check structure: Passt
#View(irr_clinical)
#View(dat_clinical)


kappam.fleiss(irr_clinical)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.0697 
# 
#         z = 3.23 
#   p-value = 0.00122 

kappam.fleiss(irr_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 49 
#    Raters = 5 
#     Kappa = 0.0559 
# 
#         z = 2.5 
#   p-value = 0.0124 

kappam.fleiss(irr_clinical_and_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.0719 
# 
#         z = 3.22 
#   p-value = 0.00127 




# Binary

# Adapt data structure for function kappam.fleiss
irr_clinical <- reshape2::dcast(dat_clinical, 
                                p_id_clinical ~ rater, 
                                value.var="pred_clinical_bin")[,-1]
irr_image <- reshape2::dcast(dat_image, 
                             p_id_image ~ rater, 
                             value.var="pred_image_bin")[,-1]
irr_clinical_and_image <- reshape2::dcast(dat_clinical_and_image, 
                                          p_id_clinical_and_image ~ rater, 
                                          value.var="pred_clinical_and_image_bin")[,-1]

kappam.fleiss(irr_clinical)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.205 
# 
#         z = 4.58 
#   p-value = 4.69e-06 

kappam.fleiss(irr_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 49 
#    Raters = 5 
#     Kappa = 0.134 
# 
#         z = 2.96 
#   p-value = 0.00312 

kappam.fleiss(irr_clinical_and_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.216 
# 
#         z = 4.82 
#   p-value = 1.44e-06