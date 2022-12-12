rm(list=ls())

library(openxlsx)
library(dplyr)
# library(plyr)

# set the path to the files
dir = "C:/Users/hezo/Dropbox/PhD/Stroke/Stroke_perfusion/"
setwd(dir)

# read data: from ipynb Read_DICOMs_Bern_01.ipynb
dat = read.table(paste0(dir,"data/data_sept_2019.csv"), sep=",", header=T)

# Number of patients
length(unique(dat$p_id)) # n = 223


# DWIs and ADCs: Different names for the different patients
length(unique(dat$p_id[dat$sequence == "ep2d_diff_M128_b0_1000_DIN" | 
                         dat$sequence == "ep2d_diff_3scan_trace_p2" |
                         dat$sequence == "resolve_3scan_trace_tra_176_p2_TRACEW" |
                         dat$sequence == "ep2d_diffusion" |
                         dat$sequence == "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_TRACEW"])) # n = 223
length(unique(dat$p_id[dat$sequence == "ep2d_diff_M128_b0_1000_DIN_ADC" | 
                         dat$sequence == "ep2d_diff_3scan_trace_p2_ADC" |
                         dat$sequence == "resolve_3scan_trace_tra_176_p2_ADC" |
                         dat$sequence == "ep2d_diffusion_ADC" |
                         dat$sequence == "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_ADC"])) # n = 223
# Perfusion maps
length(unique(dat$p_id[dat$sequence=="TTP_OLEA_ANALYSIS"])) # n = 223
length(unique(dat$p_id[dat$sequence=="rBF_OLEA_ANALYSIS"])) # n = 223
length(unique(dat$p_id[dat$sequence=="rBV_OLEA_ANALYSIS"])) # n = 223
length(unique(dat$p_id[dat$sequence=="TMAX_OLEA_ANALYSIS"])) # n = 223
length(unique(dat$p_id[dat$sequence=="MTT_OLEA_ANALYSIS"])) # n = 223

# For each patient we have DWI/ADCs and the perfusion maps


# Create a variable modality
dwi = c("ep2d_diff_M128_b0_1000_DIN", "ep2d_diff_3scan_trace_p2", "ep2d_diff_3scan_trace_p3", 
        "resolve_3scan_trace_tra_176_p2_TRACEW", "ep2d_diffusion", 
        "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_TRACEW")
adc = c("ep2d_diff_M128_b0_1000_DIN_ADC", "ep2d_diff_3scan_trace_p2_ADC", "ep2d_diff_3scan_trace_p3_ADC",
        "resolve_3scan_trace_tra_176_p2_ADC", "ep2d_diffusion_ADC", 
        "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_ADC")

dat$modality = NA
dat$modality[dat$sequence %in% dwi] = "DWI"
dat$modality[dat$sequence %in% adc] = "ADC"
dat$modality[dat$sequence == "rBF_OLEA_ANALYSIS"] = "CBF"
dat$modality[dat$sequence == "rBV_OLEA_ANALYSIS"] = "CBV"
dat$modality[dat$sequence == "TMAX_OLEA_ANALYSIS"] = "TMAX"
dat$modality[dat$sequence == "MTT_OLEA_ANALYSIS"] = "MTT"
dat$modality[dat$sequence == "TTP_OLEA_ANALYSIS"] = "TTP"
dat$modality = as.factor(dat$modality)
levels(dat$modality)
# [1] "ADC"  "CBF"  "CBV"  "DWI"  "MTT"  "TMAX" "TTP"
dat$sequence[is.na(dat$modality)]
# factor(0)



# Keep only one series of DWIs/ADCs
dat$modality_no = 0
dat$modality_no[dat$sequence == "ep2d_diff_M128_b0_1000_DIN"] = 1
dat$modality_no[dat$sequence == "ep2d_diff_M128_b0_1000_DIN_ADC"] = 1
dat$modality_no[dat$sequence == "ep2d_diff_3scan_trace_p2"] = 2
dat$modality_no[dat$sequence == "ep2d_diff_3scan_trace_p2_ADC"] = 2
dat$modality_no[dat$sequence == "ep2d_diff_3scan_trace_p3"] = 3
dat$modality_no[dat$sequence == "ep2d_diff_3scan_trace_p3_ADC"] = 3
dat$modality_no[dat$sequence == "resolve_3scan_trace_tra_176_p2_TRACEW"] = 4
dat$modality_no[dat$sequence == "resolve_3scan_trace_tra_176_p2_ADC"] = 4
dat$modality_no[dat$sequence == "ep2d_diffusion"] = 5
dat$modality_no[dat$sequence == "ep2d_diffusion_ADC"] = 5
dat$modality_no[dat$sequence == "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_TRACEW"] = 6
dat$modality_no[dat$sequence == "ep2d_diff_4scan_trace_p2_s2_inkl.Neckcoil_ADC"] = 6


# Keep only one series of DWIs/ADCs
dat0 = dat
nrow(dat) # 46427
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  # get the numbers of sequences and sort them
  mods = sort(unique(pat$modality_no[pat$modality_no != 0]))
  # keep only the first sequence (and the perfusion maps)
  if(length(mods) > 1){
    dat = dat[-which(dat$p_id == pid & dat$modality_no > mods[1]), ]
  }
}
nrow(dat) # 41338


# For some patients we have the information from multiple folders, take one only:
dat$path = as.character(dat$path)
dat$folder = NA
dat$image = NA

# Patient: 11555980, 11597844, 13169912, 13213261, 13639013, 13915045
# folder MR-Schädel in the folder with the data: delete it
dat = dat[-which(dat$p_id == "11555980" & grepl("Schädel", dat$path)), ]
dat = dat[-which(dat$p_id == "11597844" & grepl("Schädel", dat$path)), ]
dat = dat[-which(dat$p_id == "13169912" & grepl("Schädel", dat$path)), ]
dat = dat[-which(dat$p_id == "13213261" & grepl("Schädel", dat$path)), ]
dat = dat[-which(dat$p_id == "13639013" & grepl("Schädel", dat$path)), ]
dat = dat[-which(dat$p_id == "13915045" & grepl("Schädel", dat$path)), ]
# --> these patients yielded an error in the following function

for(i in 1:nrow(dat)){
  path_split = strsplit(dat$path[i], ".dcm")[[1]]
  path_split = strsplit(path_split, "\\\\")[[1]]
  path_split = strsplit(path_split[length(path_split)], "_")[[1]]
  dat$folder[i] = path_split[length(path_split)-1]
  dat$image[i] = path_split[length(path_split)]
}

# patients with folder manualexport have wrong folder and image names...
wrong = dat[dat$folder == "OLEA",] 
wrong = dat[dat$folder == "trace",] 
wrong = dat[dat$folder == "p2",]
wrong = dat[dat$folder == "1000",]
wrong = dat[dat$folder == "DIN",]
wrong = dat[dat$folder == "ep2d",]
wrong = dat[dat$folder == "diffusion",]
# --> so far this shouldn't be a problem


# check if we have multiple folders per modality, then we delete one
delete_folders = data.frame(p_id = NA, modality = NA, folder = NA)
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  folders = pat %>% group_by(modality) %>% distinct(folder, .keep_all = T)
  if(any(duplicated(folders$modality))){
    delete_folders = rbind(delete_folders, as.data.frame(folders[duplicated(folders$modality),c("p_id", "modality", "folder")]))
  }
}
delete_folders = na.omit(delete_folders)
delete_folders

# dat0 = dat
for(i in 1:nrow(delete_folders)){
  dat = dat[-which(dat$p_id == delete_folders$p_id[i] & dat$modality == delete_folders$modality[i] & dat$folder == delete_folders$folder[i]), ]
}



# Check if we still have all the perfusion maps for the patients
length(unique(dat$p_id[dat$modality == "ADC"])) # n = 223
length(unique(dat$p_id[dat$modality == "DWI"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBF"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBV"])) # n = 223
length(unique(dat$p_id[dat$modality == "TMAX"])) # n = 223
length(unique(dat$p_id[dat$modality == "TTP"])) # n = 223
length(unique(dat$p_id[dat$modality == "MTT"])) # n = 223


# Make sure that we have the same number of DWIs and ADCs
# Exclude the b0 images

# Get the number of images per perfusion map
dat = dat %>% group_by(p_id, modality) %>% mutate(rank=n()) %>% ungroup()

dat0 = dat
pid_out = c()
nrow(dat) # 37780
for(pid in unique(dat$p_id)){
  # Consider ADC and DWI series of one patient
  pat = dat[dat$p_id == pid, ]
  pat = pat[pat$modality %in% c("DWI", "ADC"), ]
  pat = pat[order(pat$modality, pat$instance_no),]
  
  rank_adc = pat$rank[pat$modality == "ADC"][1]
  rank_dwi = pat$rank[pat$modality == "DWI"][1]
  
  # Delete the first part of the DWI sequence 
  # if the number of DWIs is twice as much as the number of ADC images
  # --> checked the images, b0 always comes first
  
  # If the number of DWIs is not twice as much as the number of ADCs
  # print the patient ID and check it
  if((rank_adc != rank_dwi) & (rank_adc == 0.5*rank_dwi)){
    dat = dat[-which(dat$p_id == pid & dat$modality == "DWI" & dat$instance_no <= rank_adc), ]
  } else{
    pid_out = c(pid_out, pid)
  }
}
nrow(dat) # 33817

# Check if we still have all the perfusion maps for the patients --> DWI missing
length(unique(dat$p_id[dat$modality == "ADC"])) # n = 223
length(unique(dat$p_id[dat$modality == "DWI"])) # n = 219
length(unique(dat$p_id[dat$modality == "CBF"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBV"])) # n = 223
length(unique(dat$p_id[dat$modality == "TMAX"])) # n = 223
length(unique(dat$p_id[dat$modality == "TTP"])) # n = 223
length(unique(dat$p_id[dat$modality == "MTT"])) # n = 223

# find the missing DWIs
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  if(!any(pat$modality == "DWI")){
    print(pid)
  }
}
# [1] 2614103
# [1] 6593860
# [1] 10977813
# [1] 11865539

# All patients have duplicated instance numbers per modality ADC and DWI
# --> make sure that we have only one sequence
dwi_path = c()
adc_path = c()
for(pid in c("2614103", "6593860", "10977813", "11865539")){
  pat = dat0[dat0$p_id == pid, ]
  dwi = pat[pat$modality == "DWI", ]
  adc = pat[pat$modality == "ADC", ]
  dwi_path = c(dwi_path, dwi$path[which(duplicated(dwi$instance_no))])
  adc_path = c(adc_path, adc$path[which(duplicated(adc$instance_no))])
  
}
# delete the duplicated instances in 
dat0 = dat0[-which(dat0$path %in% c(dwi_path, adc_path)), ]

# Assign the fixed data to dat
dat = dat0

# Update the rank
dat = dat %>% group_by(p_id, modality) %>% mutate(rank=n()) %>% ungroup()


dat0 = dat
pid_out = c()
nrow(dat) # 37480
for(pid in unique(dat$p_id)){
  # Consider ADC and DWI series of one patient
  pat = dat[dat$p_id == pid, ]
  pat = pat[pat$modality %in% c("DWI", "ADC"), ]
  pat = pat[order(pat$modality, pat$instance_no),]
  
  rank_adc = pat$rank[pat$modality == "ADC"][1]
  rank_dwi = pat$rank[pat$modality == "DWI"][1]
  
  # Delete the first part of the DWI sequence 
  # if the number of DWIs is twice as much as the number of ADC images
  # --> checked the images, b0 always comes first
  
  # If the number of DWIs is not twice as much as the number of ADCs
  # print the patient ID and check it
  if((rank_adc != rank_dwi) & (rank_adc == 0.5*rank_dwi)){
    dat = dat[-which(dat$p_id == pid & dat$modality == "DWI" & dat$instance_no <= rank_adc), ]
  } else{
    pid_out = c(pid_out, pid)
  }
}
nrow(dat) # 33817

# Check if we still have all the perfusion maps for the patients
length(unique(dat$p_id[dat$modality == "ADC"])) # n = 223
length(unique(dat$p_id[dat$modality == "DWI"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBF"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBV"])) # n = 223
length(unique(dat$p_id[dat$modality == "TMAX"])) # n = 223
length(unique(dat$p_id[dat$modality == "TTP"])) # n = 223
length(unique(dat$p_id[dat$modality == "MTT"])) # n = 223


# Find the DWI series that are not a duplicated of the ADCs
# pid_out

# Check if the DWI is always a multiple of the adc:
dat_dwi_adc = dat[dat$p_id %in% pid_out & dat$modality %in% c("DWI", "ADC"), ]
dat_dwi_adc = as.data.frame(unique(dat_dwi_adc[, c("p_id", "modality", "rank")]))

unique(dat_dwi_adc$rank[dat_dwi_adc$modality == "DWI"]/dat_dwi_adc$rank[dat_dwi_adc$modality == "ADC"])
# [1] 3
# b0 followed by 2 sequences


# keep only the last sequence
dat0 = dat
nrow(dat) # 33817
for(pid in pid_out){
  # Consider ADC and DWI series of one patient
  pat = dat[dat$p_id == pid, ]
  pat = pat[pat$modality %in% c("DWI", "ADC"), ]
  pat = pat[order(pat$modality, pat$instance_no),]
  
  rank_adc = pat$rank[pat$modality == "ADC"][1]
  rank_dwi = pat$rank[pat$modality == "DWI"][1]
  
  # Keep only the last part of the DWI series:
  dat = dat[-which(dat$p_id == pid & dat$modality == "DWI" & dat$instance_no <= rank_adc*2), ]
}
nrow(dat) # 31579


# Check if we still have all the perfusion maps for the patients
length(unique(dat$p_id[dat$modality == "ADC"])) # n = 223
length(unique(dat$p_id[dat$modality == "DWI"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBF"])) # n = 223
length(unique(dat$p_id[dat$modality == "CBV"])) # n = 223
length(unique(dat$p_id[dat$modality == "TMAX"])) # n = 223
length(unique(dat$p_id[dat$modality == "TTP"])) # n = 223
length(unique(dat$p_id[dat$modality == "MTT"])) # n = 223


# Update the rank after removing the DWIs
dat = dat %>% group_by(p_id, modality) %>% mutate(rank=n()) %>% ungroup()

# check if the number of DWIs is always the same as the number of ADCs now
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  rank_dwi = unique(pat$rank[pat$modality == "DWI"])
  rank_adc = unique(pat$rank[pat$modality == "ADC"])
  
  if(length(rank_dwi)>1 | length(rank_adc)>1){
    print(paste0("More than one series: ", pid))
  }
  
  if(rank_dwi != rank_adc){
    print(paste0("#DWI != #ADC: ", pid))
  }
}
# NO output: :)


# get the number of images per patient per modality
c(min(dat$rank[dat$modality == "DWI"]), max(dat$rank[dat$modality == "DWI"]))
# 19 30
c(min(dat$rank[dat$modality == "ADC"]), max(dat$rank[dat$modality == "ADC"]))
# 19 30
c(min(dat$rank[dat$modality == "CBF"]), max(dat$rank[dat$modality == "CBF"]))
# 19 46
c(min(dat$rank[dat$modality == "CBV"]), max(dat$rank[dat$modality == "CBV"]))
# 19 46
c(min(dat$rank[dat$modality == "TMAX"]), max(dat$rank[dat$modality == "TMAX"]))
# 19 46
c(min(dat$rank[dat$modality == "MTT"]), max(dat$rank[dat$modality == "MTT"]))
# 19 46
c(min(dat$rank[dat$modality == "TTP"]), max(dat$rank[dat$modality == "TTP"]))
# 19 46


# We always need the same number of perfusion maps
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  n_cbf = pat$rank[pat$modality == "CBF"][1]
  n_cbv = pat$rank[pat$modality == "CBV"][1]
  n_tmax = pat$rank[pat$modality == "TMAX"][1]
  n_mtt = pat$rank[pat$modality == "MTT"][1]
  n_ttp = pat$rank[pat$modality == "TTP"][1]
  if(any(c(n_cbv, n_tmax, n_mtt, n_ttp) != n_cbf)){
    print(paste("Consider patient: ", pid))
  }
}
# [1] "Consider patient: 516341"
pat = dat[dat$p_id == "516341",]

# TTP is included twice with images of size 256 and 512
# --> Delete images of size 512
dat = dat[-which(dat$p_id == "516341" & dat$modality == "TTP" & dat$columns == 512), ]

# Update the ranks
dat = dat %>% group_by(p_id, modality) %>% mutate(rank=n()) %>% ungroup()

# run again: should get ne output anymore
for(pid in unique(dat$p_id)){
  pat = dat[dat$p_id == pid, ]
  n_cbf = pat$rank[pat$modality == "CBF"][1]
  n_cbv = pat$rank[pat$modality == "CBV"][1]
  n_tmax = pat$rank[pat$modality == "TMAX"][1]
  n_mtt = pat$rank[pat$modality == "MTT"][1]
  n_ttp = pat$rank[pat$modality == "TTP"][1]
  if(any(c(n_cbv, n_tmax, n_mtt, n_ttp) != n_cbf)){
    print(paste("Consider patient: ", pid))
  }
}
# :)



# check if width and height are the same for all images
all(dat$columns == dat$rows) # TRUE

c(min(dat$columns[dat$modality == "DWI"]), max(dat$columns[dat$modality == "DWI"])) 
# 128 384
c(min(dat$columns[dat$modality == "ADC"]), max(dat$columns[dat$modality == "ADC"])) 
# 128 384
c(min(dat$columns[dat$modality == "CBF"]), max(dat$columns[dat$modality == "CBF"])) 
# 256 256
c(min(dat$columns[dat$modality == "CBV"]), max(dat$columns[dat$modality == "CBV"])) 
# 256 256
c(min(dat$columns[dat$modality == "TMAX"]), max(dat$columns[dat$modality == "TMAX"])) 
# 256 256
c(min(dat$columns[dat$modality == "MTT"]), max(dat$columns[dat$modality == "MTT"])) 
# 256 256
c(min(dat$columns[dat$modality == "TTP"]), max(dat$columns[dat$modality == "TTP"])) 
# 256 256

unique(dat$p_id[dat$columns == 384])
# 13707000 14239248


# Check if the patients are the same as in the csv file:
dat0 = read.xlsx(paste0(dir,"data/Clean_Olea_Analyzed_Data_20190304.xlsx"), sheet=1, na.strings="NA")
dat0 = dat0[1:222,] # Below 222, there are summary statistics from Excel
dat0 = plyr::rename(dat0, c("PID"="p_id",
                      "Alter.(Jahre)"="age",
                      "Sex.0=Mann.1=Frau" = "sex", 
                      "Selbständig.vor.Stroke:.mRS.0-2:.0=nein.1=ja" = "independent_pre_stroke",
                      "NIHSS.bei.Eintritt" = "nihss_bl",
                      "NIHSS.nach.24.h" = "nihss_24h",
                      "Delta.NIHSS.nach.24h.-.bei.Eintritt" = "nihss_diff_bl_24h",
                      "NIHSS.nach.3.Monaten" = "nihss_3months",
                      "Delta.NIHSS.nach.3.Monaten.-.bei.Eintritt" = "nihss_diff_bl_3months",
                      "mRS.nach.3.Monaten" = "mrs_3months",
                      "Tod.nach.3.Monaten.0=nein.1=ja" = "death_after_3months",
                      "Symptomat.Intacranielle.Blutung.(SITS-MOST).0=nein.1=ja" = "intracranial_bleeding",
                      "TAH.(ASS.oder.Clopidogrel.oder.Ticagrelor).pre-stroke.0=nein.1=ja" = "tah_pre_stroke",
                      "Orale.Antikoagulation.pre-stroke.0=nein.1=ja" = "antikoagulation_pre_stroke",
                      # Wirkstoff in Medikamenten zum Blutfettwerte zu senken
                      "Statin.pre-stroke.0=nein.1=ja" = "statin_pre_stroke",
                      # Medikamente gegen Bluthochdruck
                      "Antihypertensiva.pre-stroke.0=nein.1=ja" = "antihypertensiva_pre_stroke",
                      "systolischer.Blutdruck.bei.Eintritt.[mmHg]" = "sys_bloodpressure_bl",
                      "diastolischer.Blutdruck.bei.Eintritt.[mmHg]" = "dias_bloodpressure_bl",                                                                                                                         
                      "Glucose.bei.Eintritt.[mmol/l]" = "glucose_bl",                                                                                                                                         
                      "HbA1c.bei.Eintritt.[%]" = "hba1c",                                                                                                                                         
                      "LDL-Cholesterin.bei.Eintritt.[mmol/l]" = "ldl",                                                                                                                               
                      "HDL-Cholesterin.bei.Eintritt.[mmol/l]" = "hdl",                                                                                                                                 
                      "Triglyceride.bei.Eintritt.[mmol/l]" = "triglyceride",                                                                                                                                   
                      "CRP.bei.Eintritt.[mg/l]" = "crp",                                                                                                                                               
                      "INR.bei.Eintritt" = "inr",                                                                                                                                             
                      "Anästhesie.0=Sedation/Keine.1=Vollnarkose" = "anaesthesia",                                                                                                                             
                      "Hyperdenses.Media-Zeichen.0=nein.1=ja" = "dense_media",                                                                                                                              
                      "Infarktfrühzeichen.0=nein.1=ja" = "early_infarct_sign",
                      "TOAST.1=Makroangiopathie.2=kardioembolisch.3=Mikroangiopathie.4=Andere.5=ungeklärt.(Abklärung.komplett).6=ungeklärt.(Abklärung.inkomplett).7=Mehrere.mögliche.Ursachen" = "toast",
                      "bekannte.RF:.Vorhofflimmern.0=nein.1=ja" = "atrial_fibrillation",
                      "bekannte.RF:.Diabetes.mellitus.0=nein.1=ja" = "rf_diabetes",
                      # Bluthochdruck
                      "bekannte.RF:.Hypertonie.0=nein.1=ja" = "rf_hypertonia",
                      # zu hohe Blutfettwerte
                      "bekannte.RF:.Hypercholesterinämie.(Gesamtcholesterin.>5.0.mmol/l.oder.behandelt.mit.Statin).0=nein.1=ja" = "rf_hypercholesterinaemie",
                      "bekannte.RF:.Rauchen.(aktuell).0=nein.1=ja" = "rf_smoker",
                      "bekannte.RF:.Koronare.Herzkrankheit.0=nein.1=ja" = "rf_chd",
                      "bekannte.RF:.pAVK.0=nein.1=ja" = "rf_pavk",
                      "bekannte.RF:.früherer.Stroke.OR.TIA.0=nein.1=ja" = "rf_tia_stroke",
                      "M1.Verschluss.Seite.1=links.2=rechts" = "infarct_side",
                      # weitere Verschlüsse
                      "Weitere.Verschlüsse.(Ort.2).0=nein.1=ja" = "additional_occlusion",
                      # Ort2.Seite.1=links.2=rechts.3=bds.4=unklar
                      "Ort2.Seite.1=links.2=rechts.3=bds.4=unklar" = "additional_occlusion_side",
                      "Ort.2.ICA.extracranial.0=nein.1=ja" = "additional_occlusion_ica_excranial",
                      "Ort.2.ICA.intracranial.0=nein.1=ja" = "additional_occlusion_ica_intracranial",
                      "Ort.2.ICA.T.0=nein.1=ja" = "additional_occlusion_ica_t",
                      "Ort.2.MCA.M2.0=nein.1=ja" = "additional_occlusion_mca_m2",
                      "Ort.2.MCA.M3.or.M4.0=nein.1=ja" = "additional_occlusion_mca_m2_m3",
                      "Kollateralisierung.0=schlecht/keine.1=mittel/mässig.2=gut/sehr.gut" = "collateralization",
                      # Lyse = Medikament das gegeben wird
                      "Lyse.0=nein.1=ja"  = "lyse",
                      "Konventionelle.Angiographie.0=nein.1=ja" = "angio",
                      # Thormbectomy = Behandlung
                      "Thrombektomie.(mechanisch).0=nein.1=ja" = "thrombectomy",
                      # Onset to imaging (CT/MRI) time (min.)
                      # time between onset and imaging
                      "Onset.to.imaging.(CT/MRI).time.(min.)" = "time_to_imaging",
                      "Onset.to.needle.time.(min.)" = "time_to_needle",
                      "Onset.to.groin.puncture.(min.)" = "time_to_groin_puncture",
                      # Gefäss vor Therapie wieder offen
                      "Gefäss.bei.Beginn.Angio.wieder.offen?.0=nein.1=ja" = "vessel_open_before_therapy",
                      # Perfusion nach Therapie --> wie erfolgreich war die Therapie?
                      "Recanalization.TICI.AFTER.IAT.0=.Grade.0,.1=.Grade.1.,.2=.Grade.2a,.3=.Grade.2b,.4=.Grade.3" = "tici",
                      "Recanalization.TICI.AFTER.IAT.dichotomisiert.0=schlecht.(mTICI0-2a).1=gut.(mTICI.2b-3)" = "tici_binary"))

# check if the same patients are available
pid_dat0 = unique(dat0$p_id)
pid_dat0 = as.character(pid_dat0)
pid_dat = unique(dat$p_id)
pid_dat = as.character(pid_dat)

# remove/add nulls in front of the p_id strings to make them comparable
pid_dat0_new = c()
for(pid in pid_dat0){
  pid_dat0_tmp = strsplit(pid[1],"")[[1]]
  k = min(which(pid_dat0_tmp != "0"))
  pid_dat0_new = c(pid_dat0_new, paste0(pid_dat0_tmp[k:length(pid_dat0_tmp)], collapse = ""))
}

library(plyr)
dat0$p_id_old = dat0$p_id
dat0$p_id = mapvalues(dat0$p_id, from=pid_dat0, to=pid_dat0_new)
test = dat0[,c("p_id","p_id_old")]


# Which patient have MRI files but are not included in the CSV file?
included = c()
not_included = c()
for(i in pid_dat){
  if(i %in% pid_dat0_new){
    included = c(included,i)
  } else{
    not_included = c(not_included,i)
  }
}
length(included) # n = 222
length(not_included) # n = 1: 11420316

# remove the patient from the dataset
dat0 = dat0[dat0$p_id != "11420316",]
dat = dat[dat$p_id != "11420316",]


# For which patients from the CSV file do we have the MRI information?
included = c()
not_included = c()
for(i in pid_dat0_new){
  if(i %in% pid_dat){
    included = c(included,i)
  } else{
    not_included = c(not_included,i)
  }
}
length(included) # n = 222
length(not_included) # n = 0

dat00 = merge(dat, dat0, by = "p_id")
write.table(dat00, file = paste0(dir, "/data/data_to_read_in.csv"), sep=",", row.names = F)
