# Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data

This repository contains the code to the paper "Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data".

Unfortunately, clinical variables and imaging data can not be made available due to privacy concerns. However, we provide intermediate results in terms of test predictions resulting from the models evaluated in the 5-fold cross-validation setting as well as the prediction of the rating competition.

## Structure

`/.python`: Contains the code for developing and evaluating the prediction models

- `classification_models_3D_master`: Code for the 3D ResNets that are used to model the imaging data in the ONTRAMs. The code is copied and only slightly adapted from the repository [ZFTurbo/classification_models_3D](https://github.com/ZFTurbo/classification_models_3D).
- `functions`: Some helper functions including code for 3D data augmentation and for plotting the 3D imaging data.
- `k_ontram_functions`: Code for defining and training the ONTRAMs.
- `callbacks`: Test predictions obtained in the cross-validation
- `3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet.ipynb`: Notebook containing the code for defining, training and evaluating the models based on clinical variables (SI-LSx), imaging data in terms of diffusion weighted imaging (SI-CSb) and a combination of both (SI-LSx-CSb). 
- `3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet_AddTMAX.ipynb`: Notebook containing the code for defining, training and evaluating the models based on TMAX perfusion maps (SI-CSb-TMAX), Tmax perfusion maps + DWI (SI-CSb-TMAX-DWI) and TMAX perfusion maps, DWI + clinical variables (SI-LSx-CSb-TMAX-DWI).

`./R`: Contains the code for reproducing the figures and the results presented in the manuscript
