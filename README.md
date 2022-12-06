# Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data

This repository contains the code to the paper "Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data".

Clinical variables and imaging data can not be made available for reproducibility due to privacy concerns. However, we provide intermediate results to reproduce large parts of the results and the figures in the paper.

## Structure

`./python`: Contains the code for developing and evaluating the prediction models

- `classification_models_3D_master`: Code for the 3D ResNets that are used to model the imaging data in the ONTRAMs. The code is copied and only slightly adapted from the repository [ZFTurbo/classification_models_3D](https://github.com/ZFTurbo/classification_models_3D).
- `functions`: Some helper functions including code for 3D data augmentation and for plotting the 3D imaging data.
- `k_ontram_functions`: Code for defining and training the ONTRAMs.
- `callbacks`: Test predictions obtained in the cross-validation
- All code to define, train and validate the models is contained in the two notebooks:
  - `3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet.ipynb`: Models based on clinical variables (SI_LSx), imaging data in terms of diffusion weighted imaging (SI_CSb) and a combination of both (SI_LSx_CSb). 
  - `3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet_AddTMAX.ipynb`: Models based on TMAX perfusion maps (SI_CSb_TMAX) and TMAX perfusion maps + DWI (SI_CSb_TMAX_DWI).

`./R`: Contains the code for reproducing figures and results presented in the manuscript

`./data`: Contains the intermediate results for the different models in terms of

- test predictions for each fold of the 5-fold CV: `trafo_`
- all test predictions for one model resulting from the 5-fold CV: `test_`
- estimates for the clinical variables obtained from the linear shift parts of the ONTRAMs: `estimates`
- standard deviations corresponding to the normalized clinical variables: `estimates_sd`
- negative log likelihood values in the 5-folds of the CV: `nll_`

The ending `_bin` indicates the predictions for the binarized outcome.
