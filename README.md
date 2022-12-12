# Deep Learning vs. Neurologists

This repository contains the code to the paper "Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data".

## Reading and preprocessing imaging data

The imaging data was read in and preprocessed with the following files:

1. `./python/read_bern_dicoms01.ipynb`: Code to read in all DICOMs from an external drive
2. `./R/read_bern_dicoms02.R`: Code to exatract the patients that fullfill the study requirements
3. `./python/read_bern_dicoms03.ipynb`: Code to read in the DICOMs that fullfill the study requirements from the external drive
4. `./python/preprocessing_bern_dicoms.ipynb`: Code to preprocess the DICOMs.


## Prediction models based on clinical variables, imaging data and a combination of both

All models described in the paper correspond to the class of ordinal neural network transformation models (ONTRAMs). ONTRAMs are interpretable deep learning based models which can input structured and unstructured data while they provide interpretable parameter estimates for the respective input. A detailed introduction into ONTRAMs can be found in the paper ["Deep and interpretable regression models for ordinal outcomes"](https://www.sciencedirect.com/science/article/pii/S003132032100443X).

In this paper, we developed ONTRAMs for acute ischemic stroke patients to predict functional outcome in terms of the modified Rankin Scale (mRS) three months after hospital admission. The models are either based on clinical variables, imaging data or a combination of both. All models predict the seven classes of the mRS and are evaluated in a five-fold cross validation (CV). The models are implemented in python. The code is in the folder `./python`, in the two notebooks:

- `./ontrams_3d_resent_clinical_dwi.ipynb`: Models based on clinical variables (SI-LSx), imaging data in terms of diffusion weighted imaging (SI_CSb) and a combination of both (SI_LSx_CSb).
- `./ontrams_3d_resent_dwi_tmax.ipynb`: Models based on TMAX perfusion maps (SI_CSb_TMAX) and a combination of DWI and TMAX perfusion maps (SI_CSb_TMAX_DWI).

Other relevant code to define, train and evaluate the models that is used in the two notebooks is contained in the folders:

- `./classification_models_3D_master`: The neural network architectures for the imaging data in the ONTRAMs are mainly based on 3D ResNets which are initialized with transformed 2D weightes of 2D ResNets extracted from ImageNet. The code for that is copied and only slightly adapted from the repository [ZFTurbo/classification_models_3D](https://github.com/ZFTurbo/classification_models_3D) and stored in the folder:
- `./functions`: Some helper functions including code for 3D data augmentation and for plotting the 3D imaging data.
- `./k_ontram_functions`: Code for defining and training the ONTRAMs.


## Intermediate results

Clinical variables and imaging data can not be made available due to privacy concerns. However, we provide intermediate results to reproduce large parts of the results and the figures in the paper. The intermediate results are stored in the folder `./callbacks`.

#### Prediction models

The results of the models from the five-fold CV are stored in folders with same names as the notebooks:

- `./ontrams_3d_resent_clinical_dwi`:
  - `./SI_LSx`: Results from the model based on clinical variables
  - `./SI_CSb`: Results from the model based on DWI
  - `./SI_LSx_CSb`: Results from the model based on clinical variables and DWI
- `./ontrams_3d_resent_dwi_tmax/`:
  - `./SI_CSb_TMAX`: Results from the model based on TMAX perfusion maps
  - `./SI_CSb_TMAX_DWI`: Results from the model based on TMAX perfusion maps and DWI

In each model folder, there is a file `test_`, containing all test predictions of the 5-fold CV. The ending `_bin` of the files indicates that the file contains the predictions for the binarized outcome. The predictions for the binarized outcome are obtained by summing up the predictions for the mRS classes 0-2 vs. 3-6. Each model folder additionally contains 5 folders (`fold0`, ..., `fold4`) summarizing the test results from the 5-fold CV for each fold:
- `./trafo_`: test predictions
- `./estimates`: estimates for the clinical variables resulting from the linear shift parts of the ONTRAMs
- `./estimates_sd`: standard deviations corresponding to the normalized clinical variables
- `./nll_`: negative log likelihood values


#### Raters

The predictions of the models were compared to predictions of five raters which are contained in the folder

- `./results_raters`: Results of the raters based on 
  - `./dat_clinical.csv`: clinical data
  - `./dat_imaging.csv`: imaging data
  - `./dat_clinical_and_imaging.csv`: clinical and imaging data


## Evaluation and visualization

Results of models and raters were analysed in R. The code is in the folder `./R`, which is structured as follows:

- `./calculate_bootstrap_results.R`: Code to obtain the 95% bootstrap confidence intervals for the respective metrics. The code is mainly based on the functions
  - `./functions/bootstrapping.R`
  - `./functions/metrics.R`
- `./irr.R`: Code to calculate inter-rater reliability.
- `./visualization.R`: Code to reproduce the figures in the paper. Functions for visualization are contained in the files:
  - `./functions/helper.R`
  - `./functions/calibration.R`
