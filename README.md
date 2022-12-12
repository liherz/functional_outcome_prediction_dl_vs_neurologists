# Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data

This repository contains the code to the paper "Deep Learning vs. Neurologists: Functional Outcome Prediction in Large Vessel Occlusion Stroke Patients Based on Clinical and Imaging Data".

# Reading and preprocessing imaging data

The imaging data was read in and preprocessed with the following files:

1. `./python/read_bern_dicoms01.ipynb`: Code to read in all DICOMs from an external drive
2. `./R/read_bern_dicoms02.R`: Code to exatract the patients that fullfill the study requirements
3. `./python/read_bern_dicoms03.ipynb`: Code to read in the DICOMs that fullfill the study requirements from the external drive
4. `./python/preprocessing_bern_dicoms.ipynb`: Code to preprocess the DICOMs.


# Prediction models based on clinical variables, imaging data and a combination of both

All models described in the paper correspond to the class of ordinal neural network transformation models (ONTRAMs). ONTRAMs are interpretable deep learning based models which can input structured and unstructured data while they provide interpretable parameter estimates for the respective input. A detailed introduction into ONTRAMs can be found in the paper ["Deep and interpretable regression models for ordinal outcomes"](https://www.sciencedirect.com/science/article/pii/S003132032100443X).

In this paper, we developed ONTRAMs for acute ischemic stroke patients which predict functional outcome in terms of the modified Rankin Scale (mRS) three months after hospital admission. The models are either based on clinical variables, imaging data or a combination of both. All models are trained to predict the seven classes of the mRS and are evaluated in a five-fol cross validation (CV). The models are implemented in python and contained in the two notebooks:

- `./python/ontrams_3d_resent_clinical_dwi.ipynb`: Models based on clinical variables (SI-LSx), imaging data in terms of diffusion weighted imaging (SI_CSb) and a combination of both (SI_LSx_CSb).
- `./python/ontrams_3d_resent_dwi_tmax.ipynb`: Models based on TMAX perfusion maps (SI_CSb_TMAX) and a combination of DWI and TMAX perfusion maps (SI_CSb_TMAX_DWI).

Other relevant code to define, train and evaluate the models that is used in the two notebooks is contained in the folders:

- `.python/classification_models_3D_master`: The neural network architectures for the imaging data in the ONTRAMs are mainly based on 3D ResNets which are initialized with transformed 2D weightes of 2D ResNets extracted from ImageNet. The code for that is copied and only slightly adapted from the repository [ZFTurbo/classification_models_3D](https://github.com/ZFTurbo/classification_models_3D) and stored in the folder:
- `.python/functions`: Some helper functions including code for 3D data augmentation and for plotting the 3D imaging data.
- `./python/k_ontram_functions`: Code for defining and training the ONTRAMs.


# Intermediate results

Clinical variables and imaging data can not be made available for reproducibility due to privacy concerns. However, we provide intermediate results to reproduce large parts of the results and the figures in the paper. The predictions from the five-fold CV corresponding to the respective model are stored in the folders with the same names as the notebooks:

- `./callbacks/ontrams_3d_resent_clinical_dwi/`:
  - `SI-LSx`: Clinical variables
  - `SI-CSb`: Imaging data in terms of DWI
  - `SI-LSx-CSb`: Clinical variables and imaging data in terms of DWI
- `./callbacks/ontrams_3d_resent_dwi_tmax/`:
  - `SI_CSb_TMAX`: Imaging data in terms of TMAX perfusion maps
  - `SI_CSb_TMAX_DWI`: Imaging data in terms of TMAX perfusion maps and DWI

The folders contain the following results:

- `trafo_`: test predictions for each fold of the 5-fold CV
- `test_`: all test predictions for the corresponding model resulting from the 5-fold CV
- `estimates`: estimates for the clinical variables obtained from the linear shift parts of the ONTRAMs
- `estimates_sd`: standard deviations corresponding to the normalized clinical variables. They are avialable for each fold because they were nomalized based on the training data of the respective fold
- `nll_`: negative log likelihood values obtained in each of the five folds of the CV

The ending `_bin` indicates the predictions for the binarized outcome.


# Evaluation and visualization

To obtain confidence intervals, we do bootstrapping...

Bootstrapping results, interrater agreement and figures are obtained with `R`:
