<p align="center">
<img src="./img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

## On the importance of soil texture for predicting future global soil respiration
Welcome to the repository dedicated to the `R language` source code for the scientific paper **On the importance of soil texture for predicting future global soil respiration**. This repository provides R scripts and datasets used to analyze and model the influence of soil texture on global soil respiration under future climate change scenarios.

### Objective:
Our objective with this study was to explore the influence of soil texture on future global soil respiration (Rs and Rh) under climate change scenarios up to 2100, considering three Shared Socioeconomic Pathways (SSP126, SSP370, and SSP585). The analysis examines how the inclusion or exclusion of soil texture-related variables affects the predictions of Rs and Rh, allowing for the assessment of their combined effects with climate change over time.

### Repository content:
#### R Scripts:
- [Distribution of samples in ecoregions](./pages/distribution_samples.md)<br>
- [Global soil respiration model training with data division restriction](./pages/global_model_ckfold.md)<br>
- [Global soil respiration model training without restriction on data division](./pages/global_model_skfold.md)<br>
- [Performance soil respiration model training with data division restriction](./pages/performance_model_ckfold.md)<br>
- [Performance soil respiration model training without restriction on data division](./pages/performance_model_skfold.md)<br>
- [Global soil respiration model training without restriction on data division](./pages/comparison_models.md)<br>
- [Global soil respiration model training without restriction on data division](./pages/comparison_models.md)<br>

#### Datasets:
- [Input data samples used in the study](./datasets/dataset_soil_respiration.csv)<br>
- [Final dataset for model fitting and result reproduction](./datasets/final_model_data.csv)<br>

#### Results:
The final maps (mean, quantiles, and coefficient of variation) of soil respiration projections and their associated uncertainties are available for download on Zenodo:<br>
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.xxxxxxxx.svg)](https://doi.org/10.5281/zenodo.xxxxxxxx)

### Attention:
The authors are not obligated to provide user support, updates, or bug fixes.

### How to contribute:
For more information or collaboration opportunities, please contact us at:  
📧 **lucas.gomes@agro.au.dk**  
📧 **cassiomoquedace@gmail.com**  
📧 **labgeo@ufv.br**

### Acknowledgments:
We appreciate your interest in this research. We hope this repository facilitates result replication and contributes to future studies on soil carbon modeling and the application of machine learning in global soil respiration projections.

### Reference:
Gomes, L., Moquedace, C. M., Souza, I. F., Vesterdal, L., Veloso, G. V., Francelino, M. R., Schaefer, C., Morris, K. A., Vargas, R., Bond-Lamberty, B., & Fernandes Filho, E. I. (2025). On the importance of soil texture for predicting future global soil respiration. *Journal of Geophysical Research - Biogeosciences*, xx(x), xx. DOI: [10.1016/xxxxxxxxxxx](https://doi.org/10.1016/xxxxxxxxxxx)
