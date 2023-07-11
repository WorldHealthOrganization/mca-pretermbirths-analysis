# WHO/UNICEF LBW and Preterm birth estimates (2020) analysis

This repository contains the code and input data required to generate the WHO/UNICEF LBW and Preterm birth 2020 estimates [[1]](#1) [[2]](#2) [1,2].
It also contains the regional and global LBW and Preterm outputs.

For each of the estimate code folders (LBWBayesianModelling and PretermBayesianModelling), there is a "0.master.R" file that contains all of the commands needed set up the project, create the input database, run the model, produce the country, regional and global estimates, create country estimates, and run the model validation.

The regional and global outputs from these codes can be found in the "LBW regional and global estimates" and "Preterm regional and global estimates".

The background, methods and results for these analyses can be found in the corresponding papers published in The Lancet [1,2].
Any questions on running the code please email Ellen Bradley (ellen_bradley@outlook.com).

## Model 
The models used in both estimates are heirarchical bayesian regression models, incorporating country-specific interepts, covariates, penalised splines to capture non-linear time trends and bias adjustments based on a data quality categorisation and other data quality indicators.

## Acknowledgements
### Code 
We thank Ellen Bradley (@ellenBradley18) for the development and implementation of the two model codes. 

## Conceptualisation of the Model
We thank the LSHTM team (Ellen Bradley, Eric Ohuma, Alexandra Lewin, Yemi Okwaraji, Hannah Blencowe and Joy Lawn), the UNICEF team (Julia Krasevec, Joel Conkle, Samuel Chakwera, Jennifer Requejo and Chika Hayashi), the WHO team (Gretchen Stevens, Ann-Beth Moller, Jenny A Cresswell, Emily White Johansson and Allisyn Morran) and Laith Hussain-Alkhateeb.

## References
<a id="1">[1]</a> 
Okwaraji Y, Krasevec J, Bradley E et al. National, regional and global estimates of low birthweight in 2020, with trends from 2000: a systematic analysis. Lancet (in press).

<a id="2">[2]</a> 
Ohuma E, Moller A-B, Bradley E et al. National, regional and global estimates of preterm birth in 2020, with trends from 2010: a systematic analysis. Lancet (in press).
