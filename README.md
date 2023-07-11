# UNICEF-WHO LBW and Preterm birth estimates (2020) analysis

This repository contains the code and input data required to generate the WHO/UNICEF LBW and Preterm birth 2020 estimates [[1]](#1) [[2]](#2) [[3]](#3), and the regional and global LBW and Preterm outputs.

The background, methods and results for these analyses can be found in the corresponding papers published in The Lancet [[1]](#1) [[3]](#3).

## Procedure for Generating LBW and Preterm Modelled Estimates

### Contents

1. LBW Bayesian Modelling Code
- functions: folder containing all the functions needed for the modelling code
- inputs: folder containing all of the input data needed for the LBW estimates. This includes the LBW admin and survey data, and the WPP livebirths numbers. [[4]](#4)
- models: folder containing <b>".txt"</b> files containing the code for the JAGS model
- output: folder containing all outputs of the analysis.
- R code which contains the order at which to run the R scripts for the modelling (<b>"0.master.R"</b>)
- R code which contains the file names for all the input data (<b>"0.fileNames.R"</b>)
- R code which contains the packages needed for the code (<b>"0.loadPackages.R"</b>) 
- 1-7 numbered R scripts: contatining all of the commands needed set up the project, create the input database, run the model, produce the country, regional and global estimates, create country estimates, and run the model validation.

2. LBW Regional and Global Estimates
- <b>".csv"</b> files containing LBW regional and global estimates, for 12 different regional groupings.

3. Preterm Bayesian Modelling Codes
- functions: folder containing all the functions needed for the modelling code
- inputs: folder containing all of the input data needed for the Preterm birth estimates. This includes the LBW admin and survey data, and the WPP livebirths numbers. [[4]](#4)
- models: folder containing <b>".txt"</b> files containing the code for the JAGS model
- output: folder containing all outputs of the analysis.
- R code which contains the order at which to run the R scripts for the modelling (<b>"0.masterP.R"</b>)
- R code which contains the file names for all the input data (<b>"0.fileNames.R"</b>)
- R code which contains the packages needed for the code (<b>"0.loadPackages.R"</b>) 
- 1-7 numbered R scripts: contatining all of the commands needed set up the project, create the input database, run the model, produce the country, regional and global estimates, create country estimates, and run the model validation.
  
4. Preterm Regional and Global Estimates
- <b>".csv"</b> files containing Preterm birth regional and global estimates, for 12 different regional groupings.

### Set-up
For the chosen estimate: 
1. Open the "0.loadPackages.R" file, and ensure all packages are installed. 
2. Run the "0.loadPackages.R" file and "0.fileNames.R" file.
3. Open the "0.master.R" file and run the files in the indicated order.
All of the outputs are written to the "outputs" folder.

Any questions on running the code please email Ellen Bradley (ellen_bradley@outlook.com).

## Model 
The models used in both estimates are heirarchical bayesian regression models, incorporating country-specific interepts, covariates, penalised splines to capture non-linear time trends and bias adjustments based on a data quality categorisation and other data quality indicators.

## Acknowledgements
### Code 
We thank Ellen Bradley (@ellenBradley18) for the development and implementation of the two model codes. 

### Conceptualisation of the Models
We thank Ellen Bradley, Eric Ohuma, Alexandra Lewin, Yemi Okwaraji, Hannah Blencowe and Joy Lawn (LSHTM), Julia Krasevec, Joel Conkle, Samuel Chakwera, Jennifer Requejo and Chika Hayashi (UNICEF), Gretchen Stevens, Ann-Beth Moller, Jenny A Cresswell, Emily White Johansson and Allisyn Morran (WHO) and Laith Hussain-Alkhateeb.

## References
<a id="1">[1]</a> 
Okwaraji Y, Krasevec J, Bradley E et al. National, regional and global estimates of low birthweight in 2020, with trends from 2000: a systematic analysis. Lancet (in press).

<a id="2">[2]</a>
United Nations Children’s Fund and the World Health Organization. UNICEF-WHO low birthweight estimates: Levels and trends 2000–2020. New York: UNICEF; 2023 Licence: CC BY-NC-SA 3.0 IGO.

<a id="3">[3]</a> 
Ohuma E, Moller A-B, Bradley E et al. National, regional and global estimates of preterm birth in 2020, with trends from 2010: a systematic analysis. Lancet (in press).

<a id="4">[4]</a> 
United Nations Department of Economic and Social Affairs Population Division. World Population Prospects 2022, Online Edition. 2022 [Available from: https://population.un.org/wpp/Download/Standard/Population/]

