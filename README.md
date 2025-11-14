
This is the code to accompany the in preparation manuscript: Rainfall, density, and mutualistic rhizobia tip the balance between persistence or competitive exclusion of a native legume with an invasive grass. 


Analysis steps
1. Clean raw data
2. Calculate interactions
    a. via additive intensity index
    b. via Bayesian population models
3. Structural Equation Model of additive intensity index
4. Coexistence analyses of Bayesian population models

## 1. Clean raw data
Code is located in the **1_data_cleaning** folder.
- **clean_model_dat.R** cleans and formats vegetation biomass data and calculates seed output from biomass using allometric relationships. The data frames created from this are used in calculating interactions.
- **clean_CN_dat.R** cleans leaf CN data.

## 2. Calculate Interactions
Code is located in the **2_calculate_interactions** folder. Interactions are calculated in two different ways: by the **additive intensity index** and via Bayesian **population_models**, each in a respective folder.

Within the **additive intensity index** folder
- **additive intensity index.R** calculates the index

Within the **population models** folder, scripts are separated into folders for fitting or evaluating models. 
- The **1_fit_models** folder contains 
    - a script for each species, that runs the STAN code for population models (*1_fit_ACAM_models.R* or *2_fit_BRHO_models.R*). 
    - a **model_stan_scripts** folder with the .stan script for each species (ACAM, BRHO) and each type of interaction (static, sigmoidal) for a total of four model scripts.
    - a *shinystan_model_eval.R* script for quick visualization of model output

- The **2_evaluate** folder contains: 
    - a script to load saved posterior distributions, *load_models.R*
    - a script to calculate the log likelihood diagnostic for each model, *log_likelihood.R*
    - a script to 

Structural Equation Modeling
- To-Do! using the response ratio and leaf CN data


