
This is the code to accompany the in preparation manuscript: Rainfall, density, and mutualistic rhizobia tip the balance between persistence or competitive exclusion of a native legume with an invasive grass. 


Analysis steps
1. Clean raw data
2. Calculate interactions
    a. via additive intensity index
    b. via Bayesian population models
3. Structural Equation Model of additive intensity index
4. Coexistence analyses of Bayesian population models

## 1. Clean raw data
Code is located in the **data_cleaning folder**.
- **clean_model_dat.R** cleans and formats vegetation biomass data and calculates seed output from biomass using allometric relationships.
- **clean_CN_dat.R** cleans leaf CN data.

## 2. Calculate Interactions
Code is located in the **calculate_interactions** folder, which then splits into a folder for the **additive intensity index** and a folder for the **population_models**. 

within the **additive intensity index** folder
- **additive intensity index.R** calculates the index


Structural Equation Modeling
- To-Do! using the response ratio and leaf CN data


