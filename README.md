
## Index
_ `README.md`: an overview of the project    
|___ `data`: data files used in the project   
|___ `processed_data`: intermediate files from the analysis   
|___ `results`: results of the analyses (data, tables, figures)    
|___ `R`: contains all R-code in the project   


# SAMexplore
Questionnaire scoring, preprocessing, and explorative analyses on SAM study data

The results of this project are described in: Sep MSC, Joëls M, Geuze E. Individual differences in the encoding of contextual details following acute stress: An explorative study. Eur J Neurosci. 2020; ejn.15067. [doi:10.1111/ejn.15067](https://doi.org/10.1111/ejn.15067)

*Note, `source` only works properly in a R markdown file, if Rstudio > Tools > Global options > R Markdown > evaluate chunks in directory = project.
([see](https://stackoverflow.com/questions/34029611/how-to-use-objects-from-global-environment-in-rstudio-markdown)) AND if ```source``` is not in 'setup' chunk*


## Scoring of the personality, life events, and state questionnaires
- script: `SAM_netq_calculate_scores.RMD` and `SAM_netq_function_scoring.r`
- input:
     - `SAM Questionnaire Masterfile.sav`
     - `SAM_MCT.csv`
     - `SAM_FGT.csv`
     - `SAM_Codes_Task_Protocol_Versions.csv`
- actions: descriptive statistics MCT and FGT papers of the SAM study and questionnaire scoring:
     - symptoms: SCL90
     - (early) life adversity) CTQ, LSCR
     - Personality: STAI-T, s-TCI, HEXACO
     - emotional state: VAS, STAI-S, PANAS
- output dataset: `SAM_netq_scored.rds`

## Merge questionnaire, endocrine, memory data
- script: `SAMexplore_data_merge.R`
- input: 
    - Questionnaires: `SAM_netq_scored.rds`
    - Endocrine: `sAA_sCORT_condition_dataset.rds` (available via [this repository](https://github.com/mscsep/SAM_sAA_Cortisol.git))
    - Memory: `SAM_MCT.csv` & `SAM_FGT.csv`
- actions: merge data from different sources and calculate context-memory summary scores  (neutral, emotional & fearful)
- output: `SAM_complete.rds`

## Imputation of missing values
- script: `SAMexplore_imputation.Rmd`
- input: `SAM_complete.rds`
- actions: (passive) imputations of missing values
- output:
    - imputed data: Direct imputation is saved in `impTS.rds`, Passive imputation is saved in `pas.imp.rds`
    - combined imputed data: `implist.rds`

## Preprocessing for analyses in the SAM explore project
1) **Variable reduction: AUC**
  - script: `SAMexplore_AUC_calculations.RMD`
  - input: 
    - `implist.rds`
    - `SAM_Timepoints_reactivity_measures.csv`
  - actions: Calculation AUCg and AUCi for repeated measures of emotional/stress state (self-report & endocrine) 
  - output: `SAMimputed_with.AUC.variables.RDS`
2) **Standardization: Z-scores**
  - script: `SAMexplore_prepare.for.analyses.r`
  - input: `SAMimputed_with.AUC.variables.RDS`
  - actions:
    - variable selection
    - z-scrore transformation (of all numeric variables)
    - Calculation cumulative stress exposure score [Kok & Sep et al. 2016](https://www.sciencedirect.com/science/article/pii/S0165032716304530?via%3Dihub)
    - split data per experimental condition
  - output:
    - lists with imputed data per experimental condition:
        - No-stress group: `imp.nos`
        - Immediate stress group: `imp.ims`
        - Delayed stress group: `imp.des`
    - functions to prepare boruta/rf data

## Descriptive tables
- script: `SAMexplore_descriptive_tables.Rmd`
- input: `processed_data/implist.withRAW11.rds` (created with preparation code in `SAMexplore_imputation.Rmd`, and `SAMexplore_AUC_calculations.RMD`)
- actions: create a descriptive table (see supporting information in manuscript)
- output: table

## Theoretical Model (TM): Linear Regression Models
- scripts: `SAMexplore_TM_distributions.Rmd` & `SAMexplore_TM_assumptions.RMD` & `SAMexplore_TM_analysis.Rmd` & `SAMexplore_TM_visualization.Rmd`
- input: `SAMimputed_with.AUC.variables.RDS` via `SAMexplore_prepare.for.analyses.r`
- actions: inspect variable distributions, assumptions checks, linear models analysis, and the visualization of marginal effects
- output: tables & plots

## Random Forest (RF): Boruta Variable Selection & Significance testing & Interpretation
- scripts: `SAMexplore_RF_Tuning_Boruta.Rmd` & `SAMexplore_RF_Boruta_variable.selection.Rmd` & `SAMexplore_RF_Boruta_interpretation.Rmd`
- input: `SAMimputed_with.AUC.variables.RDS` via `SAMexplore_prepare.for.analyses.r`
- actions: RF tuning, Boruta variable selection, Random permutation statistics, follow-up (PD and ALE) plots
- output: tables & plots

## Model performance evaluation: TM, RF, ensemble (TM+RF)
- script: `SAMexplore_model.performance.evaluation.Rmd`
- input: `SAMimputed_with.AUC.variables.RDS` via `SAMexplore_prepare.for.analyses.r`
- actions: 10 times 5-fold crossvalidated "predictions", to calculate R2 and RMSE of models with:
    1) significant terms TM
    2) selected variables RF/Boruta
    3) combination TM terms & RF variables: linear model with polynomial terms (if required)   
- output: tables & plots


## Other files
- `pseudoData.R` was used to create fake pseudo-data for an independent check of the R-code
- `SAMexplore_RF_R2_vs_PseudoR2.rmd` was used to explore the differences in R2 and pseudo-R2 calculations (also see information in `SAMexplore_RF_Boruta_interpretation.Rmd`)
