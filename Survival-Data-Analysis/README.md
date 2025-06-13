# Survival-Data-Analysis 

## Exploration of Proportional Hazards Model
- Data used: 
  - Generated data in R (`simdata <- sim.survdata(N=42, T=45, num.data.frames=1)$data`)
  - A single survival dataset with 42 observations, in which durations can fall on any integer between 1 and 45
- Outline of project: 
  - Exploratory Data Analysis (EDA)  
  - Fitting using Proportional Hazards Model (`coxph` with `method = "breslow"`) 
  - Reduced Model and Interpretation (using AIC to determine)
  - Adequecy Checking of Reduced Model 
- References:
  - Bozdogan, H. (1987). Model selection and Akaike's Information Criterion (AIC): The general theory and its analytical extensions. Psychometrika, 52, 345-370. https://doi.org/10.1007/BF02294361.
  - Fine, J., & Gray, R. (1999). A Proportional Hazards Model for the Subdistribution of a Competing Risk. Journal of the American Statistical Association, 94, 496-509. https://doi.org/10.1080/01621459.1999.10474144. 
