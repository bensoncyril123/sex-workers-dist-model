# Analysis of Factors Influencing Female Sex Worker Distribution in Sub-Saharan Africa

## Overview

This project investigates the distribution of Female Sex Workers (FSWs) across sub-Saharan Africa to identify key environmental and sociological factors that influence their prevalence. 
The study incorporates statistical modeling, using mixed-effects regression models, to provide insights into how these variables shape FSW distribution, guiding targeted public health interventions and HIV-prevention strategies.

---

## Objectives

1. Analyze the abundance and distribution of FSWs in sub-Saharan Africa using environmental and sociological factors.
2. Identify critical variables influencing FSW counts to prioritize in future data collection efforts.
3. Model spatial and temporal variability through advanced statistical techniques.

---

## Dataset Description

The dataset contains 750 observations, of which 675 were retained for analysis. It includes variables such as:

- **Response Variable**: `FSWCount` (number of Female Sex Workers observed, discrete and non-negative).
- **Environmental Variables**:
  - Population Density (`popDensity`)
  - Built-Up Index (`built`)
  - Growing Season (`growingSeason`)
  - Average Annual Rainfall (`rain`)
  - Temperature (`temperature`)
  - Nighttime Light Activity (`nightLight`)
  - Distance to Clean Water (`cleanWater`)
- **Sociological Variables**:
  - HIV Prevalence (`hivRate`)
  - High Crime Indicator (`highCrime`)
  - Use of Insect Nets (`insectNet`)
  - Wealth Index (`wealthIndex`)
  - Age of First Sexual Experience (`ageFirstSex`)
- **Random Effects**:
  - Country
  - Region
  - Data Year
  - Month

---

## Methodology

1. **Exploratory Data Analysis**:
   - Summary statistics and visualizations (histograms, scatterplots, correlation matrix).
   - Analysis of relationships between FSW counts and predictors.

2. **Model Selection**:
   - Initial Poisson mixed-effects models were evaluated for both environmental and sociological variables.
   - Diagnostic checks revealed overdispersion, prompting consideration of alternative models:
     - Negative Binomial
     - Zero-Inflated Poisson
     - Hurdle Poisson
     - Zero-Inflated Negative Binomial
   - Akaike Information Criterion (AIC) was used for model comparison, identifying the **Negative Binomial Mixed-Effects Model** as the most suitable.

3. **Final Model Specifications**:
   - Environmental Variables:
     - Fixed Effects: `popDensity`, `built`, `temperature`
     - Random Effects: Country, Region
   - Sociological Variables:
     - Fixed Effects: `highCrime`, `insectNet`
     - Random Effects: Country, Region, Data Year

---

## Results

1. **Environmental Variables**:
   - Lower population density (`popDensity`) is associated with higher FSW counts.
   - Less urbanized areas (`built`) exhibit higher FSW counts.
   - Higher temperatures (`temperature`) are positively correlated with FSW counts.

2. **Sociological Variables**:
   - High crime rates (`highCrime`) significantly increase FSW counts.
   - Use of insect nets (`insectNet`) showed a positive association, potentially reflecting public health resource disparities.

3. **Model Fit**:
   - The **Negative Binomial Mixed-Effects Model** provided the best fit (lowest AIC scores: 2869 for environmental variables, 2863 for sociological variables).

---

## Conclusion

This study highlights the complex interplay of environmental and sociological factors in shaping the distribution of FSWs in sub-Saharan Africa. Key findings include:

- Environmental variables such as population density, urbanization levels, and temperature are significant predictors.
- Sociological variables, particularly crime rates and insect net usage, also play a critical role.

The results underscore the importance of integrating diverse dimensions for targeted public health strategies. While these findings are region-specific, they offer valuable insights for broader applications in similar contexts.

---

## Tools and Technologies

- **Statistical Software**: R
- **Packages**: `glmmTMB`, `ggplot2`, `tidyverse`, `DHARMa`, `GGally`
- **Models**:
  - Mixed-Effects Models (Poisson, Negative Binomial, Zero-Inflated, Hurdle)

---

## Future Work

1. Incorporate additional covariates, such as economic indicators or healthcare access metrics.
2. Explore longitudinal data to assess dynamic changes over time.
3. Develop region-specific recommendations for effective public health interventions.

---

## Author

**[Benson Cyril Nana Boakye]**

For further inquiries or collaboration, please contact [nanaboab@gmail.com.
