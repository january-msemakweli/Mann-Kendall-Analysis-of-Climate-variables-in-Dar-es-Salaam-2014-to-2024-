# Decadal Trends in Seasonal Climatic Variables in Dar es Salaam, Tanzania: A Non-Parametric Approach Using the Mann-Kendall Test

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17720494.svg)](https://doi.org/10.5281/zenodo.17720494)

## Overview

This repository contains R code and documentation for conducting Mann-Kendall trend analysis on seasonal climatic variables in Dar es Salaam, Tanzania. The analysis uses monthly meteorological data from the Tanzania Meteorological Authority covering a 10-year period (2014–2024) to detect and quantify monotonic trends in rainfall, temperature, and relative humidity across different seasons.

The Mann-Kendall test is a non-parametric statistical method widely used in climatology and hydrology for detecting trends in time series data. It does not require assumptions about data distribution, making it particularly suitable for environmental data analysis. This implementation includes Sen's slope estimator to quantify the magnitude and direction of detected trends.

## Author

**January G. Msemakweli**  
Graduate Student | ScM in Epidemiology  
Bloomberg School of Public Health  
Johns Hopkins University  
[![ORCID](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0009-0007-6743-8479) https://orcid.org/0009-0007-6743-8479

## Features

- **Non-parametric trend detection**: Mann-Kendall test implementation with Z-score calculation
- **Trend quantification**: Sen's slope estimator for magnitude assessment
- **Seasonal analysis**: Separate trend analysis for each season
- **Statistical power analysis**: Minimum Detectable Trend (MDT) calculations
- **Autocorrelation diagnostics**: Lag-1 ACF assessment with Trend-Free Pre-Whitening (TFPW)
- **Change-point detection**: Pettitt test for identifying abrupt shifts
- **OLS sensitivity analysis**: Parametric validation of non-parametric results
- **Comprehensive visualization suite**: Publication-ready plots
- **Statistical tables**: Formatted HTML tables with trend test results

## Requirements

### Software
- R (version ≥ 4.0.0 recommended)
- RStudio (optional, for R Markdown rendering)

### R Packages
The following R packages are required:

```r
readr      # Data import
dplyr      # Data manipulation
Kendall    # Mann-Kendall test
trend      # Sen's slope estimator and Pettitt test
knitr      # Dynamic report generation
kableExtra # Enhanced table formatting
ggplot2    # Data visualization
tidyr      # Data tidying
broom      # Model output tidying (for OLS analysis)
```

### Installation
Install all required packages using:

```r
install.packages(c("readr", "dplyr", "Kendall", "trend", 
                   "knitr", "kableExtra", "ggplot2", "tidyr", "broom"))
```

## Repository Structure

```
.
├── README.md                          # This file
├── MK_Test.Rmd                        # Main Mann-Kendall trend analysis
├── OLS_Sensitivity_Check.Rmd          # OLS regression sensitivity analysis
├── CLIMATE DAR DATASET.csv            # Input climate data (required)
└── [generated outputs]                # PNG plots and HTML reports
```

## Analysis Files

### MK_Test.Rmd — Main Trend Analysis

The primary analysis document containing:

- **Mann-Kendall Trend Test**: Non-parametric trend detection for each season-variable combination with Z-scores, p-values, and Bonferroni correction
- **Statistical Power Analysis**: Calculates Minimum Detectable Trends (MDT) at 80% power to contextualize non-significant findings
- **Lag-1 Autocorrelation Diagnostics**: Assesses serial correlation and its implications for Type I error rates
- **Trend-Free Pre-Whitening (TFPW)**: Applies pre-whitening to series with substantial autocorrelation (|ACF| ≥ 0.3)
- **Pettitt Change-Point Test**: Detects abrupt shifts in time series central tendency
- **Visualizations**: 8 publication-ready plots including time series, box plots, and trend magnitude charts

### OLS_Sensitivity_Check.Rmd — Parametric Validation

A sensitivity analysis using Ordinary Least Squares (OLS) regression to validate the non-parametric Mann-Kendall results:

- **OLS Regression**: Fits linear models (Y ~ time) for each variable and season
- **Method Comparison**: Compares OLS slopes with Sen's slopes, and t-test significance with Mann-Kendall significance
- **Agreement Statistics**: Quantifies direction agreement and significance agreement between methods
- **Regression Diagnostics**: Shapiro-Wilk normality test and Durbin-Watson autocorrelation test
- **Slope Comparison Plot**: Scatter plot showing OLS vs Sen's slope with 1:1 reference line

This sensitivity check strengthens confidence in findings when both parametric and non-parametric methods agree.

## Data Requirements

The analysis requires a CSV file named `CLIMATE DAR DATASET.csv` with the following columns:

- `Year`: Year of observation (e.g., 2014, 2015, ...)
- `Month`: Month abbreviation (e.g., Jan, Feb, Mar, ...)
- `Season`: Season classification (e.g., Dry, Wet, etc.)
- `rainfall_mm`: Rainfall in millimeters
- `daytime_temperature_celcius`: Daytime temperature in °C
- `nighttime_temperature_celcius`: Nighttime temperature in °C
- `relative_humidity_%`: Relative humidity in percentage

## Usage

1. Open the desired `.Rmd` file in RStudio
2. Ensure `CLIMATE DAR DATASET.csv` is in the working directory
3. Click **Knit** to generate an HTML report with embedded results and visualizations

```r
# Knit from the console
rmarkdown::render("MK_Test.Rmd")              # Main analysis
rmarkdown::render("OLS_Sensitivity_Check.Rmd") # Sensitivity analysis
```

### Expected Outputs

Running the analysis will generate:
- High-resolution PNG plots (300 DPI)
- HTML tables with statistical results
- Console output showing progress and key findings

## Methodology

### Statistical Methods

1. **Mann-Kendall Test**: Non-parametric test for monotonic trends
   - Null hypothesis (H₀): No monotonic trend exists
   - Alternative hypothesis (H₁): A monotonic trend exists
   - Test statistic: Z-score (standardized Mann-Kendall S statistic)
   - Significance level: α = 0.05

2. **Sen's Slope Estimator**: Robust non-parametric slope estimator
   - Calculates the median of all pairwise slopes
   - Units: change per time period (e.g., mm/year, °C/year)

### Analysis Workflow

1. **Data Preparation**: Load and format the climatic dataset with proper date handling
2. **Variable Selection**: Define climatic variables of interest and their labels
3. **Seasonal Stratification**: Separate analysis for each season
4. **Trend Detection**: Apply Mann-Kendall test to each variable-season combination
5. **Trend Quantification**: Calculate Sen's slope for detected trends
6. **Visualization**: Generate comprehensive plots showing trends and patterns
7. **Results Export**: Save statistical tables and publication-ready figures

## Outputs

### Statistical Results Table
A formatted HTML table containing:
- Season
- Variable name
- Z-score (test statistic)
- p-value (statistical significance)
- Sen's slope (trend magnitude)

## Interpretation Guidelines

### Statistical Significance
- **p < 0.05**: Statistically significant trend at the 95% confidence level
- **|Z| > 1.96**: Corresponds to p < 0.05 for a two-tailed test

### Trend Direction
- **Positive Sen's slope**: Increasing trend over time
- **Negative Sen's slope**: Decreasing trend over time

While statistical significance indicates that a trend is unlikely due to chance, consider the magnitude (Sen's slope) for practical implications. Small but statistically significant trends may have limited practical importance for long-term climate planning.

## Citation

If you use this code or methodology in your research, please cite:

```bibtex
@software{msemakweli2024mannkendall,
  author       = {Msemakweli, January G.},
  title        = {{Decadal Trends in Seasonal Climatic Variables in 
                   Dar es Salaam, Tanzania: A Non-Parametric Approach 
                   Using the Mann-Kendall Test}},
  year         = {2024},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.17720494},
  url          = {https://doi.org/10.5281/zenodo.17720494}
}
```

## Data Source

Climate data sourced from:
**Tanzania Meteorological Authority (TMA)**

## License

This project is licensed under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

You are free to:
- **Share**: Copy and redistribute the material in any medium or format
- **Adapt**: Remix, transform, and build upon the material for any purpose, even commercially

Under the following terms:
- **Attribution**: You must give appropriate credit, provide a link to the license, and indicate if changes were made

## Contributing

Contributions, issues, and feature requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Acknowledgments

- Tanzania Meteorological Authority for providing the climate data
- The R community for developing and maintaining the statistical packages used in this analysis

## Contact

For questions, suggestions, or collaboration opportunities, please contact:

**January G. Msemakweli**  
Graduate Student | ScM in Epidemiology  
Bloomberg School of Public Health  
Johns Hopkins University  
615 N Wolfe St  
ORCID: [0009-0007-6743-8479](https://orcid.org/0009-0007-6743-8479)

---

**Keywords**: Mann-Kendall test, Sen's slope, climate trend analysis, time series analysis, Dar es Salaam, Tanzania, rainfall trends, temperature trends, seasonal analysis, non-parametric statistics

**Last Updated**: January 2026


