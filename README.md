# Decadal Trends in Seasonal Climatic Variables in Dar es Salaam, Tanzania: A Non-Parametric Approach Using the Mann-Kendall Test

[![DOI](https://zenodo.org/badge/1103447537.svg)](https://doi.org/10.5281/zenodo.17720495)

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
- **Comprehensive visualization suite**: 8 publication-ready plots
- **Statistical tables**: Formatted HTML tables with trend test results
- **Dual implementation**: Both R Markdown (for reports) and standalone R script versions

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
trend      # Sen's slope estimator
knitr      # Dynamic report generation
kableExtra # Enhanced table formatting
ggplot2    # Data visualization
tidyr      # Data tidying
```

### Installation
Install all required packages using:

```r
install.packages(c("readr", "dplyr", "Kendall", "trend", 
                   "knitr", "kableExtra", "ggplot2", "tidyr"))
```

## Repository Structure

```
.
├── README.md                          # This file
├── MK_Test.Rmd                        # R Markdown document with narrative
├── MKTest.R                           # Standalone R script version
├── CLIMATE DAR DATASET.csv            # Input climate data (required)
└── outputs/                           # Generated plots (after running)
    ├── panel_significant_trends.png
    ├── complete_time_series.png
    ├── seasonal_boxplots.png
    ├── sens_slope_magnitude.png
    ├── annual_trends.png
    ├── monthly_climatology.png
    ├── zscore_visualization.png
    └── trend_magnitudes_by_variable.png
```

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

### Option 1: R Markdown (Recommended for Reports)

1. Open `MK_Test.Rmd` in RStudio
2. Ensure `CLIMATE DAR DATASET.csv` is in the working directory
3. Click **Knit** to generate an HTML report with embedded results and visualizations

```r
# Or knit from the console
rmarkdown::render("MK_Test.Rmd")
```

### Option 2: Standalone R Script

1. Set your working directory to the repository folder
2. Ensure `CLIMATE DAR DATASET.csv` is present
3. Run the script:

```r
source("MKTest.R")
```

Or execute interactively in R/RStudio by running sections sequentially.

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

### Visualizations

1. **panel_significant_trends.png**: Time series of variables with statistically significant trends (p < 0.05), with Sen's slope trend lines

2. **complete_time_series.png**: Comprehensive panel showing all variables and seasons, with trends distinguished by significance level

3. **seasonal_boxplots.png**: Box-and-whisker plots showing seasonal distributions of each climatic variable

4. **sens_slope_magnitude.png**: Horizontal bar chart of Sen's slope values, colored by statistical significance

5. **annual_trends.png**: Annual mean values by season with linear trend fits and 95% confidence intervals

6. **monthly_climatology.png**: Average monthly values showing typical annual cycles for each variable

7. **zscore_visualization.png**: Mann-Kendall Z-scores for each season-variable combination, with critical value thresholds (±1.96)

8. **trend_magnitudes_by_variable.png**: Sen's slope magnitudes grouped by variable and season, showing trend directions

All plots are saved at 300 DPI resolution suitable for publication.

## Interpretation Guidelines

### Statistical Significance
- **p < 0.05**: Statistically significant trend at the 95% confidence level
- **|Z| > 1.96**: Corresponds to p < 0.05 for a two-tailed test

### Trend Direction
- **Positive Sen's slope**: Increasing trend over time
- **Negative Sen's slope**: Decreasing trend over time

### Practical Significance
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
  doi          = {10.5281/zenodo.XXXXXXX},
  url          = {https://doi.org/10.5281/zenodo.XXXXXXX}
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

**Last Updated**: November 2024


