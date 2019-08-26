iglu: Interpretation of data from Continuous Glucose Monitors (CGMs)
====================================================================

The R package 'iglu' provides functions for outputting relevant metrics for data collected from Continuous Glucose Monitors (CGM). For reference, see ["Interpretation of continuous glucose monitoring data: glycemic variability and quality of glycemic control." Robard (2009)](https://www.ncbi.nlm.nih.gov/pubmed/19469679)

Installation
------------

``` install
devtools::install_github("stevebroll/iglu")
```

Example
-------

``` r
library(iglu)
data(example_data_1_subject)
## Plot data

# Use plot on glucose vector for histogram
plot_glu(example_data_1_suject$gl)

# Use plot on dataframe with time and glucose values for time series plot
plot_glu(example_data_1_subject)

# Summary statistics and some metrics
summary_glu(example_data_1_subject)

in_range_percent(example_data_1_subject)

above_percent(example_data_1_subject, targets = c(80,140,200,250))

j_index(example_data_1_subject)

conga(example_data_1_subject)




```
