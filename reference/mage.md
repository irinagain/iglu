# Calculate Mean Amplitude of Glycemic Excursions

The function calculates MAGE values and can optionally return a plot of
the glucose trace.

## Usage

``` r
mage(
  data,
  version = c("ma", "naive"),
  sd_multiplier = 1,
  short_ma = 5,
  long_ma = 32,
  return_type = c("num", "df"),
  direction = c("avg", "service", "max", "plus", "minus"),
  tz = "",
  inter_gap = 45,
  max_gap = 180,
  plot = FALSE,
  title = NA,
  xlab = NA,
  ylab = NA,
  show_ma = FALSE,
  show_excursions = TRUE
)
```

## Arguments

- data:

  DataFrame object with column names "id", "time", and "gl" OR numeric
  vector of glucose values.

- version:

  Either `'ma'` or `'naive'`. **Default: 'ma'.** Chooses which version
  of the MAGE algorithm to use. `'ma'` algorithm is more accurate, and
  is the default. Earlier versions of iglu package (\<=2.0.0) used
  `'naive'`.

- sd_multiplier:

  A numeric value that can change the sd value used to determine size of
  glycemic excursions used in the calculation. This is the only
  parameter that can be specified for `version = "naive"`, and it is
  ignored if `version = "ma"`.

- short_ma:

  **Default: 5.** Integer for period length of the short moving average.
  Must be positive and less than \`long_ma\`. (Recommended \<15)

- long_ma:

  **Default: 32.** Integer for period length for the long moving
  average. Must be positive and greater than \`short_ma\`. (Recommended
  \>20)

- return_type:

  **Default: "num".** One of ("num", "df"). Will return either a single
  number for the "MAGE over the entire trace" (weighted by segment
  length) or a DataFrame with the MAGE value for each segment (see the
  MAGE vignette for discussion of handling large gaps by splitting trace
  into multiple segments).

- direction:

  **Default: "avg".** One of ("avg", "service", "max", "plus", or
  "minus"). Algorithm will calculate one of the following: MAGE+ (nadir
  to peak), MAGE- (peak to nadir), MAGEavg = avg(MAGE+, MAGE-), MAGEmax
  = max(MAGE+, MAGE-), or automatically choose MAGE+/MAGE- based on the
  first countable excursion (i.e., "service"). NOTE: the selection of
  peak-to-nadir or nadir-to-peak is chosen independently on each
  segment, thus MAGEservice may choose peak-to-nadir on one segment and
  nadir-to-peak on another, for example.

- tz:

  A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

- inter_gap:

  The maximum allowable gap (in minutes) for interpolation. The values
  will not be interpolated between the glucose measurements that are
  more than inter_gap minutes apart. The default value is 45 min.

- max_gap:

  **Default: 180.** Integer for the maximum length of a gap in minutes
  before the trace is split into segments and MAGE is calculated on each
  segment independently.

- plot:

  **Default: FALSE.** Boolean. If \`TRUE\`, returns a plot that
  visualizes all identified peaks and nadirs, excursions, and missing
  gaps. An interactive GUI can be loaded with \`static_or_gui =
  'plotly'\`.

- title:

  **Default: "Glucose Trace - Subject \[ID\]".** Title for the ggplot.

- xlab:

  **Default: "Time".** Label for x-axis of ggplot.

- ylab:

  **Default: "Glucose Level".** Label for y-axis of ggplot.

- show_ma:

  **Default: FALSE.** Boolean. If TRUE, plots the moving average lines
  on the plot.

- show_excursions:

  **Default: TRUE.** Boolean. If TRUE, shows identified excursions as
  arrows from peak-to-nadir/nadir-to-peak on the plot.

## Value

A tibble object with two columns: the subject id and corresponding MAGE
value. If a vector of glucose values is passed, then a tibble object
with just the MAGE value is returned.

In `version = "ma"`, if `plot = TRUE`, a list of ggplots will be
returned with one plot per subject. To return an interactive plot, use
iglu::mage_ma_single with `static_or_gui='plotly'` on each subject
individually.

## Details

If version `'ma'` is selected, the function computationally emulates the
manual method for calculating the mean amplitude of glycemic excursions
(MAGE) first suggested in "Mean Amplitude of Glycemic Excursions, a
Measure of Diabetic Instability", (Service, 1970). For this version,
glucose values will be interpolated over a uniform time grid prior to
calculation.

`'ma'` is a more accurate algorithm that uses the crosses of a short and
long moving average to identify intervals where a peak/nadir might
exist. Then, the height from one peak/nadir to the next nadir/peak is
calculated from the \_original\_ (not moving average) glucose values.
(Note: this function internally uses CGMS2DayByDay with dt0 = 5. Thus,
all CGM data is linearly interpolated to 5 minute intervals. See the
MAGE vignette for more details.)

`'naive'` algorithm calculates MAGE by taking the mean of absolute
glucose differences (between each value and the mean) that are greater
than the standard deviation. A multiplier can be added to the standard
deviation using the `sd_multiplier` argument.

## References

Service et al. (1970) Mean amplitude of glycemic excursions, a measure
of diabetic instability *Diabetes* **19** .644-655,
[doi:10.2337/diab.19.9.644](https://doi.org/10.2337/diab.19.9.644) .

Fernandes, Nathaniel J., et al. "Open-source algorithm to calculate mean
amplitude of glycemic excursions using short and long moving averages."
Journal of diabetes science and technology 16.2 (2022): 576-577.
[doi:10.1177/19322968211061165](https://doi.org/10.1177/19322968211061165)

## Examples

``` r
data(example_data_5_subject)
mage(example_data_5_subject, version = 'ma')
#> Gap found in data for subject id: Subject 2, that exceeds 12 hours.
#> # A tibble: 5 × 2
#> # Rowwise: 
#>   id         MAGE
#>   <fct>     <dbl>
#> 1 Subject 1  72.4
#> 2 Subject 2 118. 
#> 3 Subject 3 116. 
#> 4 Subject 4  70.9
#> 5 Subject 5 142. 
mage(example_data_5_subject, return_type='df')
#> Gap found in data for subject id: Subject 2, that exceeds 12 hours.
#> # A tibble: 5 × 2
#> # Rowwise: 
#>   id        MAGE         
#>   <fct>     <list>       
#> 1 Subject 1 <df [13 × 5]>
#> 2 Subject 2 <df [5 × 5]> 
#> 3 Subject 3 <df [5 × 5]> 
#> 4 Subject 4 <df [2 × 5]> 
#> 5 Subject 5 <df [5 × 5]> 
```
