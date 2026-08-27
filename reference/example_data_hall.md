# Example data from Hall et al. (2018)

Dexcom G4 CGM measurements for 19 subjects from the Hall publicly
available dataset. Chosen as a subset of all subjects to be only those
with diabetes or pre-diabetes. Primarily intended for use with
example_meals_hall

## Usage

``` r
example_data_hall
```

## Format

a data.frame with 34890 rows and 4 columns, which are:

- id:

  identifier of subject

- time:

  date and time stamp

- gl:

  glucose level as measured by CGM (mg/dL)

- diagnosis:

  character indicating diabetes diagnosis: diabetic or pre-diabetic

## Details

This dataset can be used along with the example_meals_hall dataset in
this package to calculate meal_metrics.

## References

Hall et al. (2018) : Glucotypes reveal new patterns of glucose
dysregulation *Plos Biology* **16** (7): 3:e2005143
[doi:10.1371/journal.pbio.2005143](https://doi.org/10.1371/journal.pbio.2005143)
.
