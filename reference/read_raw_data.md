# Read raw data from a variety of common sensors.

Helper function to assist in reading data directly from sensor outputs.
Should return a dataframe in correct format for use with the rest of the
`iglu` package. Assumes all data will be readable with base R
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) function.

## Usage

``` r
read_raw_data(
  filename,
  sensor = c("dexcom", "libre", "librepro", "asc", "ipro"),
  id = "filename",
  tz = ""
)
```

## Arguments

- filename:

  String matching the name of the data to be read. Assumed to be .csv

- sensor:

  **Default: "dexcom".** String naming the type of sensor the data was
  exported from. Must be one of "dexcom", "libre", "librepro", "asc", or
  "ipro".

- id:

  **Default: "filename".** String indicating subject id. A value of
  "read" will cause the program to attempt to read the subject id from
  the file. A value of "filename" will cause the program to use the
  basename of the filename (i.e. filename without any directory
  information) with .csv removed, as subject id. A value of "default"
  will cause the program to use whichever of "read" or "filename" that
  is default for that specific sensor. Any other string will be treated
  as the unique id for the entire file.

  Note the asc reader currently does not support id="read"

- tz:

  A character string specifying the time zone to be used.
  System-specific (see
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)), but " " is
  the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
  Invalid values are most commonly treated as UTC, on some platforms
  with a warning.

## Value

A dataframe containing the data read from the named file.

## Details

A DataFrame object with the columns "id", "time" and "gl" and one row
per reading will be returned. For the libre reader, if the phrase
"mmol/l" is found in the column names, the glucose values will be
multiplied by 18.

Assumes .csv format for all data.

Sensor formats change with ongoing development, so these functions may
become deprecated. If any issues are encountered, contact the package
maintainer: this is currently Irina Gaynanova, who can be reached at
<irinagn@umich.edu>.

Note: this function is heavily derived from the readers avaiable in the
cgmanalysis package's `cleandata` function.

## References

Vigers et al. (2019) cgmanalysis: An R package for descriptive analysis
of continuous glucose monitor data *PLoS ONE* **14(10)**: e0216851,
[doi:10.1371/journal.pone.0216851](https://doi.org/10.1371/journal.pone.0216851)

## Author

David Buchanan
