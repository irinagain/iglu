# iglu 3.2.0
* Add automatic filtering and warning in case multiple measurements for one subject at the same time stamp
* Add automatic sorting of time before calculating MAGE
* Allow to change some of the default parameters when calculatin metrics with all_metrics


# iglu 3.1.0
* Minor bug corrections to shiny app
* Correct bug wtih reading raw data from iglu pro

# iglu 3.0.0
* Added functions for hypo- and hyper-glycemic episodes calculation with visualization profile
* Updated main vignette, expanded additional vignettes on lasagna plots and episode calculation as part of AGP
* Updated documentation for MAD with an option to change constant parameter
* Updated MAGE algorithm to allow separate calculation of MAGE+ and MAGE-

# iglu 2.2.0
* Added updated MAGE algorithm
* Added process_data and read_raw_data to allow reading meter-specific values
* Shiny app now takes time zone
* Documentation and vignette updates


# iglu 2.1.0
* Added MAG (Mean Absolute Glucose)
* Added COGI (COntinuous Glucose monitoring Index)
* Added AGP (Ambulatory Glucose Profile)
* Added new color scheme for lasagna plots
* Added functionality for semilogarithmic plots
* Adjust default in range interval to [70, 180] mg/dL
* Extended active_percent to return information on available data period
* Fixed missing data artifact in time-series plots

# iglu 2.0.1
* Updated links in readme

# iglu 2.0.0
* Version 2 - Version 1 + all new metrics (rate of change, GMI, cv measures, AUC, GVP and MAD)

# iglu 1.5.0
* Added GVP and MAD metrics

# iglu 1.4.0

* Added active percent metric
* Added cv_measures metrics
* Added AUC metric

# iglu 1.3.0

* Added glucose management indicator (GMI) and eA1c

# iglu 1.2.0

* Added rate of change functions with plots

# iglu 1.0.0

* First release
* Added Shiny app functionality.
* Added a `NEWS.md` file to track changes to the package.
