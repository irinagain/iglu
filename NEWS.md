# iglu 4.2.2
* Fixed bug in Shiny app where MAGE plot was not rendering

# iglu 4.2.1
* Fixed bug in MAGE

# iglu 4.2.0
* Fixed bug in GRI calculation

# iglu 4.1.7
* added tz parameter to functions that require it

# iglu 4.1.6
* Updates to the MAGE vignette
* Minor documentation updates

# iglu 4.1.5
* Updates to the MAGE vignette
* In MAGE function, linearly interpolate data to 5 min intervals using CGMS2DayByDay

# iglu 4.1.1
* Breaking change: changed parameter `plot_type` in iglu::mage, iglu::mage_ma_single to `static_or_cgm`
* Added plotly functionality to `plot_glu()` and the lasagna plots
* Updated author ids
* Fixed formatting issues in the documentation

# iglu 4.1.0
* Add meal plotting functionality

# iglu 4.0.2
* Fixed broken whiplash plot in MAGE documentation
* Added MAGEmax plotting functionality

# iglu 4.0.0
* CRAN release to match upcoming paper summarizing updated MAGE algorithm, episode calculation functionality, meal metrics, clustering and new example data inclusion


# iglu 3.5.3
* minor bugfix in MAGE: stopped extraneous warning from being thrown
* Added tests for mage + mean + median + sd

# iglu 3.5.2
* MAGE plot can now show excursions via "show_excursions = TRUE" flag
* debugged formatting error with Mage vignette
* debugged internal build warnings
* Metrics heatmap removes metrics with no variability across subjects
* Add example dataset to demonstrate meal metrics calculations

# iglu 3.5.1
* MAGE handles traces with gaps, restarting calculation when gap >= user-defined threshold
* MAGE returns ggplot as well as plotly
* Fixed plotting functionality with large gaps and multiple segments
* MAGE "left-side accumulation" optimization
* Added "MAGE_max" and "MAGE_avg" to possible directions able to be calculated (in addition to MAGE+, MAGE-, and Service's definition)
* Updated MAGE vignette for more details

# iglu 3.5.0
* minor bug corrections in mage calculations
* adjustment from summarize to reframe
* update algorithm for episode calculations
* DT package import for shiny app
* add glycemia risk index (GRI)
* add AGP plot smoothing
* add functionality to create a heatmap of metrics by subject

# iglu 3.4.3
* bug correction for denominator in hypo_index and hyper_index in case of missing glucose values

# iglu 3.4.2
* Rewrite for computational efficiency of CGM2DayByDay

# iglu 3.3.2
* Correct bug with ADRR that ignored time zone (worked incorrectly on daylight savings time dates)
* Return COGI in percentage (rather than proportion)
* Correct bug in gvp formula calculation

# iglu 3.3.1
* Correct bug that makes mage not work on data with time supplied as POSIXlt instead of POSIXct

# iglu 3.3.0
* MAGE vignette
* MAGE default parameters for short and long moving averages are automatically adjusted in case of short length of glucose trace
* MAGE automatically adapts to given meter frequency

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
