#' Put what the function does here, ex:
#' Calculate High Blood Glucose Index (HBGI)
#'
#' @description
#' (This section should list function name, what it calculates, and what it returns)
#' The function hbgi produces HBGI values in a tibble object.
#'
#' @usage
#' (directions on how to use the function, include any optional parameters, should mirror
#' the function declaration)
#' hbgi(data, example = "this is an optional parameter")
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#' @param example string object used as an example to show optional parameters.
#' Default value is "this is an optional parameter"
#' (This section should list the parameters the function takes and give
#' a description of what they function expects them to be)
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding HBGI value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the HBGI value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#' (This section should give an explanation of what the function will return)
#'
#' (This is a test)
#' @export
#'
#'
#' @details (A longer explanation of what the function does, what it returns and
#' what it's actually calculating, include formula and/or methodology of calculation)
#'
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for HBGI values is returned. NA glucose values are
#' omitted from the calculation of the HBGI.
#'
#' HBGI is calculated by \eqn{1/n * \sum (10 * fbg_i ^2)},
#' where \eqn{fbg_i = max(0, 1.509 * (log(BG_i)^{1.084} - 5.381)},
#' BG_i is the ith Blood Glucose measurement for a subject, and
#' n is the total number of measurements for that subject.
#'
#' @references (the paper from which the metric was sourced, folow this general format)
#' Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose Variability in,
#' Diabetes
#' \emph{Diabetes care} \strong{29} .2433-2438,
#' \doi{10.2337/dc06-1085}.
#'
#' @examples (example uses of function, the data(example_data_1_subject) includes
#' the example JHU data into the R environmennt, hbgi(example_data_1_subject)
#' is the example call)
#'
#' data(example_data_1_subject)
#' hbgi(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' hbgi(example_data_5_subject)
#'


hbgi <- function(data, example = "this is an optional parameter"){

  #This part is sanitizing and should always go at the front
  fbg = gl = id = NULL  #Do these two steps for any variables used within the function
  rm(list = c("fbg", "gl", "id")) #This ensures they are not in the global environment

  data = check_data_columns(data) #This part sanitizies the input data
  is_vector = attr(data, "is_vector") #All non time-dependent metrics should accept vectors of gl

  #Define any helper functions here
  helper_function = function(one_subject_data) {
    individual = do_stuff(one_subject_data)
    return(individual)
  }

  #Perform calculation
  #These steps may change depending on the needs of your function
  #We broadly follow the split-apply-combine paradigm with the metrics
  #So if anyone asks you have experience with it
  out = data %>% #First line is always defining output variable and piping (%>%) it into the next line
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate( #This could be done before or after grouping by id
      fbg = log(gl)^{1.084} - 5.381,
      fbg = pmax(fbg, 0)) %>%
    dplyr::group_by(id) %>% # Split
    dplyr::summarise( #Apply & combine
      hbgi = 22.77  *
        sum(fbg[gl >=  112.5]^2, na.rm = TRUE) /
        sum(!is.na(gl))
    )
  if (is_vector) { #For functions which take glucose vectors, add this at the end
    out$id = NULL  #Because a vector contains no id information
  }
  return(out) #return

}
