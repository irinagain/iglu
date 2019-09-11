#' Calculate percentage in targeted value ranges
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param targets List of target values. Default list is [(80,200), (70,180), (70,140)].
#'
#' @return
#'
#' @export
#'
#' @examples
#' in_range_percent(data)
#' in_range_percent(data, targets = list(c(100,200), c(150,250))
#'
#' # The following will return same output as previous output:
#' in_range_percent(data, targets = list(c(200, 100), c(250,150))

in_range_percent <- function(data,
                  targets = list(c(80,200), c(70,180), c(70,140))){
  in_range_percent_single = function(data, targets){
    gl_by_id = read_df_or_vec(data)
    out_vec = NULL
    colnames_list = NULL
    for(target_range in targets){
      target_range = as.double(target_range)
      percent = sum(gl_by_id >= min(target_range) &
                    gl_by_id <= max(target_range))/length(gl_by_id) * 100
      out_vec = c(out_vec, percent)
      name = paste('in_range_', min(target_range),'_',
                  max(target_range), sep = '')
      colnames_list = c(colnames_list, name)
    }
    out = data.frame(matrix(out_vec, nrow = 1))
    names(out) = colnames_list
    return(out)
  }

  in_range_percent_multi = function(data, targets){
    subjects = unique(data$id)
    colnames_list = vector(length = length(targets))
    out_mat = matrix(nrow = length(subjects), ncol = length(targets))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      for(col in 1:length(targets)){
        target_range = targets[[col]]
        percent = sum(gl_by_id >= min(target_range) &
                        gl_by_id <= max(target_range))/length(gl_by_id) * 100
        out_mat[row, col] = percent
      }
    }
    for(col in 1:length(targets)){
      target_range = targets[[col]]
      name = paste('in_range_', min(target_range),'_',
                   max(target_range), sep = '')
      colnames_list[col] = name
    }

    out = data.frame(out_mat)
    names(out) = colnames_list
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data)){
    in_range_percent_multi(data,targets)
  } else in_range_percent_single(data,targets)

}
