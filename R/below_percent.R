#' Calculate percentage below targeted values
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param targets List of target values. Default list is (50,80).
#'
#' @return
#'
#' @export
#'
#' @examples
#' below_percent(data)
#' below_percent(data, targets = c(50,100, 180))

below_percent <- function(data, targets = c(50,80)){
  below_percent_single = function(data,targets){
    gl_by_id = read_df_or_vec(data)
    targets = as.double(targets)
    out_vec = NULL
    colnames_list = NULL
    for(target_val in targets){
      percent = sum(gl_by_id < target_val)/length(gl_by_id) * 100
      out_vec = c(out_vec, percent)
      name = paste('below_', target_val, sep = '')
      colnames_list = c(colnames_list, name)
    }
    out = data.frame(matrix(out_vec, nrow = 1))
    names(out) = colnames_list
    return(out)
  }

  below_percent_multi = function(data, targets){
    subjects = unique(data$id)
    targets = as.double(targets)
    colnames_list = vector(length = length(targets))
    out_mat = matrix(nrow = length(subjects), ncol = length(targets))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row],
                                             'gl']))
      for(col in 1:length(targets)){
        percent = sum(gl_by_id < targets[col])/length(gl_by_id) * 100
        out_mat[row, col] = percent
      }
    }
    for(col in 1:length(targets)){
      name = paste('below_', targets[col], sep = '')
      colnames_list[col] = name
    }

    out = data.frame(out_mat)
    names(out) = colnames_list
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    below_percent_multi(data,targets)
  } else below_percent_single(data,targets)

}




