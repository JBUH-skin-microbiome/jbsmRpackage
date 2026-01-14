get_utils <- function(data, prefix=NULL, var=NULL){
  # Diff columns
  diff_cols <- grep(
    paste0(prefix, var),
    colnames(data), value = TRUE
  )

  # Error coding
  if (length(diff_cols) == 0) {
    stop("No significant values found for variable: ", var)
  } else {
    groups <- stringr::str_remove(diff_cols, paste0(prefix, var))
  }

  list(diff_cols = diff_cols, groups = groups)
}
