#' Extract differential result columns and group labels
#'
#' Internal utility function to identify differential result columns
#' and extract corresponding group names based on a variable suffix.
#'
#' @param data A data frame containing ANCOM-BC2 results.
#' @param prefix Regular expression prefix used to identify columns.
#' @param var Character string specifying the variable name.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{diff_cols}: character vector of matching column names
#'   \item \code{groups}: extracted group labels
#' }
#'
#' @keywords internal
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
