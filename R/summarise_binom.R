#' Calculates basic statistics for 0/1 binomial data
#'
#' @name summarise_binom
#' @description Calculates basic statistics for 0/1 binomial data
#' @param tbl A tibble of data.
#' @param col A binomial column in 0/1 or TRUE/FALSE format.
#' @param ... Categorical columns that you wish .to group the analysis by
#' @import dplyr
#' @import rlang
#' @export




summarise_binom <- function(tbl, col, ...){


  group_vars <- rlang::enquos(...)
  col_var <- deparse(substitute(col))
  col_name <- paste0("n_",col_var)
  pct_name <- paste0("pct_",col_var)

  ret <- tbl %>%
    dplyr::group_by(!!! group_vars) %>%
    dplyr::summarise(!! col_name := sum({{ col }}, na.rm = T),
                     n = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(!! pct_name := .data[[col_name]] / n,
                  low = stats::qbeta(.025, .data[[col_name]] + .5, n - .data[[col_name]] + .5 ),
                  high = stats::qbeta(.975, .data[[col_name]] + .5, n - .data[[col_name]] + .5),
                  pct_tot = n / sum(n))


  return(ret)

}






