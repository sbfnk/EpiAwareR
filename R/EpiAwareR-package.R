#' @keywords internal
"_PACKAGE"

#' @importFrom stats median quantile time
#' @importFrom utils head
#' @importFrom rlang .data
NULL

# Suppress R CMD check notes for NSE variables used in ggplot2 aes()
utils::globalVariables(c("q5", "q95", "time_idx"))
