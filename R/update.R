#' Updating kelloggmktg482
#'
#' This function updates the kelloggmkgt482 package
#' @export
#' @examples
#' \dontrun{
#' update_kelloggmkgt482()
#' }
update_kelloggmkgt482 <- function() {
  devtools::install_github("blakemcshane/kelloggmktg482", upgrade = "never", force = TRUE)
  }
