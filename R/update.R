#' A function to update the kelloggmkgt482 package
#'
#' This function allows you to easily update the mkgt482 package from github
#' @keywords update
#' @export
#' @examples
#' update_kelloggmkgt482()

update_kelloggmkgt482 <- function() {
  devtools::install_github("blakemcshane/kelloggmktg482", upgrade = "never", force = TRUE)
  }
