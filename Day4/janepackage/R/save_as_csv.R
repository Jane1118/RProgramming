#' Save your files as csv
#'
#' @param dataset dataset to save
#' @param filename your file names
#' @param row.names name of rows
#' @param ... any argument
#' @import assertthat
#'
#' @return a list
#' @export
#' @importFrom utils write.csv2
#'
#' @examples
#' \dontrun{
#' save_as_csv()
#' }
save_as_csv <- function(dataset, filename, row.names = FALSE, ...) {
  assert_that(has_extension(filename,"csv"))
  assert_that(is.dir(dirname(filename)))
  assert_that(is.writeable(dirname(filename)))
  assert_that(not_empty(dataset))
  assert_that(is.data.frame(dataset))
  write.csv2(x = dataset, file = filename, row.names = row.names, ...)
  invisible(normalizePath(filename))
}
