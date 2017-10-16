#' import all sheets from an excel file
#'
#' @param file path to xlsx file
#' @import readxl
#'
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#'
#' read_excel_multi("/floder/toxlsxfile")
#' }
#'
#'
read_excel_multi <- function(file){
  all_sheets <- readxl::excel_sheets(file)
  result <- lapply(all_sheets, function(sheet){
    readxl::read_excel(file, sheet = sheet)
  })
  return(result)
}
