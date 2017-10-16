#' Use dygraph to draw the dynamic graph of the frequency of the
#'
#' @param names prenoms
#' @import dplyr tidyr dygraphs assertthat
#'
#' @return a graph
#' @export
#'
#' @examples
#' \dontrun{
#' draw_names()
#' }
draw_names<- function(names){
  assert_that(is.character(names),msg = "Please spell the name in a correct way with quotation mark!!")
  prenoms::prenoms %>%
    group_by(year, name) %>%
    summarise(total =sum(n))%>%
    filter(name %in% names) %>%
    spread(key=name, value=total)%>%
    dygraph()
}
