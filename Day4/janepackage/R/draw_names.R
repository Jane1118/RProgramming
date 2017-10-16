#' Use dygraph to draw the dynamic graph of the frequency of the
#'
#' @param names prenoms
#'
#' @return a graph
#' @export
#'
#' @examples
#' \dontrun{
#' draw_names()
#' }
draw_names<- function(names){
  prenoms %>%
    group_by(year, name) %>%
    summarise(total =sum(n))%>%
    filter(name %in% names) %>%
    spread(key=name, value=total)%>%
    dygraph()
}
