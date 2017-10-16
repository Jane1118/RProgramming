#' Draw the name with frequency over time
#'
#' @param the_name prenoms
#' @param the_sex sex of people
#' @importFrom dplyr filter
#'
#' @return a graph
#' @export
#'
#' @examples
#' \dontrun{
#' draw_a_name()
#'
#' }
#'
#'
draw_a_name<-function(the_name, the_sex){

  dataname <-prenoms::prenoms %>% group_by(name,year)%>%
    filter(name==the_name & sex==the_sex)%>%
    summarise(
      count=sum(n)
    )
  ggplot(dataname, aes(x=year, y=count))+
    geom_line()

}
