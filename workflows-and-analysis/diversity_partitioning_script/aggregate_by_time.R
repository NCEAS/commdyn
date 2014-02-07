

#' @param df the data data frame which has sites as rows, species as columns
#' @param unit the unit of time over which we want to aggregate
#' @param fun.aggregate A function to aggregate 
#' @param species.columns the position of all columns containing species data 
#' @param date.column the position of the column containing the dates we will aggregate by
#' @import lubridate
#' @examples
#' 
#' )
aggregate_by_time <- function(df,
                              unit = "year",
                              fun.aggregate = sum,
                              species.columns, #= which(sapply(df, is, "numeric")))
                              date.column = which(sapply(df, is, "Date"))){
  
  df$date_block <- floor_date(df[[date.column]], unit)
  factors <- names(df)[-c(species.columns, date.column)]
  
  out <- ddply(
    df, 
    factors,
    function(x){
      ap <- apply(x[species.columns], 2, fun.aggregate)
      cbind(x[1, -species.columns, drop = FALSE], matrix(ap, nrow=1))
    })
}
