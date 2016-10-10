#' @title Range Value
#' @param x numeric vector
#' @return range as max - min

#na.rm set automatically to FALSE, change to TRUE if missing values exist
range_value <- function(x, na.rm=FALSE) {
  if (na.rm){
    new_x <- na.omit(x)
    max(new_x) - min(new_x)
  } else {
    max(x) - min(x)
  }}

#' @title Missing Values
#' @param x a vector
#' @return number of missing values
missing_values <- function(x) {
  sum(is.na(x)) 
}