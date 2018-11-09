

#' CleanFee (operational function)
#'
#' This function is used to extract numerical values from the Fee field.
#'
#'


CleanFee = function(vec_fees){

  ifelse(grepl(x = vec_fees, pattern = "k"),
         1000 * as.numeric(gsub(x = vec_fees, pattern = "[^0-9\\.]", replacement = "")),
         ifelse(grepl(x = vec_fees, pattern = "m"),
                1000000 * as.numeric(gsub(x = vec_fees, pattern = "[^0-9\\.]", replacement = "")), 0))

}
