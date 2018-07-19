#ashareDescription <- db_query('select * from ASHAREDESCRIPTION')
#' return company name given windcode
#'
#' @param windCode
#'
#' @return
#' @export
#'
#' @examples
windCode2Name <- function(windCode){
  sapply(windCode, function(x){
    ashareDescription[ashareDescription$S_INFO_WINDCODE == x, "S_INFO_NAME"]
  },USE.NAMES = FALSE)
}
#' return company info given windcode
#'
#' @param windCode
#' @param info like name, listdate
#'
#' @return
#' @export
#'
#' @examples
getCompInfo <- function(windCode, info = 'name'){
  sapply(windCode, function(x){
    ashareDescription[ashareDescription$S_INFO_WINDCODE == x,
                      toupper(paste0('S_INFO_', info))]
  },USE.NAMES = FALSE)
}
