#' Success
#' 
#' Basic success function to be called from test app
#' 
#' @export
#' @param myname your name. Required.
success <- function(myname = ""){
  if(myname == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello", myname, "! This is", R.Version()$version.string)
  )
}