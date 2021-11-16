
#' @export
get_hfenrolment_form_pattern <- function(){
  string <- "Heart Failure.*Enrolment"
  return(stringr::regex(string,ignore_case = TRUE))
}

#' @export
get_hfhomevisit_form_pattern <- function(){
  string <- "Heart Failure.*Home Visit"
  return(stringr::regex(string,ignore_case = TRUE))
}

