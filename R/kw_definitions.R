
#' Define enrolment form string
#'
#' @export
#' @family definition
get_hfenrolment_form_pattern <- function(){
  string <- "Heart Failure.*Enrolment"
  return(stringr::regex(string,ignore_case = TRUE))
}

#' Define homevisit form string
#'
#' @export
#' @family definition
get_hfhomevisit_form_pattern <- function(){
  string <- "Heart Failure.*Home Visit"
  return(stringr::regex(string,ignore_case = TRUE))
}

#' Define lvef categories
#'
#' @export
#' @family definition
get_lvef_bin <- function(){
  bins <- list(
          values = c(-Inf,41,50,Inf),
          labels = c("40 or less","41-49","50+")
        )
  return(bins)
}

#' Define bnp categories
#'
#' @export
#' @family definition
get_bnp_bin <- function(){
  bins <- list(
    values = c(-Inf,450,900,1800,Inf),
    labels = c("< 450","450-900","900-1800","1800+")
  )
  return(bins)
}

#' @export
#' @family definition
get_transferrin_sat_bin <- function(){
  bins <- list(
    values = c(-Inf,20,Inf),
    labels = c("< 20","20+")
  )
  return(bins)
}

#' Define ferritin categories
#'
#' @export
#' @family definition
get_ferritin_bin <- function(){
  bins <- list(
    values = c(-Inf,100,300,Inf),
    labels = c("< 100","100-299","300+")
  )
  return(bins)
}

#' Define age categories
#'
#' @export
#' @family definition
get_age_bin <- function(){
  bins <- list(
    values =  c(-Inf,18, 25, 50, 75, Inf),
    labels = c("<18", "18-24", "25-49", "50-75", "75+")
  )
  return(bins)
}


#' Define the regex for LVEF
#'
#' @export
#' @family regex
get_lvef_regex <- function(){
  regex <- "(EF|(?i)Ejection|LVEF|fraction).*[[:digit:]]{1,2}"
  return(regex)
}

#' Define the regex for LVEF
#'
#' @export
#' @family regex
get_lvef_refine_regex <- function(){
  regex <- "(?i)systolic.*[[:digit:]]{1,2}.*%"
  return(regex)
}

#' Define the regex for extracting digits
#'
#' @export
#' @family regex
get_digit_regex <- function(){
  regex <- "[[:digit:]]{2}"
  return(regex)
}


