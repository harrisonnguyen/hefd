
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


#' @export
get_lvef_bin <- function(){
  bins <- list(
          values = c(-Inf,40,50,Inf),
          labels = c("< 40","40-49","50+")
        )
  return(bins)
}


#' @export
get_bnp_bin <- function(){
  bins <- list(
    values = c(-Inf,450,900,1800,Inf),
    labels = c("< 450","450-900","900-1800","1800+")
  )
  return(bins)
}

#' @export
get_transferrin_sat_bin <- function(){
  bins <- list(
    values = c(-Inf,20,Inf),
    labels = c("< 20","20+")
  )
  return(bins)
}

#' @export
get_ferritin_bin <- function(){
  bins <- list(
    values = c(-Inf,100,300,Inf),
    labels = c("< 100","100-299","300+")
  )
  return(bins)
}

#' @export
get_age_bin <- function(){
  bins <- list(
    values =  c(-Inf,18, 25, 50, 75, Inf),
    labels = c("<18", "18-24", "25-49", "50-75", "75+")
  )
  return(bins)
}
