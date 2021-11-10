
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
get_hfhomevisit_form_query <- function(){
  query <- "select x.ENCNTR_ID,x.PERSON_ID,x.FORM_DT_TM,y.PARENT_EVENT_ID,y.TASK_ASSAY_CD,y.RESULT_VAL from DCP_FORMS_ACTIVITY as x
  left join FORMS_EVENT as y
  on PARENT_EVENT_ID = PARENT_ENTITY_ID
  where DESCRIPTION = 'Heart Failure - Home Visit/Phone Call Assessment';"
  return(query)
}
