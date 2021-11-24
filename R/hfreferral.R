#' @export
process_hfreferral_form <- function(){
  referral_forms <- execute_query(get_hfreferral_form_query())
  referral_cohort <- referral_forms %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::slice_min(order_by=FORM_DT_TM) %>%
    dplyr::select(PERSON_ID,ENCNTR_ID,FORM_DT_TM,UPDT_DT_TM,DCP_FORMS_ACTIVITY_ID,PARENT_ENTITY_ID) %>%
    dplyr::rename(FORM_ENCNTR_ID = ENCNTR_ID,FORM_UPDT_DT_TM = UPDT_DT_TM)
}
