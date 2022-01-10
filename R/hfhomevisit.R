#' Process HEART_FAILURE_SYMPTOMS field in Homevisit form
#'
#' Exytracts the HEART_FAILURE_SYMPTOMS column
#' and separates the value in this field into specific, separate columns
#'
#' @param df a dataframe where each row represents a homevisit form and each column the fields
#' @param column the column name containing the `HEART_FAILURE_SYMPTOMS`
#' @seealso [hefd::process_homevisit_form()] for more details about `visit_form`
#' @family hfhomevisit
#' @export
extract_hf_symptoms <- function(df,column="HEART_FAILURE_SYMPTOMS"){
  column <- rlang::sym(column)

  visit <- df %>%
    dplyr::mutate(DEPENDENT_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Dependent Oedema"),
                  FATIGUE_HEARTFAILURESYMPTOMS = stringr::str_detect(!!column, "(?i)fatigue"),
                  LETHARGY_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)lethargy"),
                  ANKLE_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Ankle Oedema"),
                  ABDOMINAL_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Abdominal Oedema"),
                  DYSPNOEA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Dyspnoea"),
                  LOWER_LEG_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Lower leg Oedema"),
                  NAUSEA_HEARTFAILURESYMPTOMS  = stringr::str_detect(!!column, "(?i)Nausea"))


  return(visit)
}




#' Process the MACARF_VISIT_TYPE field
#'
#' Extracts the visit type of each form 
#' and assigns the information to a column for each patient.
#' Then assigns a class to each patient, which describes which part of their
#' trajectory in the program
#' e.g. if `HAS_HOME_VISIT = TRUE`, `HAS_PHONE_CALL_1_12_VISIT = FALSE`,
#' `HAS_PHONE_CALL_3_12_VISIT = FALSE`, `HAS_PHONE_CALL_6_12_VISIT =TRUE`
#' the patient would be classified as being in "Phone Call 6/12" of their journey
#'
#' Calculates the number of home visit each person has
#' @param hfhomevisit a dataframe where each row represents a form and the column is each field
#' @return a dataframe where each row represents patient level information
#'
#' @seealso [hefd::process_homevisit_form()] for details about how this field is processed
#' @family hfhomevisit
#' @export
create_visit_trajectory <- function(hfhomevisit) {


  n_homevisit <- hfhomevisit %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::summarise(
      N_CONTACT = dplyr::n_distinct(PARENT_EVENT_ID),
      N_HOMEVISIT = sum(MACARF_VISIT_TYPE == "Home Visit",na.rm=TRUE))


  homevisit_trajectory <- hfhomevisit %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::mutate(HAS_HOME_VISIT = any(MACARF_VISIT_TYPE == "Home Visit", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_1_12_VISIT = any(MACARF_VISIT_TYPE == "Phone Call 1/12", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_3_12_VISIT = any(MACARF_VISIT_TYPE == "Phone Call 3/12", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_6_12_VISIT = any(MACARF_VISIT_TYPE == "Phone Call 6/12", na.rm = TRUE)) %>%
    dplyr::mutate(OTHER_VISIT = any(stringr::str_detect(MACARF_VISIT_TYPE, "(?i)other"), na.rm = TRUE) &
                    !any(stringr::str_detect(MACARF_VISIT_TYPE, "(?i)12/12"), na.rm = TRUE) &
                    !any(stringr::str_detect(MACARF_VISIT_TYPE, "(?i)10/12"), na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_12_12_VISIT = any(stringr::str_detect(MACARF_VISIT_TYPE, "(?i)12/12"), na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_10_12_VISIT = any(stringr::str_detect(MACARF_VISIT_TYPE, "(?i)10/12"), na.rm = TRUE)) %>%
    dplyr::select(PERSON_ID,HAS_HOME_VISIT,HAS_PHONE_CALL_1_12_VISIT,
                  HAS_PHONE_CALL_3_12_VISIT,HAS_PHONE_CALL_6_12_VISIT,OTHER_VISIT,
                  HAS_PHONE_CALL_12_12_VISIT,HAS_PHONE_CALL_10_12_VISIT) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VISIT_TRAJECTORY = dplyr::case_when(
      HAS_PHONE_CALL_12_12_VISIT ~ 'Phone Call 12/12',
      HAS_PHONE_CALL_10_12_VISIT ~ 'Phone Call 10/12',
      HAS_PHONE_CALL_6_12_VISIT ~ 'Phone Call 6/12',
      HAS_PHONE_CALL_3_12_VISIT ~ 'Phone Call 3/12',
      HAS_PHONE_CALL_1_12_VISIT ~ 'Phone Call 1/12',
      OTHER_VISIT & !HAS_HOME_VISIT ~ 'Only Other',
      HAS_HOME_VISIT & OTHER_VISIT~ 'Home visit & Other',
      HAS_HOME_VISIT ~ 'Only Home Visit/s',
      TRUE ~ '(MISSING)'
    )) %>%
    dplyr::left_join(n_homevisit, by = "PERSON_ID")

  return(homevisit_trajectory)
}

#' A helper function to extract homevisit forms
#'
#' Determines whether a query or the function will be used to extract the homevisit forms
#'
#' @family hfhomevisit
#' @export
get_homevisit_forms <- function(forms = NULL,dcp_forms_activity = NULL){
  if(is.null(forms) | is.null(dcp_forms_activity)){
    df <- execute_query(get_hfhomevisit_form_query())
  }
  else{
    df <- get_forms(forms,dcp_forms_activity,pattern = get_hfhomevisit_form_pattern())
  }
  return(df)
}

#' Process homevisit forms
#'
#' Processes the homevisit forms into relevant information such as "MACARF Visit Type"
#' "NYHA Classification","Heart Failure Symptoms"
#' Each row represents a field for each form
#' 
#' The data is combined and pivoted from a long format to a wide format for each form
#'
#' Fields of the same form are connected by `PARENT_EVENT_ID`/`PARENT_ENTITY_ID`.
#' Hence, these grouping are done based on `PARENT_EVENT_ID`/`PARENT_ENTITY_ID`
#' and NOT `ENCNTR_ID`. All these visit forms are linked to the same encounter
#'
#' If `forms` and `dcp_forms_activity` is not provided, the default query will be used/
#' 
#' @param forms df a dataframe where each row contains the fields of the homevisit form
#' @param dcp_forms_activity a dataframe containing the parent key of df
#' @seealso [hefd::extract_hf_symptoms()] for details about how this field is processed
#' @family hfhomevisit
#' @export
process_homevisit_form <- function(forms = NULL,dcp_forms_activity  = NULL){
  df <- get_homevisit_forms(forms,dcp_forms_activity)

  updt <- df %>%
    dplyr::group_by(PARENT_EVENT_ID) %>%
    dplyr::summarise(UPDT_DT_TM = max(UPDT_DT_TM))

  df %<>%
    dplyr::filter(TASK_ASSAY_CD == "MACARF Visit Type" | TASK_ASSAY_CD == "NYHA Classification"
                  |  TASK_ASSAY_CD == "Heart Failure Symptoms") %>%
    dplyr::arrange(desc(FORM_DT_TM)) %>%
    tidyr::pivot_wider(
                      id_cols = c(ENCNTR_ID,PERSON_ID,FORM_DT_TM,PARENT_EVENT_ID),
                      names_from = TASK_ASSAY_CD,
                       values_from = RESULT_VAL,
                      names_sort = TRUE,
                       values_fn = get_first_non_na) %>%
    dplyr::rename_all(.funs = ~toupper(gsub(" ","_",.x))) %>%
    extract_hf_symptoms() %>%
    dplyr::left_join(updt,by="PARENT_EVENT_ID")



  return(df)


}

#' Extract first and last visit for each patient
#'
#' Extracts the dates, NYHA classification and the type of visit for each patients
#' 
#' @param visit_form a df containing visit forms, each patient can have multiple visit forms
#' @return a df, where each row represents a patient and information about their first and last visit
#' @seealso [hefd::process_hfreferral_form()] for more details about `visit_form`
#' @family hfhomevisit
#' @export
create_visit_journey <- function(visit_form){
  cohort <- visit_form %>%
    dplyr::distinct(PERSON_ID,.keep_all = FALSE)

  visit <- visit_form %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::arrange(desc(FORM_DT_TM)) %>%
    dplyr::slice(c(1,dplyr::n())) %>%
    dplyr::mutate(MOST_RECENT_VISIT  = order(dplyr::desc(FORM_DT_TM)) == 1)

  visit_first<- visit %>%
    dplyr::filter(!MOST_RECENT_VISIT)



  visit %<>% dplyr::filter(MOST_RECENT_VISIT) %>%
    dplyr::right_join(visit_first, by = 'PERSON_ID', suffix = c("_RECENT","_FIRST")) %>%
    dplyr::mutate(NYHA_CLASSIFICATION_FIRST =
                    dplyr::case_when(
                      PARENT_EVENT_ID_FIRST == PARENT_EVENT_ID_RECENT ~ 'Only 1 Contact',
                      TRUE ~ NYHA_CLASSIFICATION_FIRST),
                  MACARF_VISIT_TYPE_FIRST =
                    dplyr::case_when(
                      PARENT_EVENT_ID_FIRST == PARENT_EVENT_ID_RECENT ~ 'Only 1 Contact',
                      TRUE ~ MACARF_VISIT_TYPE_FIRST))
  cohort %<>%
    dplyr::left_join(
      visit,by="PERSON_ID"
    ) %>%
    dplyr::left_join(
      visit_form %>% create_visit_trajectory(),by="PERSON_ID"
    ) %>%
    dplyr::select(-dplyr::contains("HEARTFAILURESYMPTOMS"),
                  -dplyr::contains("DT_TM"),
                  -dplyr::contains("MOST_RECENT_VISIT"),
                  -dplyr::contains("MACARF_VISIT_TYPE"),
                  -dplyr::contains("HEART_FAILURE_SYMPTOMS"))

  return(cohort)

}


