#' @export
get_hf_symptoms <- function(df){

  visit <- df %>%
    dplyr::filter(TASK_ASSAY_CD == "Heart Failure Symptoms") %>%
    dplyr::mutate(DEPENDENT_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Dependent Oedema"),
                  FATIGUE_HEARTFAILURESYMPTOMS = stringr::str_detect(RESULT_VAL, "(?i)fatigue"),
                  LETHARGY_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)lethargy"),
                  ANKLE_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Ankle Oedema"),
                  ABDOMINAL_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Abdominal Oedema"),
                  DYSPNOEA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Dyspnoea"),
                  LOWER_LEG_OEDEMA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Lower leg Oedema"),
                  NAUSEA_HEARTFAILURESYMPTOMS  = stringr::str_detect(RESULT_VAL, "(?i)Nausea")) %>%
    dplyr::group_by(PARENT_EVENT_ID) %>%
    dplyr::summarise(
      DEPENDENT_OEDEMA_HEARTFAILURESYMPTOMS = any(DEPENDENT_OEDEMA_HEARTFAILURESYMPTOMS),
      FATIGUE_HEARTFAILURESYMPTOMS = any(FATIGUE_HEARTFAILURESYMPTOMS),
      LETHARGY_HEARTFAILURESYMPTOMS = any(LETHARGY_HEARTFAILURESYMPTOMS),
      ANKLE_OEDEMA_HEARTFAILURESYMPTOMS = any(ANKLE_OEDEMA_HEARTFAILURESYMPTOMS),
      ABDOMINAL_OEDEMA_HEARTFAILURESYMPTOMS = any(ABDOMINAL_OEDEMA_HEARTFAILURESYMPTOMS),
      DYSPNOEA_HEARTFAILURESYMPTOMS = any(DYSPNOEA_HEARTFAILURESYMPTOMS),
      LOWER_LEG_OEDEMA_HEARTFAILURESYMPTOMS = any(LOWER_LEG_OEDEMA_HEARTFAILURESYMPTOMS),
      NAUSEA_HEARTFAILURESYMPTOMS = any(NAUSEA_HEARTFAILURESYMPTOMS)
    ) %>%
    dplyr::left_join(dplyr::select(df,PERSON_ID,FORM_DT_TM,PARENT_EVENT_ID), by ="PARENT_EVENT_ID") %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::arrange(desc(FORM_DT_TM)) %>%
    dplyr::slice(c(1,dplyr::n())) %>%
    dplyr::mutate(MOST_RECENT_VISIT  = order(as.Date(FORM_DT_TM)) == 1)




  visit_first<- visit %>%
    dplyr::filter(!MOST_RECENT_VISIT)



  visit %<>% dplyr::filter(MOST_RECENT_VISIT) %>%
    dplyr::left_join(visit_first, by = 'PERSON_ID', suffix = c("","_FIRST"))


  return(visit)
}

#' @export
get_nyha_classification <- function(df){
  visit <- df %>% dplyr::filter(TASK_ASSAY_CD == "NYHA Classification" | TASK_ASSAY_CD == "MACARF Visit Type") %>%
    dplyr::select(PARENT_EVENT_ID,ENCNTR_ID,PERSON_ID,TASK_ASSAY_CD,RESULT_VAL) %>%
    dplyr::group_by(PARENT_EVENT_ID) %>%
    tidyr::pivot_wider(names_from = TASK_ASSAY_CD, values_from = RESULT_VAL) %>%
    dplyr::rename(MACARFVISITTYPE = `MACARF Visit Type`, NYHACLASSIFICATION = `NYHA Classification`) %>%
    dplyr::left_join(dplyr::select(df,PERSON_ID,FORM_DT_TM,PARENT_EVENT_ID), by =c("PARENT_EVENT_ID","PERSON_ID")) %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::arrange(desc(FORM_DT_TM)) %>%
    dplyr::slice(c(1,dplyr::n())) %>%
    dplyr::mutate(MOST_RECENT_VISIT  = order(as.Date(FORM_DT_TM)) == 1)




  visit_first<- visit %>%
    dplyr::filter(!MOST_RECENT_VISIT)



  visit %<>% dplyr::filter(MOST_RECENT_VISIT) %>%
    dplyr::left_join(visit_first, by = 'PERSON_ID', suffix = c("","_FIRST")) %>%
    dplyr::mutate(NYHACLASSIFICATION_FIRST =
                  dplyr::case_when(
                    is.na(PARENT_EVENT_ID_FIRST) ~ 'Only 1 Contact',
                    TRUE ~ NYHACLASSIFICATION_FIRST),
                MACARFVISITTYPE_FIRST =
                  dplyr::case_when(
                    is.na(PARENT_EVENT_ID_FIRST) ~ 'Only 1 Contact',
                    TRUE ~ MACARFVISITTYPE_FIRST))
}



#' @export
create_visit_trajectory <- function(hfhomevisit) {


  n_homevisit <- hfhomevisit %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::summarise(
      N_CONTACT = dplyr::n(),
      N_HOMEVISIT = sum(MACARFVISITTYPE == "Home Visit",na.rm=TRUE))


  homevisit_trajectory <- hfhomevisit %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::mutate(HAS_HOME_VISIT = any(MACARFVISITTYPE == "Home Visit", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_1_12_VISIT = any(MACARFVISITTYPE == "Phone Call 1/12", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_3_12_VISIT = any(MACARFVISITTYPE == "Phone Call 3/12", na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_6_12_VISIT = any(MACARFVISITTYPE == "Phone Call 6/12", na.rm = TRUE)) %>%
    dplyr::mutate(OTHER_VISIT = any(stringr::str_detect(MACARFVISITTYPE, "(?i)other"), na.rm = TRUE) &
                    !any(stringr::str_detect(MACARFVISITTYPE, "(?i)12/12"), na.rm = TRUE) &
                    !any(stringr::str_detect(MACARFVISITTYPE, "(?i)10/12"), na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_12_12_VISIT = any(stringr::str_detect(MACARFVISITTYPE, "(?i)12/12"), na.rm = TRUE)) %>%
    dplyr::mutate(HAS_PHONE_CALL_10_12_VISIT = any(stringr::str_detect(MACARFVISITTYPE, "(?i)10/12"), na.rm = TRUE)) %>%
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
      HAS_HOME_VISIT & OTHER~ 'Home visit & Other',
      HAS_HOME_VISIT ~ 'Only Home Visit/s',
      TRUE ~ '(MISSING)'
    )) %>%
    dplyr::left_join(n_homevisit, by = "PERSON_ID")

  return(homevisit_trajectory)
}



#' @export
process_homevisit_form <- function(forms,dcp_forms_activity){
  df <- get_forms(forms,dcp_forms_activity,pattern = get_hfhomevisit_form_pattern())

  form_cohort <- df %>%
    dplyr::select(ENCNTR_ID,PERSON_ID,PARENT_EVENT_ID,FORM_DT_TM) %>%
    dplyr::distinct()

  form_cohort %<>% dplyr::left_join(df %>% get_hf_symptoms(),by="PERSON_ID") %>%
    dplyr::left_join(df %>% get_nyha_classification(),by="PERSON_ID")

  visit_trajectory <- form_cohort %>%
    create_visit_trajectory()

}