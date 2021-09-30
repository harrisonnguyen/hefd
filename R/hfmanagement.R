
#' @export
get_precipitants <- function(df){
  df %<>% dplyr::filter(TASK_ASSAY_CD == "HF Precipitamts" | TASK_ASSAY_CD == "HF Possible precipitants") %>%
    dplyr::mutate(ARRYTHMIA_HFPRECIPITANTS  = stringr::str_detect(RESULT_VAL, "(?i)arrythmia"),
                  INFECTION_HFPRECIPITANTS  = stringr::str_detect(RESULT_VAL, "(?i)infection"),
                  CHANGE_MEDICATION_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)change in medication"),
                  ISCHAEMIA_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)ischaemia"),
                  POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)Poor compliance with medication"),
                  NONE_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)no known"),
                  OTHER_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)Other:"),
                  RECENT_AMI_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)ami"),
                  POOR_COMPLIANCE_DIET_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)Poor compliance with diet"),
                  FLU_VAC_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL, "(?i)flu")) %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::summarise(
      ARRYTHMIA_HFPRECIPITANTS = any(ARRYTHMIA_HFPRECIPITANTS),
      INFECTION_HFPRECIPITANTS = any(INFECTION_HFPRECIPITANTS),
      CHANGE_MEDICATION_HFPRECIPITANTS = any(CHANGE_MEDICATION_HFPRECIPITANTS),
      ISCHAEMIA_HFPRECIPITANTS = any(ISCHAEMIA_HFPRECIPITANTS),
      POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS = any(POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS),
      NONE_HFPRECIPITANTS = any(NONE_HFPRECIPITANTS),
      OTHER_HFPRECIPITANTS = any(OTHER_HFPRECIPITANTS),
      RECENT_AMI_HFPRECIPITANTS = any(RECENT_AMI_HFPRECIPITANTS),
      POOR_COMPLIANCE_DIET_HFPRECIPITANTS = any(POOR_COMPLIANCE_DIET_HFPRECIPITANTS),
      FLU_VAC_HFPRECIPITANTS = any(FLU_VAC_HFPRECIPITANTS)
    ) %>%
    dplyr::mutate(N_PRECIPIANTS = rowSums(dplyr::across(where(is.logical))))
  return(df)
}

#' @export
get_aetiology <- function(df){
  df %<>% dplyr::filter(TASK_ASSAY_CD == "Aetiology of CCF") %>%
     dplyr::mutate(ISCHAEMIC_AETIOLOGYOFCCF  = stringr::str_detect(RESULT_VAL, "(?i)ISCHAEMIC"),
                       ALCOHOLIC_AETIOLOGYOFCCF  = stringr::str_detect(RESULT_VAL, "(?i)ALCOHOLIC"),
                       AORTIC_STENOSIS_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL, "(?i)AORTIC STENOSIS"),
                       IDIOPATHIC_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL, "(?i)IDIOPATHIC"),
                       OTHER_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL, "(?i)OTHER"),
                       HYPERTENSION_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL, "(?i)HYPERTENSION"),
                       HYPERTROPHIC_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL, "(?i)HYPERTROPHIC")) %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::summarise(
      ISCHAEMIC_AETIOLOGYOFCCF = any(ISCHAEMIC_AETIOLOGYOFCCF),
      ALCOHOLIC_AETIOLOGYOFCCF = any(ALCOHOLIC_AETIOLOGYOFCCF),
      AORTIC_STENOSIS_AETIOLOGYOFCCF = any(AORTIC_STENOSIS_AETIOLOGYOFCCF),
      IDIOPATHIC_AETIOLOGYOFCCF = any(IDIOPATHIC_AETIOLOGYOFCCF),
      OTHER_AETIOLOGYOFCCF = any(OTHER_AETIOLOGYOFCCF),
      HYPERTENSION_AETIOLOGYOFCCF = any(HYPERTENSION_AETIOLOGYOFCCF),
      HYPERTROPHIC_AETIOLOGYOFCCF = any(HYPERTROPHIC_AETIOLOGYOFCCF)
    )
  return(df)
}

#' @export
get_ecg_on_admit <- function(df){
  df %<>% dplyr::filter(TASK_ASSAY_CD == "ECG on Admission") %>%
    dplyr::mutate(ATRIAL_FABRILLATION_ECGONADMISSION  = stringr::str_detect(RESULT_VAL, "(?i)Atrial Fibrillation"),
                  LEFT_BBB_ECGONADMISSION  = stringr::str_detect(RESULT_VAL, "(?i)Left bundle-branch block"),
                  ATRIAL_FLUTTER_ECGONADMISSION = stringr::str_detect(RESULT_VAL, "(?i)Atrial flutter"),
                  PPM_ECGONADMISSION = stringr::str_detect(RESULT_VAL, "(?i)PPM"),
                  SINUS_BRADYCARDIA_ECGONADMISSION = stringr::str_detect(RESULT_VAL, "(?i)Sinus bradycardia"),
                  SINUS_RHYTHM_ECGONADMISSION = stringr::str_detect(RESULT_VAL, "(?i)Sinus rhythm"),
                  RIGHT_BBB_ECGONADMISSION = stringr::str_detect(RESULT_VAL, "(?i)Right bundle-branch block")) %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::summarise(
      ATRIAL_FABRILLATION_ECGONADMISSION = any(ATRIAL_FABRILLATION_ECGONADMISSION),
      LEFT_BBB_ECGONADMISSION = any(LEFT_BBB_ECGONADMISSION),
      ATRIAL_FLUTTER_ECGONADMISSION = any(ATRIAL_FLUTTER_ECGONADMISSION),
      PPM_ECGONADMISSION = any(PPM_ECGONADMISSION),
      SINUS_BRADYCARDIA_ECGONADMISSION = any(SINUS_BRADYCARDIA_ECGONADMISSION),
      SINUS_RHYTHM_ECGONADMISSION = any(SINUS_RHYTHM_ECGONADMISSION),
      RIGHT_BBB_ECGONADMISSION = any(RIGHT_BBB_ECGONADMISSION)
    )
  return(df)
}

#' @export
get_ecg_data <- function(df){
  breaks <- c(0,40,50,Inf)

  tags <- c("<40", "40-49", "50+")
  df %<>% dplyr::filter(TASK_ASSAY_CD == "LVEF %" | TASK_ASSAY_CD == "Echocardiogram date") %>%
    dplyr::select(ENCNTR_ID,TASK_ASSAY_CD,RESULT_VAL) %>%
    dplyr::group_by(ENCNTR_ID) %>%
    tidyr::pivot_wider(names_from = TASK_ASSAY_CD, values_from = RESULT_VAL,values_fn = max) %>%
    dplyr::mutate(`LVEF %` = as.numeric(`LVEF %`)) %>%
    dplyr::mutate(LVEF_BIN = cut(`LVEF %`,breaks=breaks,include.lowest = TRUE, right = FALSE,labels=tags)) %>%
    dplyr::rename(LVEF = `LVEF %`, ECG_DATE = `Echocardiogram date`)

  pattern <- "(?i)(ecg_date)"
  dttm_cols <- names(df) %>% .[stringr::str_detect(., pattern)]
  df  %<>% dplyr::mutate_at(dttm_cols,
                                     purrr::partial(lubridate::dmy_hm, tz = "Australia/Sydney", truncated = 3)) %>%
    dplyr::mutate(LVEF_BIN = as.character(LVEF_BIN))

}

#' @export
process_hfmanagement_form <- function(forms,dcp_forms_activity){
  df <- get_forms(forms,dcp_forms_activity,pattern = get_hfenrolment_form_pattern())


  form_cohort <- df %>%
   dplyr::select(ENCNTR_ID,PERSON_ID,FORM_DT_TM,PARENT_EVENT_ID,UPDT_DT_TM) %>%
   dplyr::distinct(ENCNTR_ID,.keep_all = TRUE)



  # get the first form for each person
  form_cohort %<>% dplyr::left_join(df %>% get_ecg_data()) %>%
   dplyr::left_join(df %>% get_ecg_on_admit()) %>%
   dplyr::left_join(df %>% get_aetiology()) %>%
   dplyr::left_join(df %>% get_precipitants()) %>%
   dplyr::group_by(PERSON_ID) %>%
   dplyr::mutate(ENROLMENT_ORDER = order(order(FORM_DT_TM))) %>%
   dplyr::ungroup()




  return(form_cohort)


}
