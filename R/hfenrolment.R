
#' Define the fields in HF enrolment form
#'
#' Returns a list of field names that
#' we are interested in, in the  HF enrolment form
#'
#' @family hfenrolment
#' @export
get_enrolment_fields <- function(){
  fields <- c(
    "HF Symptoms",
    "HF Precipitamts",
    "Echocardiogram date",
    "ECG on Admission",
    "LVEF %",
    "Heart Rate from ECG",
    "Heart Failure Risk Factors",
    "HF Possible precipitants",
    "NYHA Classification",
    "Cardiac Device",
    "Aetiology of CCF"
  )
  return(fields)
}




#' Pivots the HF enrolment form
#'
#' Pivots the HF enrolment forms from long form to wide
#' based on the fields given in `get_enrolment_fields()`
#' for each encounter.
#' For encounters with multiple enrolment forms,
#' the most recent, non-null result will be used
#'
#' @seealso [hefd::get_enrolment_fields()] for list of fields extracts,
#'    [hefd::get_first_non_na()] for definition for non empty fields, and
#'    [hefd::get_hfenrolment_form_query()] for the dataframe that will be pivoted
#' @family hfenrolment
#' @export
process_hfenrolment_form <- function(){
  forms <- execute_query(get_hfenrolment_form_query())

  result <- forms %>%
    dplyr::filter(TASK_ASSAY_CD %in% get_enrolment_fields())

  form_complete_time <-  result %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::summarise(FORM_DT_TM = max(FORM_DT_TM),
                     UPDT_DT_TM = max(UPDT_DT_TM))

  temp <- result %>%
    dplyr::mutate(TASK_ASSAY_CD =  toupper(gsub('[[:punct:] ]+',"",TASK_ASSAY_CD))) %>%
    dplyr::arrange(desc(UPDT_DT_TM)) %>%
    tidyr::pivot_wider(id_cols = c(ENCNTR_ID,PERSON_ID),
                       names_from = TASK_ASSAY_CD,
                       values_from = c(RESULT_VAL,CLINICAL_EVENT_ID),
                       names_sort = TRUE,
                       values_fn = get_first_non_na) %>%
    dplyr::left_join(form_complete_time,by="ENCNTR_ID") %>%
    dplyr::mutate(RESULT_VAL_ECHOCARDIOGRAMDATE = lubridate::dmy_hm(RESULT_VAL_ECHOCARDIOGRAMDATE)) %>%
    process_hfmanagement()

  return(temp)
}

#' Process HF enrolment forms
#'
#' Processes the fields of the HF enrolment forms
#' Fields list `RESULT_VAL_ECGONADMISSION`,
#' `RESULT_VAL_HFPRECIPITAMTS`, `RESULT_VAL_AETIOLOGYOFCCF`
#' will be seperated into different columns based on their value
#' LVEF will be binned accordingly
#' The number of precipiants will be calculated
#'
#' @param df the dataframe to processed
#' @seealso [hefd::process_hfenrolment_form()] for more details on the dataframe used
#' @family hfenrolment
#' @export
process_hfmanagement <- function(df){

  lvef_bins <- get_lvef_bin()
  df %<>%
    dplyr::mutate(ATRIAL_FABRILLATION_ECGONADMISSION  = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Atrial Fibrillation"),
                  LEFT_BBB_ECGONADMISSION  = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Left bundle-branch block"),
                  ATRIAL_FLUTTER_ECGONADMISSION = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Atrial flutter"),
                  PPM_ECGONADMISSION = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)PPM"),
                  SINUS_BRADYCARDIA_ECGONADMISSION = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Sinus bradycardia"),
                  SINUS_RHYTHM_ECGONADMISSION = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Sinus rhythm"),
                  RIGHT_BBB_ECGONADMISSION = stringr::str_detect(RESULT_VAL_ECGONADMISSION, "(?i)Right bundle-branch block")) %>%
    dplyr::mutate(ARRYTHMIA_HFPRECIPITANTS  = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)arrythmia") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)arrythmia"),
                  INFECTION_HFPRECIPITANTS  = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)infection") |stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)infection"),
                  CHANGE_MEDICATION_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)change in medication") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)change in medication"),
                  ISCHAEMIA_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)ischaemia") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)ischaemia"),
                  POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)Poor compliance with medication") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)Poor compliance with medication"),
                  NONE_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)no known"),
                  OTHER_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)Other:") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)Other:"),
                  RECENT_AMI_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)ami") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)ami"),
                  POOR_COMPLIANCE_DIET_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)Poor compliance with diet") |
                    stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)Poor compliance with diet"),
                  FLU_VAC_HFPRECIPITANTS = stringr::str_detect(RESULT_VAL_HFPRECIPITAMTS, "(?i)flu") | stringr::str_detect(RESULT_VAL_HFPOSSIBLEPRECIPITANTS, "(?i)flu")) %>%
    dplyr::mutate(ISCHAEMIC_AETIOLOGYOFCCF  = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)ISCHAEMIC"),
                  ALCOHOLIC_AETIOLOGYOFCCF  = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)ALCOHOLIC"),
                  AORTIC_STENOSIS_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)AORTIC STENOSIS"),
                  IDIOPATHIC_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)IDIOPATHIC"),
                  OTHER_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)OTHER"),
                  HYPERTENSION_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)HYPERTENSION"),
                  HYPERTROPHIC_AETIOLOGYOFCCF = stringr::str_detect(RESULT_VAL_AETIOLOGYOFCCF, "(?i)HYPERTROPHIC")) %>%
    dplyr::mutate(LVEF_BIN = cut(as.numeric(stringr::str_remove_all(RESULT_VAL_LVEF, '[^0-9.-]')),
                                            breaks=lvef_bins$values,
                                            labels=lvef_bins$labels)) %>%
    dplyr::mutate(
      N_PRECIPITANTS =
        ARRYTHMIA_HFPRECIPITANTS
      + INFECTION_HFPRECIPITANTS
      + CHANGE_MEDICATION_HFPRECIPITANTS
      + ISCHAEMIA_HFPRECIPITANTS
      + POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS
      + OTHER_HFPRECIPITANTS
      + RECENT_AMI_HFPRECIPITANTS
      + POOR_COMPLIANCE_DIET_HFPRECIPITANTS
      + FLU_VAC_HFPRECIPITANTS,
      NONE_HFPRECIPITANTS =
        !ARRYTHMIA_HFPRECIPITANTS
      & !INFECTION_HFPRECIPITANTS
      & !CHANGE_MEDICATION_HFPRECIPITANTS
      & !ISCHAEMIA_HFPRECIPITANTS
      & !POOR_COMPLIANCE_MEDICATION_HFPRECIPITANTS
      & !OTHER_HFPRECIPITANTS
      & !RECENT_AMI_HFPRECIPITANTS
      & !POOR_COMPLIANCE_DIET_HFPRECIPITANTS
      & !FLU_VAC_HFPRECIPITANTS
    )
    ##get_n_precipitants(HFPRECIPITANTS = "RESULT_VAL_HFPRECIPITAMTS",
    ##                   HFPOSSIBLEPRECIPITANTS = "RESULT_VAL_HFPOSSIBLEPRECIPITANTS")
    return(df)
}

#' Get most recent and first enrolment
#'
#' Gets the most and first enrolment for each person
#' @param df
#' @seealso [hefd::process_hfenrolment_form()] for more details
#'   on the df used
#' @family hfenrolment
#' @export
summarise_enrolment <- function(df){
  summary <- df %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::arrange(FORM_DT_TM) %>%
    dplyr::summarise(N_ENROLMENT = dplyr::n(),
                     FIRST_ENCNTR_ID = dplyr::first(ENCNTR_ID),
                     MOST_RECENT_ENCNTR_ID = dplyr::last(ENCNTR_ID),
                     UPDT_DT_TM = dplyr::last(UPDT_DT_TM))

  return(summary)
}



#' Computes the number of precipiants and possible precipitants
#'
#' Computes the number of precipiants and possible precipitants in the hfmanagement form
#' Field is considered missing when both possible and precipiants is left empty
#' Number of precipiants is considered 0 when no known precipiants is field
#' @param hfmanagement a df containing form fields of the enrolment form
#' @return a df, the hfmanagemet table with the number of precipitants and possible precipitants
#' @seealso [hefd::process_hfenrolment_form()]
#' @family hfenrolment
#' @export
get_n_precipitants <- function(hfmanagement,
                               HFPRECIPITANTS = "HFPRECIPITAMTS",
                               HFPOSSIBLEPRECIPITANTS = "HFPOSSIBLEPRECIPITANTS",
                               PERSON_ID = "PERSON_ID"){
  col_syms <- rlang::syms(
    list(
      "PRECIP" = HFPRECIPITANTS,
      "POSSIBLE" = HFPOSSIBLEPRECIPITANTS,
      "PERSON_ID" = PERSON_ID
    ))

  n_precipitants<- hfmanagement %>%
    tidyr::unite(PRECIP_COMB, !!col_syms[["PRECIP"]], !!col_syms[["POSSIBLE"]],sep=",") %>%
    tidyr::separate_rows(PRECIP_COMB,sep=',') %>%
    dplyr::filter(PRECIP_COMB != ',',PRECIP_COMB != '',!stringr::str_detect(PRECIP_COMB, "(?i)no known")) %>%
    dplyr::group_by(!!col_syms[["PERSON_ID"]]) %>%
    dplyr::summarise(N_PRECIPITANTS = dplyr::n_distinct(PRECIP_COMB))

  hfmanagement %<>%
    dplyr::left_join(n_precipitants, by = PERSON_ID) %>%
    dplyr::mutate(N_PRECIPITANTS =
                    dplyr::case_when(!!col_syms[["PRECIP"]] == '' & !!col_syms[["POSSIBLE"]] == '' ~ NA_real_,
                                     !is.na(N_PRECIPITANTS) ~ as.double(N_PRECIPITANTS),
                                     TRUE ~ 0))
  return(hfmanagement)
}






