


#' @export
extract_diagnosis <- function(df){
  diag <- execute_query(get_diagnosis_query())

  df %<>% dplyr::left_join(
    diag,by="ENCNTR_ID") %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::arrange(DIAG_PRIORITY,desc(BEG_EFFECTIVE_DT_TM.y)) %>%
    dplyr::slice_head() %>%
    dplyr::mutate(PRIMARY_DIAG = dplyr::case_when(
      DIAG_PRIORITY > 1 ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    dplyr::rename(DIAG_UPDT_DT_TM = UPDT_DT_TM.y) %>%
    dplyr::select(ENCNTR_ID,DIAGNOSIS_ID,PRIMARY_DIAG,DIAG_PRIORITY,SOURCE_IDENTIFIER,SOURCE_STRING,DIAG_UPDT_DT_TM)
  return(df)
}


#' calculates age and LOS
#' @export
extract_age_los <- function(df){


  AGE_breaks <- c(-Inf,18, 25, 45, 65, 80, Inf)
  AGE_labels <- c("<18", "19-24", "25-44", "45-64", "65-80", "80+")

  enc <- df %>%
    dplyr::mutate(AGE = as.integer(difftime(ACTIVE_STATUS_DT_TM,BIRTH_DT_TM, units="weeks")/52.25))%>%
    dplyr::mutate(AGE_BINNED = cut(AGE, breaks = AGE_breaks, labels = AGE_labels,
                                   right = FALSE)) %>%
    dplyr::mutate(LOS = as.numeric(difftime(DISCH_DT_TM,CREATE_DT_TM,units="days"))) %>%
    dplyr::rename(ENCNTR_UPDT_DT_TM = UPDT_DT_TM) %>%
    dplyr::select(ENCNTR_ID,AGE,AGE_BINNED,LOS,ENCNTR_UPDT_DT_TM)
  return(enc)
}



#' extracts advanced care directives and checks whether the alert was
#' created before or during the encounter
#' @export
extract_alert <- function(df){
  problem <- execute_query(get_problem_query())

  alert <- df %>%
    dplyr::left_join(problem,by="PERSON_ID") %>%
    dplyr::mutate(ALERT_BEFORE_ENC  =
                    !is.null(PROBLEM_ID) &
                    BEG_EFFECTIVE_DT_TM.y < CREATE_DT_TM) %>%
    dplyr::mutate(ALERT_DURING_ENC =
                    !is.null(PROBLEM_ID) &
                    BEG_EFFECTIVE_DT_TM.y >= CREATE_DT_TM &
                    BEG_EFFECTIVE_DT_TM.y <= DISCH_DT_TM) %>%
    dplyr::rename(ALERT_BEG_EFFECTIVE_DT_TM = BEG_EFFECTIVE_DT_TM.y,
                  ALERT_UPDT_DT_TM = UPDT_DT_TM.y) %>%
    dplyr::select(ENCNTR_ID,ALERT_BEFORE_ENC,ALERT_DURING_ENC,ALERT_BEG_EFFECTIVE_DT_TM,ALERT_UPDT_DT_TM) %>%
    tidyr::replace_na(list(ALERT_BEFORE_ENC = FALSE,ALERT_DURING_ENC=FALSE))

    return(alert)
}

#' extracts the most recent echo order to the encounter
#' @export
extract_echos <- function(df){
  echos <-  execute_query(get_echos_query())

  echo_enc <- df %>%
    get_most_recent_order(echos,"ECHO","ORIG_ORDER_DT_TM") %>%
    dplyr::select(ENCNTR_ID,ORDER_ID,ECHO_ENCNTR_ID,ORIG_ORDER_DT_TM,ECHO_DELTA,UPDT_DT_TM) %>%
    dplyr::rename(ECHO_ORDER_ID = ORDER_ID,
                  ECHO_ORIG_ORDER_DT_TM = ORIG_ORDER_DT_TM,
                  ECHO_UPDT_DT_TM = UPDT_DT_TM)
  return(echo_enc)
}

#' a helper function to extract the most recent event/order
#' based on the given time column
get_most_recent_order <- function(df,order_df,suffix,
                                  order_dttm_col,
                                  df_dttm = "DISCH_DT_TM",
                                  delta_cutoff = 0){
  order_dttm_col <- rlang::sym(order_dttm_col)
  df_dttm <-  rlang::sym(df_dttm)

  encntr_link <- rlang::sym(paste0("ENCNTR_ID_",suffix))

  same_encntr_link <- rlang::sym(paste0(suffix,"_DIFF_ENCNTR_ID"))

   # get any orders before disch
   # if they were after discharge, they must be from the same encntr
  df %<>%
    dplyr::select(ENCNTR_ID,PERSON_ID,!!df_dttm) %>%
    dplyr::left_join(order_df,by="PERSON_ID",suffix=c("",paste0("_",suffix))) %>%
    dplyr::mutate(DELTA = as.numeric(difftime(!!df_dttm,!!order_dttm_col,units="days")),
                  "{suffix}_DIFF_ENCNTR_ID" := ENCNTR_ID != !!encntr_link) %>%
    dplyr::filter(DELTA >= delta_cutoff | (DELTA < delta_cutoff & ENCNTR_ID == !!encntr_link)) %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::arrange(as.numeric(!!same_encntr_link),DELTA) %>%
    dplyr::slice(1) %>%
    #dplyr::slice_min(order_by = c(as.numeric(!!same_encntr_link),DELTA),with_ties = FALSE) %>%
    dplyr::rename("{suffix}_DELTA" := DELTA,
                  "{suffix}_ENCNTR_ID" := !!encntr_link)


  return(df)
}

#' extracts the result and bin pathology result for a given rest
#' @export
extract_path_result <- function(df,regex,prefix,breaks=NULL,labels=NULL){
  df %<>% dplyr::filter(stringr::str_detect(EVENT_CD,regex)) %>%
    dplyr::select(ENCNTR_ID,PERSON_ID,CLINICAL_EVENT_ID,EVENT_START_DT_TM,RESULT_VAL,RESULT_UNITS_CD,UPDT_DT_TM) %>%
    dplyr::mutate(RESULT_VAL = as.numeric(stringr::str_remove_all(RESULT_VAL, '[^0-9.-]')) )  %>%
    dplyr::filter(!is.na(RESULT_VAL)) %>%
    dplyr::rename("{prefix}_EVENT_START_DT_TM" := EVENT_START_DT_TM,

                  "{prefix}_RESULT_UNITS_CD" := RESULT_UNITS_CD,
                  "{prefix}_CLINICAL_EVENT_ID" := CLINICAL_EVENT_ID,
                  "{prefix}_UPDT_DT_TM" := UPDT_DT_TM)

  if(!is.null(breaks)){
    df %<>% dplyr::mutate("{prefix}_BIN" := cut(RESULT_VAL,breaks=breaks,labels=labels))
  }
  df %<>% dplyr::rename("{prefix}_RESULT_VAL" := RESULT_VAL)

  return(df)
}


#' @export
extract_patho_result <- function(df){
  patho <- execute_query(get_pathology_query())

  # combine BNP tests to the same group
  # and take the last result of the encounter
  path_slice <- patho %>%
    dplyr::mutate(EVENT_CD = dplyr::case_when(
      stringr::str_detect(EVENT_CD,"(Brain Natriuretic peptide)|(N Terminal pro BNP)|(NT-ProBNP)") ~ "BNP",
      TRUE ~ EVENT_CD
    )) %>%
    dplyr::group_by(ENCNTR_ID,EVENT_CD) %>%
    dplyr::arrange(desc(EVENT_START_DT_TM)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # extract the different pathology results
  bnp <- extract_path_result(path_slice,"BNP", "BNP", c(-Inf,450,900,Inf),c("< 450","450-900","900+"))
  trans_sat <- extract_path_result(path_slice,"Transferrin Saturation", "TRANSFERRIN_SAT",c(-Inf,20,Inf),c("< 20","20+"))
  ferritin <- extract_path_result(path_slice,"Ferritin", "FERRITIN",c(-Inf,100,300,Inf),c("< 100","100-299","300+"))

  # attach them to the encounter
  df %<>%
    dplyr::select(ENCNTR_ID) %>%
    dplyr::left_join(
      df %>%
        get_most_recent_order(bnp,"BNP","BNP_EVENT_START_DT_TM") %>%
        dplyr::select(ENCNTR_ID,dplyr::starts_with("BNP")),by="ENCNTR_ID") %>%
    dplyr::left_join(
      df %>%
        get_most_recent_order(trans_sat,"TRANSFERRIN_SAT","TRANSFERRIN_SAT_EVENT_START_DT_TM") %>%
        dplyr::select(ENCNTR_ID,ENCNTR_ID,dplyr::starts_with("TRANSFERRIN_SAT")),by="ENCNTR_ID") %>%
    dplyr::left_join(
      df %>%
        get_most_recent_order(ferritin,"FERRITIN","FERRITIN_EVENT_START_DT_TM") %>%
        dplyr::select(ENCNTR_ID,ENCNTR_ID,dplyr::starts_with("FERRITIN")),by="ENCNTR_ID") %>%
    dplyr::mutate(IS_IRON_DEFICIENT =
                    dplyr::case_when(
                      FERRITIN_RESULT_VAL < 100 |
                        FERRITIN_RESULT_VAL >= 100 & FERRITIN_RESULT_VAL < 300 &
                        TRANSFERRIN_SAT_RESULT_VAL < 20 ~ TRUE,
                      TRUE ~ FALSE
                    ))
  return(df)
}

#' extract the latest hf related icd10 diagnosis for each encounter
#' @export
extract_hf_hist_diag <- function(df){
  hf_hist_diag <- execute_query(get_historical_hf_diag_query()) %>%
    dplyr::group_by(PX9_ENCNTR_ID) %>%
    dplyr::slice_max(order_by = DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,with_ties = FALSE) %>%
    dplyr::select(PX9_ENCNTR_ID,DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,DIAGNOSIS_ID,DIAG_PRIORITY,
                  ENCNTR_ID,SOURCE_IDENTIFIER,SOURCE_STRING,HISTORICAL_DIAG,UPDT_DT_TM) %>%
    dplyr::rename(ENCNTR_ID = PX9_ENCNTR_ID,
                  DIAG_ENCNTR_ID = ENCNTR_ID)

  return(hf_hist_diag)
}


#' create discharge medications from encounters
#' @export
extract_discharge_meds <- function(df){

  meds <- execute_query(get_disch_med_query(df$ENCNTR_ID))


  # create a list of columns names we should expect
  med_group <- load_db("medication_group")

  # create a named list filled with FALSE
  # used for replace_na later
  upper_groups <- toupper(gsub('[[:punct:] ]+','_',sort(med_group$MEDICATION_GROUP)))
  list_name <-   purrr::map_chr(upper_groups, ~ paste0("HAS_DISCHARGE_", .))
  values <- rep(FALSE, length(list_name))
  names(values) <- list_name


  summarised_meds<-meds %>%
    dplyr::mutate(HAS_DISCHARGE = TRUE,
                  MEDICATION_GROUP = toupper(gsub('[[:punct:] ]+','_',MEDICATION_GROUP))) %>%
    dplyr::group_by(ENCNTR_ID,MEDICATION_GROUP) %>%
    dplyr::arrange(desc(UPDT_DT_TM)) %>% #PICK THE MOST RECENT MEDICATION FOR THE GROUP
    dplyr::slice(1) %>%
    tidyr::pivot_wider(id_cols = ENCNTR_ID,
                       names_from = MEDICATION_GROUP,
                       values_from = c(HAS_DISCHARGE,ORDER_ID,HOME_MED),
                       names_sep = "_") %>%

    dplyr::ungroup()




  full_col <- c(purrr::map_chr(upper_groups, ~ paste0("HAS_DISCHARGE_", .)),
                purrr::map_chr(upper_groups, ~ paste0("HOME_MED_", .)),
                purrr::map_chr(upper_groups, ~ paste0("ORDER_ID_", .)))

  # the pivot may cause some missing columns
  # add the missing columns with NA
  missing_col <- setdiff(full_col,colnames(summarised_meds))
  summarised_meds[missing_col] <- NA


  updt_dttm <- meds %>%
    dplyr::group_by(ENCNTR_ID) %>%
    dplyr::summarise(UPDT_DT_TM = max(UPDT_DT_TM))

  # replace the boolean column with FALSES
  # ensure columns are in the correct order
  summarised_meds %<>%
    tidyr::replace_na(as.list(values)) %>%
    dplyr::select(ENCNTR_ID,full_col) %>%
    dplyr::left_join(
      updt_dttm, by ="ENCNTR_ID"
    )

  return(summarised_meds)
}


#' @export
process_inpatient <- function(df){

  ref <- execute_query(get_discharge_referral())
  emds_ref <- ref %>%
    dplyr::filter(stringr::str_detect(EVENT_TITLE_TEXT,"(?i)emeds"))

  inpatient <- df %>%
    dplyr::select(ENCNTR_ID,PERSON_ID,DISCH_DT_TM,BEG_EFFECTIVE_DT_TM) %>%
    dplyr::left_join(
      df %>% extract_age_los(),by = 'ENCNTR_ID') %>%
    dplyr::left_join(
      df %>% extract_alert(),by = 'ENCNTR_ID') %>%
    dplyr::mutate(
      HAS_DISCHARGE_REFERRAL = ENCNTR_ID %in% ref$ENCNTR_ID,
      HAS_EMEDS_REFERRAL = ENCNTR_ID %in% emds_ref$ENCNTR_ID,
    ) %>%
    dplyr::left_join(
      df %>% extract_diagnosis(),by = 'ENCNTR_ID')

  return(inpatient)

}

#' @export
extract_hfreferral_form <- function(df){
  referral_forms <- execute_query(get_hfreferral_form_query()) %>%
    dplyr::select(PERSON_ID,ENCNTR_ID,FORM_DT_TM,UPDT_DT_TM,DCP_FORMS_ACTIVITY_ID,PARENT_ENTITY_ID) %>%
    dplyr::rename(REFERRAL_UPDT_DT_TM = UPDT_DT_TM,
                  REFERRAL_FORM_DT_TM = FORM_DT_TM,
                  REFERRAL_DCP_FORMS_ACTIITY_ID = DCP_FORMS_ACTIVITY_ID,
                  REFERRAL_PARENT_ENTITY_ID = PARENT_ENTITY_ID)

  referrals <- get_most_recent_order(df,referral_forms,"REFERRAL","REFERRAL_FORM_DT_TM") %>%
    dplyr::select(ENCNTR_ID,dplyr::starts_with("REFERRAL_"))

  return(referrals)

}

#' @export
extract_hfenrolment_form <- function(df){
  forms <- execute_query(get_hfenrolment_form_query())


  lvef <- extract_form_result(forms,"LVEF", "LVEF", c(-Inf,40,50,Inf),c("< 40","40-49","50+"),is_numeric=TRUE)

  enrolment <- df %>%
    get_most_recent_order(forms,"ENROLMENT","FORM_DT_TM",delta_cutoff = -30) %>%
    dplyr::rename(ENROLMENT_FORM_DT_TM = FORM_DT_TM,
                  ENROLMENT_PARENT_EVENT_ID = PARENT_EVENT_ID,
                  ENROLMENT_UPDT_DT_TM = UPDT_DT_TM) %>%
    dplyr::select(ENCNTR_ID,dplyr::starts_with("ENROLMENT")) %>%
    dplyr::left_join(
      df %>%
        get_most_recent_order(lvef,"LVEF","LVEF_FORM_DT_TM") %>%
        dplyr::select(ENCNTR_ID,dplyr::starts_with("LVEF")),by="ENCNTR_ID")



  return(enrolment)

}

#' extracts the result and bin pathology result for a given rest
#' @export
extract_form_result <- function(df,regex,prefix,breaks=NULL,labels=NULL,is_numeric=FALSE){
  if(is_numeric){
    df %<>% dplyr::filter(stringr::str_detect(TASK_ASSAY_CD,regex)) %>%
      dplyr::mutate(RESULT_VAL = as.numeric(stringr::str_remove_all(RESULT_VAL, '[^0-9.-]')) )  %>%
      dplyr::filter(!is.na(RESULT_VAL)) %>%
      dplyr::rename("{prefix}_FORM_DT_TM" := FORM_DT_TM,

                    "{prefix}_RESULT_UNITS_CD" := RESULT_UNITS_CD,
                    "{prefix}_CLINICAL_EVENT_ID" := CLINICAL_EVENT_ID,
                    "{prefix}_UPDT_DT_TM" := UPDT_DT_TM,
                    "{prefix}_PARENT_EVENT_ID" := PARENT_EVENT_ID)

    if(!is.null(breaks)){
      df %<>% dplyr::mutate("{prefix}_BIN" := cut(RESULT_VAL,breaks=breaks,labels=labels))
    }
    df %<>% dplyr::rename("{prefix}_RESULT_VAL" := RESULT_VAL)
  }

  return(df)
}


