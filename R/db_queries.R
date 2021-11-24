

#' @export
get_hfhomevisit_form_query <- function(){
  query <- "select x.ENCNTR_ID,x.PERSON_ID,x.FORM_DT_TM,y.PARENT_EVENT_ID,y.TASK_ASSAY_CD,y.RESULT_VAL,x.UPDT_DT_TM from DCP_FORMS_ACTIVITY as x
  left join FORMS_EVENT as y
  on PARENT_EVENT_ID = PARENT_ENTITY_ID
  where DESCRIPTION = 'Heart Failure - Home Visit/Phone Call Assessment';"
  return(query)
}




#' @export
get_hf_encounter_query <- function(){
  query <- "SELECT x.*,BIRTH_DT_TM,SEX_CD,ZIPCODE,DECEASED_DT_TM
  FROM ENCOUNTER_REFERENCE as z
  left join ENCOUNTER as x
  on z.PX9_ENCNTR_ID = x.ENCNTR_ID and z.PX9_PERSON_ID = x.PERSON_ID
  left join PATIENT as y
  on x.PERSON_ID = y.PERSON_ID
  where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1 and (y.PERSON_ID is not NULL or x.ENCNTR_ID is not NULL);"
  return(query)
}

#' get the most recently updated note for each encounter
#' @export
get_discharge_referral <- function(){
  query <- "
  SELECT  k.ENCNTR_ID,k.UPDT_DT_TM as NOTE_UPDT_DT_TM,
  k.EVENT_ID AS NOTE_EVENT_ID,k.EVENT_TAG,k.EVENT_TITLE_TEXT
  FROM ENCOUNTER_REFERENCE as z
  left join NOTES_BLOB as k
  on k.ENCNTR_ID = z.PX9_ENCNTR_ID
  where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1;
  "
  return(query)
}

#' queries the first instance
#' the advance care directive for a given `PERSON_ID`
#' @export
get_problem_query <- function(){
  query <- "
  WITH added_row_number AS (
SELECT *,
    ROW_NUMBER() OVER(PARTITION BY PERSON_ID ORDER BY BEG_EFFECTIVE_DT_TM) AS row_number
  FROM proBLEM
  where SOURCE_STRING like 'Known to Heart Failure Support Services' or SOURCE_STRING = 'Known to Heart Failure Support Servces'
 )
 SELECT
  *
FROM added_row_number
WHERE row_number = 1;"
  return(query)
}

#' @export
get_echos_query <- function(){
  query <- "
  SELECT * from ORDERS as x
  where  CATALOG_CD = 'Cardio - Echo (LV & RV Function, CCF)'"
  return(query)
}

#' @export
get_pathology_query <- function(){
  query <- "SELECT * from PATHOLOGY_EVENT
  where CATALOG_CD in  ('Brain Natriuretic Peptide','Iron Level','Iron Studies','N Terminal Pro Brain Natriuretic Peptide') and
  PERSON_ID in (SELECT PX9_PERSON_ID from ENCOUNTER_REFERENCE
  where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1);"
  return(query)
}


#' @export
get_historical_hf_diag_query <- function(){
  query <- "select PX9_ENCNTR_ID,temp.*,0 AS HISTORICAL_DIAG from ENCOUNTER_REFERENCE as x
    left join ENCOUNTER AS z
    on PX9_ENCNTR_ID = z.ENCNTR_ID and PX9_PERSON_ID = z.PERSON_ID
    left join (
      select j.BEG_EFFECTIVE_DT_TM as DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,k.* from DIAGNOSIS as k
      left join ENCOUNTER as j
      on j.ENCNTR_ID = k.ENCNTR_ID
      where Source_VOCABULARY_CD = 'ICD10-AM' and SOURCE_IDENTIFIER like 'I50%'
    ) as temp
    on temp.PERSON_ID = PX9_PERSON_ID and z.BEG_EFFECTIVE_DT_TM > temp.DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM
    where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1
  union all
    select PX9_ENCNTR_ID,temp.*,1 AS HISTORICAL_DIAG from ENCOUNTER_REFERENCE as x
    left join ENCOUNTER AS z
    on PX9_ENCNTR_ID = z.ENCNTR_ID and PX9_PERSON_ID = z.PERSON_ID
    left join (
      select j.BEG_EFFECTIVE_DT_TM as DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,k.* from DIAGNOSIS_HISTORY as k
      left join ENCOUNTER_HISTORY as j
      on j.ENCNTR_ID = k.ENCNTR_ID
      where Source_VOCABULARY_CD = 'ICD10-AM' and SOURCE_IDENTIFIER like 'I50%'
    ) as temp
    on temp.PERSON_ID = PX9_PERSON_ID and z.BEG_EFFECTIVE_DT_TM > temp.DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM
    where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1  and z.ENCNTR_ID is not NULL;"
  return(query)
}

#' include a list of `ENCNTR_ID`
#' @export
get_disch_med_query <- function(encntr_list){
  query <- paste0("select x.ENCNTR_ID,y.ORDER_ID,y.UPDT_DT_TM,q.MEDICATION_GROUP, 1 as HOME_MED from ENCOUNTER as x
    left join MEDICATION_HOME_ORDERS as y
    on x.PERSON_ID = y.PERSON_ID  and y.CURRENT_START_DT_TM <= x.DISCH_DT_TM and
    (ORDER_STATUS_CD = 'Completed' and STATUS_DT_TM > x.DISCH_DT_TM or
    	ORDER_STATUS_CD = 'Ordered' or
    	ORDER_STATUS_CD = 'Discontinued' and DISCONTINUE_EFFECTIVE_DT_TM > x.DISCH_DT_TM or
    	ORDER_STATUS_CD = 'Deleted' and STATUS_DT_TM > x.DISCH_DT_TM)
     LEFT JOIN MEDICATION_LIST as z

      on CATALOG_CD = MEDICATION_NAME
      left join MEDICATION_GROUP as q
    	on z.MEDICATION_GROUP_ID = q.MEDICATION_GROUP_ID
    where x.ENCNTR_ID in (",paste(encntr_list,collapse=","),") and q.MEDICATION_GROUP is not NULL
        union all
     select x.ENCNTR_ID,y.ORDER_ID,y.UPDT_DT_TM,q.MEDICATION_GROUP, 0 as HOME_MED from ENCOUNTER as x
    left join MEDICATION_ORDERS as y
    on x.ENCNTR_ID = y.ENCNTR_ID and
  (ORDER_STATUS_CD = 'Completed' and STATUS_DT_TM > x.DISCH_DT_TM or
  	ORDER_STATUS_CD = 'Ordered' or
  	ORDER_STATUS_CD = 'Discontinued' and DISCONTINUE_EFFECTIVE_DT_TM > x.DISCH_DT_TM or
  	ORDER_STATUS_CD = 'Deleted' and STATUS_DT_TM > x.DISCH_DT_TM)
     LEFT JOIN MEDICATION_LIST as z
      on CATALOG_CD = MEDICATION_NAME
      left join MEDICATION_GROUP as q
    	on z.MEDICATION_GROUP_ID = q.MEDICATION_GROUP_ID
    where y.ORIG_ORD_AS_FLAG =  1 and x.ENCNTR_ID in (",paste(encntr_list,collapse=","),") and q.MEDICATION_GROUP is not NULL; ")

  return(query)
}

#' @export
get_diagnosis_query <- function(){
  query <- "SELECT * from diagnosis
  where SOURCE_IDENTIFIER in ('I50','I50.0','I50.1','I50.9');"
  return(query)
}


#' @export
get_hfenrolment_form_query <- function(){
  query <- "
  select x.ENCNTR_ID,x.PERSON_ID,x.FORM_DT_TM,y.PARENT_EVENT_ID,y.TASK_ASSAY_CD,y.RESULT_VAL,y.CLINICAL_EVENT_ID,y.RESULT_UNITS_CD,x.UPDT_DT_TM from DCP_FORMS_ACTIVITY as x
  left join FORMS_EVENT as y
  on PARENT_EVENT_ID = PARENT_ENTITY_ID
  where DESCRIPTION = 'Heart Failure - Management of Cardiac Function Enrolment';"
  return(query)
}


#' @export
get_hfreferral_form_query <- function(){
  query <- "select * from DCP_FORMS_ACTIVITY
    where DESCRIPTION = 'Heart Failure (MACARF) Referral Form';"

return(query)
}

