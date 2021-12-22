
#' @family query
#' @export
get_hfhomevisit_form_query <- function(){
  query <- "select x.ENCNTR_ID,x.PERSON_ID,x.FORM_DT_TM,y.PARENT_EVENT_ID,y.TASK_ASSAY_CD,y.RESULT_VAL,x.UPDT_DT_TM from DCP_FORMS_ACTIVITY as x
  left join FORMS_EVENT as y
  on PARENT_EVENT_ID = PARENT_ENTITY_ID
  where DESCRIPTION = 'Heart Failure - Home Visit/Phone Call Assessment';"
  return(query)
}



#' @family query
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
#'
#' @family query
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
#'
#'
#' extracts the earliest record of the directive
#' @family query
#' @export
get_acd_problem_query <- function(){
  query <- "
  WITH added_row_number AS (
SELECT *,
    ROW_NUMBER() OVER(PARTITION BY PERSON_ID ORDER BY BEG_EFFECTIVE_DT_TM) AS row_number
  FROM proBLEM
  where SOURCE_STRING in ('Advance Care Directive','Advance Care Directives','Advance Care Plan','Advance Care Planning')
 )
 SELECT
  *
FROM added_row_number
WHERE row_number = 1;"
  return(query)
}

#' queries the first instance
#' the known to heart failure for a given `PERSON_ID`
#'
#' @family query
#' @export
get_hf_alert_query <- function(){
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

#' @family query
#' @export
get_echos_query <- function(){
  query <- "
  SELECT * from ORDERS as x
  where  CATALOG_CD = 'Cardio - Echo (LV & RV Function, CCF)'"
  return(query)
}

#' @family query
#' @export
get_pathology_query <- function(person_id = NULL){
  # Brain Natriuretic Peptide has been excluded
  # due to instability of measurement

  if(is.null(person_id)){
    person_string <- "SELECT PX9_PERSON_ID from ENCOUNTER_REFERENCE
  where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1"
  }
  else{
    person_string <- paste(person_id,collapse=",")
  }
  query <- paste0("SELECT * from PATHOLOGY_EVENT
  where CATALOG_CD in  ('Iron Level','Iron Studies','N Terminal Pro Brain Natriuretic Peptide') and
  PERSON_ID in (", person_string,");")
  return(query)
}

#' queries historical heart failure diagnosis
#'
#' @family query
#' @family deprecated
#' `r lifecycle::badge("deprecated")`
#' @export
get_historical_hf_diag_query <- function(){
  lifecycle::deprecate_stop("1.0.0", "get_historical_hf_diag_query()")

  query <- "select PX9_ENCNTR_ID,temp.*,0 AS HISTORICAL_DIAG from ENCOUNTER_REFERENCE as x
    left join ENCOUNTER AS z
    on PX9_ENCNTR_ID = z.ENCNTR_ID and PX9_PERSON_ID = z.PERSON_ID
    left join (
      select j.BEG_EFFECTIVE_DT_TM as DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,j.DISCH_DT_TM as DIAG_ENCNTR_DISCH_DT_TM,k.* from DIAGNOSIS as k
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
      select j.BEG_EFFECTIVE_DT_TM as DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM,j.DISCH_DT_TM as DIAG_ENCNTR_DISCH_DT_TM,k.* from DIAGNOSIS_HISTORY as k
      left join ENCOUNTER_HISTORY as j
      on j.ENCNTR_ID = k.ENCNTR_ID
      where Source_VOCABULARY_CD = 'ICD10-AM' and SOURCE_IDENTIFIER like 'I50%'
    ) as temp
    on temp.PERSON_ID = PX9_PERSON_ID and z.BEG_EFFECTIVE_DT_TM > temp.DIAG_ENCNTR_BEG_EFFECTIVE_DT_TM
    where Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1  and z.ENCNTR_ID is not NULL;"
  return(query)
}

#' Extracts discharge meds
#'
#' include a list of `ENCNTR_ID`
#' @family query
#' @export
get_disch_med_query <- function(encntr_list){
  query <- paste0("
  with MEDS AS (
  (select x.ENCNTR_ID,y.ORDER_ID,y.UPDT_DT_TM,q.MEDICATION_GROUP, 1 as HOME_MED from ENCOUNTER as x
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
    where  q.MEDICATION_GROUP is not NULL)
        union all
     (select x.ENCNTR_ID,y.ORDER_ID,y.UPDT_DT_TM,q.MEDICATION_GROUP, 0 as HOME_MED from ENCOUNTER as x
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
    where y.ORIG_ORD_AS_FLAG =  1 and q.MEDICATION_GROUP is not NULL))
  select * from MEDS
  where ENCNTR_ID in (",paste(encntr_list,collapse=","),");")

  return(query)
}

#' @family query
#' @export
get_diagnosis_query <- function(){
  query <- "SELECT * from diagnosis
  where SOURCE_IDENTIFIER in ('I50','I50.0','I50.1','I50.9');"
  return(query)
}

#' Extracts enrolment forms
#'
#' @family query
#' @export
get_hfenrolment_form_query <- function(){
  query <- "
  select x.ENCNTR_ID,x.PERSON_ID,x.FORM_DT_TM,y.PARENT_EVENT_ID,y.TASK_ASSAY_CD,y.RESULT_VAL,y.CLINICAL_EVENT_ID,y.RESULT_UNITS_CD,x.UPDT_DT_TM from DCP_FORMS_ACTIVITY as x
  left join FORMS_EVENT as y
  on PARENT_EVENT_ID = PARENT_ENTITY_ID
  where DESCRIPTION = 'Heart Failure - Management of Cardiac Function Enrolment';"
  return(query)
}

#' Extracts referral forms
#'
#' @family query
#' @export
get_hfreferral_form_query <- function(){
  query <- "select * from DCP_FORMS_ACTIVITY
    where DESCRIPTION = 'Heart Failure (MACARF) Referral Form';"

return(query)
}

#' Extracts referred to facility
#'
#' @family query
#' @export
get_referred_to_facility_query <- function(){
  query <- "select ENCNTR_ID,VALUE_CD from ENCOUNTER_UDF
    where INFO_SUB_TYPE_CD = 'Referred to Facility';"

  return(query)
}

#' rank all inpatient encounters by date
#'
#' `r lifecycle::badge("deprecated")`
#' @family deprecated
#' @family query
#' @export
get_previous_encounter_query <- function(df,
                                         PERSON_ID = "PERSON_ID",
                                         ENCNTR_ID = "ENCNTR_ID"){
  lifecycle::deprecate_stop("1.0.0", "get_previous_encounter_query()")
  PERSON_ID_sym <- rlang::sym(PERSON_ID)
  ENCNTR_ID_sym <- rlang::sym(ENCNTR_ID)

  query <- paste0("
   with join_tab as (
    select ENCNTR_ID,PERSON_ID,BEG_EFFECTIVE_DT_TM,DISCH_DT_TM from encounter
    where ENCNTR_TYPE_CD in ('Inpatient','Emergency','Recurring Inpatient')
    union all
    select  ENCNTR_ID,PERSON_ID,BEG_EFFECTIVE_DT_TM,DISCH_DT_TM from encounter_history
    where ENCNTR_TYPE_CD in ('Inpatient','Emergency','Recurring Inpatient')
  ),
  added_row_number as(
    SELECT *,
    ROW_NUMBER() OVER(PARTITION BY PERSON_ID ORDER BY BEG_EFFECTIVE_DT_TM asc) AS ENCNTR_ORDER
    from join_tab

  )

  SELECT
  k.ENCNTR_ID,k.PERSON_ID,K.BEG_EFFECTIVE_DT_TM,k.DISCH_DT_TM,k.ENCNTR_ORDER, y.BEG_EFFECTIVE_DT_TM as PREV_BEG_EFFECTIVE_DT_TM,y.ENCNTR_ID as PREV_ENCNTR_ID,y.ENCNTR_ORDER AS PREV_ENCNTR_ORDER,
  datediff(hour,y.BEG_EFFECTIVE_DT_TM,k.DISCH_DT_TM) AS ENCNTR_DELTA
  FROM added_row_number as k
  left join added_row_number as y
  on k.ENCNTR_ORDER = y.ENCNTR_ORDER + 1 and k.person_id = y.person_id
  where k.", ENCNTR_ID_sym, " in (",paste(df[[ENCNTR_ID_sym]],collapse=","),")
  ")



  return(query)
}


#' extracts the encounters that haven't been allocated a journey id
#'
#' `join_tab` extracts encounters from the encounter table and encounter history table
#' `last_journey` gets the last journey currently for each patient
#' `last_encounter` gets the encounters associated for those journeys
#' using the above, we extract the encounters that haven't been allocated a journey key
#' and the most recent encounters for each patient, just in case their journeys
#' are continued with the most recent set of new encounters
#'
#' be aware that we assume that each encounter can only be associated with one person_id
#' across the encounter and encounter history tables
#' @family query
#' @export
get_encounter_journey_query <- function(){

  query <- paste0("with join_tab as (
      select ENCNTR_ID,PERSON_ID,BEG_EFFECTIVE_DT_TM,DISCH_DT_TM,UPDT_DT_TM,ADMIT_TYPE_CD,ENCNTR_TYPE_CD,0 as HIS_ENCNTR from encounter
      where ENCNTR_TYPE_CD in ('Inpatient','Emergency','NDF') and ADMIT_TYPE_CD is not null and LOC_FACILITY_CD != 'APAC'
      union all
      select  ENCNTR_ID,PERSON_ID,BEG_EFFECTIVE_DT_TM,DISCH_DT_TM,UPDT_DT_TM,ADMIT_TYPE_CD,ENCNTR_TYPE_CD,1 as HIS_ENCNTR from encounter_history
      where ENCNTR_TYPE_CD in ('Inpatient','Emergency','NDF') and ADMIT_TYPE_CD is not null and LOC_FACILITY_CD != 'APAC'
    ),
    last_journey as (
      select t.journey_id
      from hf_journey t
      inner join
      (SELECT person_id,MAX(journey_order) as journey_order
        FROM hf_journey
        GROUP BY person_id) a
      on a.person_id = t.person_id and a.journey_order = t.journey_order
    ),
    last_encounter as (
      select g.ENCNTR_ID from hf_encounter_journey as g
      left join last_journey as h
      on h.JOURNEY_ID = g.JOURNEY_ID
      where h.JOURNEY_ID is not null
    )

    select distinct(ENCNTR_ID),max(PERSON_ID) as PERSON_ID,
    min(BEG_EFFECTIVE_DT_TM) AS BEG_EFFECTIVE_DT_TM,max(DISCH_DT_TM) AS DISCH_DT_TM,max(UPDT_DT_TM) AS UPDT_DT_TM,
    min(HIS_ENCNTR) AS HIS_ENCNTR from join_tab
    where PERSON_ID in (
      select PX9_PERSON_ID FROM ENCOUNTER_REFERENCE WHERE
      Q4_DIAGNOSIS = 1) and  DISCH_DT_TM >= BEG_EFFECTIVE_DT_TM and ENCNTR_ID IN (
        SELECT ENCNTR_ID FROM last_encounter)
    group by encntr_id

    union all
    select distinct(ENCNTR_ID),max(PERSON_ID) as PERSON_ID,
    min(BEG_EFFECTIVE_DT_TM) AS BEG_EFFECTIVE_DT_TM,max(DISCH_DT_TM) AS DISCH_DT_TM,max(UPDT_DT_TM) AS UPDT_DT_TM,
    min(HIS_ENCNTR) AS HIS_ENCNTR from join_tab
    where PERSON_ID in (
      select PX9_PERSON_ID FROM ENCOUNTER_REFERENCE WHERE
      Q4_DIAGNOSIS = 1) and  DISCH_DT_TM >= BEG_EFFECTIVE_DT_TM AND ENCNTR_ID NOT IN (SELECT ENCNTR_ID FROM HF_ENCOUNTER_JOURNEY)
    group by encntr_id;")


  return(query)
}





