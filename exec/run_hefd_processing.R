library(hefd)
library(magrittr)



setwd("D:/projects/hefd")
hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")))

df <- process_homevisit_form() %>%
  dplyr::select(
    "ENCNTR_ID","PERSON_ID","FORM_DT_TM","PARENT_EVENT_ID","UPDT_DT_TM","MACARF_VISIT_TYPE","NYHA_CLASSIFICATION",
    "HEART_FAILURE_SYMPTOMS",
    "DEPENDENT_OEDEMA_HEARTFAILURESYMPTOMS",
    "FATIGUE_HEARTFAILURESYMPTOMS","LETHARGY_HEARTFAILURESYMPTOMS","ANKLE_OEDEMA_HEARTFAILURESYMPTOMS","ABDOMINAL_OEDEMA_HEARTFAILURESYMPTOMS","DYSPNOEA_HEARTFAILURESYMPTOMS",
    "LOWER_LEG_OEDEMA_HEARTFAILURESYMPTOMS","NAUSEA_HEARTFAILURESYMPTOMS"
  )


filename <- paste0(hf_file_prefix,"_homevisit_form.csv")
full_path <- file.path(config::get("output_dir"),filename)
write.csv(df,full_path,row.names = FALSE,na = "")

visit_cohort <- create_visit_journey(df)
filename <- paste0(hf_file_prefix,"_homevisit_cohort.csv")

full_path <- file.path(config::get("output_dir"),filename)
write.csv(visit_cohort,full_path,row.names = FALSE,na = "")
