renv::load("D:/projects/hefd")

library(hefd)
library(magrittr)



setwd("D:/projects/hefd")


df <- execute_query(get_hf_encounter_query())

function_names <- c(
                "enc" = "process_inpatient",
                "echos" = "extract_echos",
                "patho" = "extract_patho_result",
                #"his_diag" = "extract_hf_hist_diag",
                "disch_meds" = "extract_discharge_meds",
                "hfenrolment" = "extract_hfenrolment_form",
                'hfreferral' = "extract_hfreferral_form"
                )
function_names <- c(
  "STAGING_HF_INPATIENT_ENCNTR" = "process_inpatient",
  "STAGING_HF_INPATIENT_ECHO" = "extract_echos",
  "STAGING_HF_INPATIENT_PATHO" = "extract_patho_result",
  #"his_diag" = "extract_hf_hist_diag",
  "STAGING_HF_INPATIENT_DISCH_MED" = "extract_discharge_meds",
  "STAGING_HF_INPATIENT_ENROLMENT" = "extract_hfenrolment_form",
  'STAGING_HF_INPATIENT_REFERRAL' = "extract_hfreferral_form"
)


#hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")),"_inpatient_")
#hf_file_suffix <- ".csv"

for(name in names(function_names)){

  #filename <- paste0(hf_file_prefix,name,hf_file_suffix)

  #full_path <- file.path(config::get("output_dir"),filename)


  new_df <- df %>% get(function_names[[name]])()
  execute_write_to_db(new_df,name)

  #write.csv(new_df,full_path,row.names = FALSE,na = "")
}

