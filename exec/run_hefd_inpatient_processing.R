library(hefd)
library(magrittr)



setwd("D:/projects/hefd")


df <- execute_query(get_hf_encounter_query())

function_names <- c(
                "enc" = "process_inpatient",
                "echos" = "extract_echos",
                "patho" = "extract_patho_result",
                "his_diag" = "extract_hf_hist_diag",
                "disch_meds" = "extract_discharge_meds",
                "hfenrolment" = "extract_hfenrolment_form",
                'hfreferral' = "extract_hfreferral_form"
                )

hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")),"_inpatient_")
hf_file_suffix <- ".csv"

for(name in names(function_names)){

  filename <- paste0(hf_file_prefix,name,hf_file_suffix)

  full_path <- file.path(config::get("output_dir"),filename)


  new_df <- df %>% get(function_names[[name]])()

  write.csv(new_df,full_path,row.names = FALSE,na = "")
}




