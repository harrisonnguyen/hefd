renv::load("D:/projects/hefd")

library(hefd)
library(magrittr)



setwd("D:/projects/hefd")



function_names <- c(
  "STAGING_HF_ENROLMENT_FORM" = "process_hfenrolment_form",
  "STAGING_HF_ENROLMENT_SUMMARY" = "summarise_enrolment",
  "STAGING_HF_ENROLMENT_ECHO" = "extract_echos",
  "STAGING_HF_ENROLMENT_PATHO" = "extract_patho_result"
)



#hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")),"_enrolment_")
#hf_file_suffix <- ".csv"


name <- "STAGING_HF_ENROLMENT_FORM"
#filename <- paste0(hf_file_prefix,name,hf_file_suffix)
#full_path <- file.path(config::get("output_dir"),filename)
new_df <- get(function_names[[name]])()


execute_write_to_db(new_df,"STAGING_HF_ENROLMENT_FORM")
#write.csv(new_df,full_path,row.names = FALSE,na = "")

for(name in names(function_names)){
  #filename <- paste0(hf_file_prefix,name,hf_file_suffix)
  #full_path <- file.path(config::get("output_dir"),filename)

  if(name == "STAGING_HF_ENROLMENT_FORM"){
    next
  }
  else if(name == "STAGING_HF_ENROLMENT_ECHO"){
    result <- new_df %>% get(function_names[[name]])(df_dttm = "FORM_DT_TM")
  }
  else if(name == "STAGING_HF_ENROLMENT_PATHO"){
    result <- new_df %>% get(function_names[[name]])(use_default_person_list = FALSE,df_dttm="FORM_DT_TM")
  }
  else{
    result <- new_df %>% get(function_names[[name]])()
  }

  #write.csv(result,full_path,row.names = FALSE,na = "")
  execute_write_to_db(result,name)
}

#name <- "summary"


#name <- "summary"
#filename <- paste0(hf_file_prefix,name,hf_file_suffix)
#full_path <- file.path(config::get("output_dir"),filename)
#echo <- new_df %>% get(function_names[[name]])()
#write.csv(new_df,full_path,row.names = FALSE,na = "")

