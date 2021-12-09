library(hefd)
library(magrittr)



setwd("D:/projects/hefd")



function_names <- c(
  "form" = "process_hfenrolment_form",
  "summary" = "summarise_enrolment",
  "echo" = "extract_echos",
  "patho" = "extract_patho_result"
)



hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")),"_enrolment_")
hf_file_suffix <- ".csv"


name <- "form"
filename <- paste0(hf_file_prefix,name,hf_file_suffix)
full_path <- file.path(config::get("output_dir"),filename)
new_df <- get(function_names[[name]])()
write.csv(new_df,full_path,row.names = FALSE,na = "")

for(name in names(function_names)){
  filename <- paste0(hf_file_prefix,name,hf_file_suffix)
  full_path <- file.path(config::get("output_dir"),filename)

  if(name == "form"){
    next
  }
  else if(name == "echo"){
    result <- new_df %>% get(function_names[[name]])(df_dttm = "FORM_DT_TM")
  }
  else if(name == "patho"){
    result <- new_df %>% get(function_names[[name]])(use_default_person_list = FALSE,df_dttm="FORM_DT_TM")
  }
  else{
    result <- new_df %>% get(function_names[[name]])()
  }

  write.csv(result,full_path,row.names = FALSE,na = "")
}

#name <- "summary"


#name <- "summary"
#filename <- paste0(hf_file_prefix,name,hf_file_suffix)
#full_path <- file.path(config::get("output_dir"),filename)
#echo <- new_df %>% get(function_names[[name]])()
#write.csv(new_df,full_path,row.names = FALSE,na = "")

