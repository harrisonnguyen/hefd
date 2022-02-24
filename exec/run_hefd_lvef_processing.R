renv::load("D:/projects/hefd")

library(hefd)
library(magrittr)



setwd("D:/projects/hefd")


df <- execute_query(get_lvef_result_to_strip())



if (nrow(df)>0){
  temp <- df %>%
    extract_lvef_result()


  execute_write_to_db(temp,"STAGING_HF_LVEF_STRIP")

}
