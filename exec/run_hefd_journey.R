library(hefd)
library(magrittr)



setwd("D:/projects/hefd")
hf_file_prefix <- paste0("hf","_",tolower(format(Sys.Date(), "%Y-%b-%d")))

.dots <- rlang::exprs(
  person_key = PERSON_ID,
  encntr_key = ENCNTR_ID,
  journey_key = JOURNEY_ID,
  admit_dttm = BEG_EFFECTIVE_DT_TM,
  discharge_dttm = DISCH_DT_TM,
  journey_start = JOURNEY_START_DT_TM,
  journey_end = JOURNEY_END_DT_TM,
  journey_day = JOURNEY_DAYS,
  encntr_key_end = ENCNTR_ID_END
)


df <- execute_query(get_encounter_journey_query())
encounter_journey <- df %>%
  link_encounters_by_timestamps(.default_dots = .dots)

filename <- paste0(hf_file_prefix,"_encounter_journey.csv")
full_path <- file.path(config::get("output_dir"),filename)
write.csv(encounter_journey ,full_path,row.names = FALSE,na = "")

#%>%
#  dplyr::select(ENCNTR_ID,JOURNEY_ID,ENCNTR_ORDER,HIS_ENCNTR)

# journeys <- encounter_journey %>%
#   create_journey_table(.default_dots = .dots)
#
# filename <- paste0(hf_file_prefix,"_journey.csv")
# full_path <- file.path(config::get("output_dir"),filename)
# write.csv(journeys,full_path,row.names = FALSE,na = "")

