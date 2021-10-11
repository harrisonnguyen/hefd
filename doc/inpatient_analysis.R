## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE, message=FALSE
)
knitr::opts_knit$set(
  root.dir = here::here()
)

## ----import-------------------------------------------------------------------

library(magrittr)
library(targets)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(acs)
library(flextable)

library(hefd)

## -----------------------------------------------------------------------------

proportion_bar_plot <- function(df,column,title,n_suffix = NULL,vjust=-0.25){
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  plot<- df %>%
     ggplot2::ggplot(., aes(x=!!rlang::sym(column),fill=!!rlang::sym(column))) +
    ggplot2::geom_bar(aes(y = ..count..),stat="count",alpha=0.5,show.legend = FALSE) +
    ggplot2::xlab(title) + 
    ggplot2::ylab("Count") + 
    ggtitle(title) +
    geom_text(
       aes(label=paste(round((..count..)/sum(..count..)*100,0),"%",sep=""),y= ..count..),
       stat='count',
       vjust=vjust
   ) + 
  theme_minimal()
  
  
  return(plot)
}

conditional_bar_plot <- function(df,var1,var2,title,n_suffix = NULL){
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  var1<- rlang::sym(var1)
  var2<- rlang::sym(var2)
  plot <- df%>%
  dplyr::count(!!var1,!!var2) %>%
  dplyr::group_by(!!var1) %>%
  dplyr::mutate(prop = prop.table(n)*100) %>%
  ggplot2::ggplot(., aes(x=!!var1,y=n,fill=!!var2)) + 
  geom_bar(stat="identity",position=position_dodge(0.7),width=0.7,alpha=0.5) +
  geom_text(aes(label= paste(round(prop,0),"%",sep=""),y=n),position=position_dodge(0.7),vjust=-0.5) +
  labs(y="Count",x=var1, title = title) +
  scale_fill_discrete(name = var2) + 
  theme_minimal()
  
  return(plot)
}

plot_histogram <- function(df,column, title,threshold = NULL,n_suffix=NULL){
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  var1<- rlang::sym(column)

  df %<>% dplyr::filter(!is.na(!!var1))
  
  
  plot <- df %>%
  ggplot2::ggplot(., aes(x=!!var1,fill=!!var1)) + 
   geom_histogram(fill="lightblue",alpha=0.5) +
  labs(y="Count",x=var1, title = title)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) +
  theme_minimal()
    

  
  if(!is.null(threshold)){
    plot <- plot + geom_vline(aes(xintercept=threshold),
            color="red", linetype="dashed", size=1)
  }
  
  return(plot)
}

plot_histogram_group <- function(df,var1,var2,title,n_suffix){
  
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  var1<- rlang::sym(var1)
  var2<- rlang::sym(var2)

  df %<>% dplyr::filter(!is.na(!!var1)) %>%
    dplyr::filter(!is.na(!!var2))
  
  
  plot <- df %>%
  ggplot2::ggplot(., aes(x=!!var1,fill=!!var2,color=!!var2)) + 
   geom_histogram(aes(y=..density..),alpha=0.5, position="identity") +
    geom_density(alpha=0.3)+
  labs(y="Count",x=var1, title = title)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_minimal()
    
  
  return(plot)
}

plot_logical_columns <- function(df,var1,title,n_suffix=NULL){
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  var1<- rlang::sym(var1)
  
  plot_data <- df %>% 
    dplyr::summarise_if(is.logical,sum) %>%
   tidyr::pivot_longer(everything(),values_to = "N",names_to = rlang::as_string(var1)) %>%
  dplyr::mutate(prop = N/nrow(df)*100)

  plot <- plot_data %>%
       ggplot2::ggplot(., aes(x=!!var1,y=N,fill=!!var1)) +
      ggplot2::geom_bar(position="dodge", stat="identity",show.legend = FALSE) +
    geom_text(aes(label= paste(round(prop,0),"%",sep=""),y=N),vjust=-0.5) + 
    labs(y="Count",x=var1, title = title)
  
  return(plot)
}


## -----------------------------------------------------------------------------
plot_pie_chart <- function(df,var1,title,n_suffix = NULL){
  if(is.null(n_suffix)){
    title <- paste0(title, " (N = ",nrow(df), ")") 
  }
  else{
    title <- paste0(title, " (N = ",nrow(df), " ",n_suffix, ")")
  }
  
  var1<- rlang::sym(var1)
  df %<>% 
    filter(!is.na(!!var1)) %>%
    group_by(!!var1) %>% # Variable to be transformed
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  plot <- ggplot(df, aes(x = "", y = n, fill = !!var1)) +
    geom_col() +
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y")
    labs(y="Count",x=var1, title = title)
    
  return(plot)
}


## ----data-load----------------------------------------------------------------
encounter_ref <- load_data("encounter_reference")
encounter <- load_data("encounter")
diagnosis <- load_data("diagnosis")
diagnosis_history <- load_data("diagnosis_history")
problem <- load_data("problem")
orders <- load_data("orders")
patient <- load_data("patient")



## -----------------------------------------------------------------------------
query <- glue::glue("SELECT * from MEDICATION_ORDERS as x \\
            WHERE ENCNTR_ID in (SELECT PX9_ENCNTR_ID from ENCOUNTER_REFERENCE \\
             WHERE Q4_DIAGNOSIS = 1 and PROCESS_FLAG = 1 and HISTORY_FLAG = 0) and (ORIG_ORD_AS_FLAG = 1 OR ORIG_ORD_AS_FLAG=2)")
medication_order <- execute_query(query)

hf_form <- execute_query("SELECT * from DCP_FORMS_ACTIVITY where DESCRIPTION = 'Heart Failure (MACARF) Referral Form' or DESCRIPTION = 'Heart Failure - Management of Cardiac Function Enrolment'")

## -----------------------------------------------------------------------------
location_string <- "Royal North Shore"
hf_diag <- diagnosis %>%
  dplyr::filter(stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") & DIAG_TYPE_CD == "Final")

qual_enc <- encounter %>%
  dplyr::filter(ENCNTR_ID %in% hf_diag$ENCNTR_ID & LOC_FACILITY_CD == location_string)

cohort <- qual_enc %>%
  dplyr::distinct(PERSON_ID)

time_period <- round(difftime(max(qual_enc$DISCH_DT_TM,na.rm = TRUE), min(qual_enc$DISCH_DT_TM,na.rm = TRUE),units="days")/362.2425,1)

## ----dist-enc-plot,fig.cap="Distribution of the number of encounters for each patient."----
counts <- qual_enc %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::summarise(N = dplyr::n())

proportion_bar_plot(counts,"N", "Number of encounters per patient",n_suffix="patients") + xlab("")

## ----encntr-type-plot, fig.cap="Distribution of Encounter Type."--------------
proportion_bar_plot(qual_enc,"ENCNTR_TYPE_CD","Distribution of Encounter Type",n_suffix = "encounters") + xlab("")

## -----------------------------------------------------------------------------
extract_hf_diagnosis <- function(df,group_cols = "ENCNTR_ID"){
  
  group_cols <- rlang::sym(group_cols)
  df %<>%
  dplyr::group_by(!!group_cols) %>%
  dplyr::summarise(
    `Congestive heart failure` = any(SOURCE_IDENTIFIER == "I50.0" | SOURCE_IDENTIFIER == "I50"),
    `Heart failure, unspecified` = any(SOURCE_IDENTIFIER == "I50.9"),
    `Left ventricular failure` = any(SOURCE_IDENTIFIER == "I50.1")
  ) %>%
  replace(is.na(.), FALSE)
  
  return(df)
}

## -----------------------------------------------------------------------------

hf_diag_primary <- hf_diag %>%
  dplyr::filter(DIAG_PRIORITY == 1 & ENCNTR_ID %in% qual_enc$ENCNTR_ID)
  
hf_diag_secondary <- hf_diag %>%
  dplyr::filter(DIAG_PRIORITY != 1 & ENCNTR_ID %in% qual_enc$ENCNTR_ID)
  

## -----------------------------------------------------------------------------
primary_temp <- hf_diag_primary %>%
  extract_hf_diagnosis() %>%
  dplyr::mutate(`Primary HF Diagnosis` = TRUE)

secondary_temp <- hf_diag_secondary %>%
  extract_hf_diagnosis() %>%
  dplyr::mutate(`Secondary HF Diagnosis` = TRUE)

combined_temp <- hf_diag %>%
  dplyr::filter(ENCNTR_ID %in% qual_enc$ENCNTR_ID) %>%
  extract_hf_diagnosis() %>%
  dplyr::mutate(`Prim. & Sec. HF Diagnosis` = TRUE)

## -----------------------------------------------------------------------------

hf_diag_primary_count <- hf_diag_primary %>%
  dplyr::group_by(ENCNTR_ID) %>%
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::filter(N > 1)

hf_diag_secondary_count <- hf_diag_secondary %>%
  dplyr::group_by(ENCNTR_ID) %>%
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::filter(N > 1)

## ----icd10-dist-plot, fig.cap="Distribution of primary and secondary ICD10 diagnosis."----

p1 <- plot_logical_columns(primary_temp,"ICD10","Primary HF ICD10",n_suffix="patients") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
  ggplot2::ylim(0,2000)

p2 <- plot_logical_columns(secondary_temp,"ICD10","Secondary HF ICD10",n_suffix="patients") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ 
  ggplot2::ylim(0,3500)

p3 <- plot_logical_columns(combined_temp,"ICD10","Combined Primary & Secondary HF ICD10",n_suffix="patients") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ 
  ggplot2::ylim(0,5000)

ggpubr::ggarrange(p1,p2,p3,labels= c("A","B","C"), ncol = 1, nrow = 3)

## -----------------------------------------------------------------------------
prim_sec_merge <- primary_temp %>%
  dplyr::inner_join(secondary_temp,by="ENCNTR_ID",suffix = c("_PRIMARY","_SECONDARY")) %>%
  dplyr::mutate(PRIMARY_DIAG = 
                  dplyr::case_when(
                    `Congestive heart failure_PRIMARY` ~ "Congestive heart failure",
                    `Left ventricular failure_PRIMARY` ~ "Left ventricular failure",
                    `Heart failure, unspecified_PRIMARY` ~ "Heart failure, unspecified",
                  ),
                SECONDARY_DIAG = 
                  dplyr::case_when(
                    `Congestive heart failure_SECONDARY` ~ "Congestive heart failure",
                    `Left ventricular failure_SECONDARY` ~ "Left ventricular failure",
                    `Heart failure, unspecified_SECONDARY` ~ "Heart failure, unspecified",
                  )
                )

## -----------------------------------------------------------------------------
diag_tab <- prop.table(table(prim_sec_merge$PRIMARY_DIAG,prim_sec_merge$SECONDARY_DIAG))

## ----primary-sec-hf-diag------------------------------------------------------
tab_caption <- paste0("Contingency table of Primary and Secondary HF Diagnosis (N = ",
                      nrow(prim_sec_merge),"/",nrow(combined_temp),")")
diag_tab %>% 
  knitr::kable(caption = tab_caption,digits=2) %>%
  kableExtra::add_header_above(c("Primary ICD10"=1,"Secondary ICD10"=3))%>%
  kable_styling(position = "center",full_width = FALSE)

## -----------------------------------------------------------------------------

only_secondary <- secondary_temp %>%
  dplyr::filter(!ENCNTR_ID %in% primary_temp$ENCNTR_ID)
n_only_prim <-  nrow(only_secondary)

## -----------------------------------------------------------------------------
primary_diag_of_secondary <- diagnosis %>%
  dplyr::filter(!stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") & 
                  ENCNTR_ID %in% only_secondary$ENCNTR_ID &
                  DIAG_PRIORITY == 1 &
                  SOURCE_VOCABULARY_CD == "ICD10-AM") 

tab_cap <- paste(
  "Top 15 primary diagnosis for those with only a secondary heart failure diagnosis (N =",
  n_only_prim,
  "encounters).",
  sep=" "
)

## ----prim-sec-tab-------------------------------------------------------------
primary_diag_of_secondary %>%
  dplyr::group_by(SOURCE_STRING) %>%
  dplyr::summarise(N = dplyr::n_distinct(ENCNTR_ID)) %>% 
  dplyr::mutate(prop = round(N/dplyr::n_distinct(primary_diag_of_secondary$PERSON_ID)*100,1)) %>%
  dplyr::arrange(desc(N)) %>% 
  dplyr::top_n(15) %>%
  knitr::kable(caption = tab_cap)


## -----------------------------------------------------------------------------
primary_cohort <- cohort %>%
  dplyr::filter(PERSON_ID %in% hf_diag_primary$PERSON_ID) %>%
  dplyr::mutate(HAS_PRIMARY_HF = TRUE)


AGE_breaks <- c(-Inf,18, 25, 45, 65, 80, Inf)
  AGE_labels <- c("<18", "19-24", "25-44", "45-64", "65-80", "80+")
  
primary_enc <- encounter %>%
  dplyr::filter(ENCNTR_ID %in% hf_diag_primary$ENCNTR_ID) %>%
  dplyr::left_join(patient, by = "PERSON_ID") %>%
  dplyr::mutate(AGE = as.integer(difftime(ACTIVE_STATUS_DT_TM,BIRTH_DT_TM, units="weeks")/52.25)) %>%
  dplyr::mutate(AGE_BINNED = cut(AGE, breaks = AGE_breaks, labels = AGE_labels,
                                   right = FALSE)) %>%
    dplyr::mutate(LOS = difftime(DISCH_DT_TM,ARRIVE_DT_TM,units="days")) %>%
  dplyr::left_join(primary_temp, by="ENCNTR_ID") %>%
  dplyr::mutate(PRIMARY_DIAG = 
                  dplyr::case_when(
                    `Congestive heart failure` ~ "Congestive heart failure",
                    `Left ventricular failure` ~ "Left ventricular failure",
                    `Heart failure, unspecified` ~ "Heart failure, unspecified",
                  ))



## -----------------------------------------------------------------------------
primary_demo <- primary_enc %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::arrange(AGE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

total<- bind_rows(primary_demo, mutate(primary_demo, SEX_CD = "Total")) %>%
  group_by(SEX_CD) %>%
  summarise(
    N = n(),
    Age = round(mean(AGE)),
  ) %>%
  pivot_wider(names_from = "SEX_CD", values_from = c("N", "Age")) %>%
  mutate(p_Female = round(N_Female/N_Total*100, 1),
         p_Male = round(N_Male/N_Total*100, 1)) %>%
  select(
         Females = N_Female, p_Female,
         Males = N_Male, p_Male,
         N_Total,
         Age_Female,
         Age_Male,
         Age_Total) %>%
  flextable() %>%
  set_header_labels(Group = "", p_Female = "%", p_Male = "%", N_Total = "n",
                    Age_Female = "Age (F)", Age_Male = "Age (M)", Age_Total = "Mean Age")
total

## ----age-sex-plot, fig.cap="Distribution of Sex conditioned on Age."----------
conditional_bar_plot(primary_demo,"AGE_BINNED","SEX_CD", "Age and Sex",n_suffix="patients") +
  scale_fill_discrete(name = "SEX") + 
  xlab("AGE")

## ----admit-disch-mode-plot,fig.cap="Admission and Discharge mode of encounters with primary HF diagnosis."----

p1 <- proportion_bar_plot(primary_enc,"ADMIT_MODE_CD","Mode of Admission",n_suffix="encounters",vjust=0.25) + ylim(0,800) +xlab("")  + coord_flip()

p2 <- proportion_bar_plot(primary_enc,"DISCH_DISPOSITION_CD","Mode of Separation",n_suffix="encounters",vjust=0.25)  + ylim(0,1100) +xlab("")  + coord_flip()


ggpubr::ggarrange(p1,p2,nrow=2,ncol = 1, labels=c("A","B"))

## ----los-dis-plot, fig.cap="Distribution of Length of Stay"-------------------

p1 <- plot_histogram(primary_enc,"LOS","Distribution of LOS",mean(primary_enc$LOS,na.rm=TRUE),n_suffix = "encounters") + xlab("LOS (days)") + xlim(0,40) + ylim(0,250)

p2 <- plot_histogram_group(primary_enc,"LOS","PRIMARY_DIAG","Distribution of LOS by Diagnosis",n_suffix = "encounters") + xlab("LOS (days)") + xlim(0,40)

ggpubr::ggarrange(p1,p2,nrow=2,ncol = 1, labels=c("A","B"))

## ----los-stat-tab-------------------------------------------------------------
primary_enc %>%
  dplyr::bind_rows(mutate(primary_enc, PRIMARY_DIAG = "Total")) %>%
  dplyr::filter(!is.na(LOS)) %>%
  dplyr::group_by(PRIMARY_DIAG) %>%
  dplyr::summarise(MEAN = as.numeric(round(mean(LOS),1)),
                   MAX = as.numeric(round(max(LOS),1)),
                   MIN = as.numeric(round(min(LOS),1)),
                   MEDIAN = as.numeric(round(median(LOS),1)),
                   STD = as.numeric(round(sd(LOS),1)),
                   N = dplyr::n()) %>%
  flextable() %>%
  set_caption("Statistics of LOS by HF diagnosis.")

## ----clinical-app-demo, fig.cap="Distribution of ICD10 heart failure encounters using the `Admitted Patient CaseMix App."----
# All defaults
knitr::include_graphics(file.path(here::here(),"vignettes/img/inpatient_hf_casemix.PNG"))

## ----acd-tab------------------------------------------------------------------
known_to_hf <- problem %>%
  dplyr::filter(stringr::str_detect(SOURCE_STRING,"(?i)Known to heart failure")) %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::arrange(BEG_EFFECTIVE_DT_TM) %>%
  dplyr::slice(1) %>%
  dplyr::select(PERSON_ID,BEG_EFFECTIVE_DT_TM) %>%
  dplyr::mutate(HAS_ALERT = TRUE)

kk<- primary_cohort %>%
  merge(.,select(known_to_hf,-BEG_EFFECTIVE_DT_TM) , by="PERSON_ID",all=TRUE) %>%
  replace(is.na(.), FALSE)

prop_tab <- prop.table(table(kk$HAS_PRIMARY_HF,kk$HAS_ALERT))
prop_tab %>% 
  knitr::kable(caption = "Contigency table of Primary HF ICD10 and Advanced Care directive (ACD).",digits=2) %>%
  kableExtra::add_header_above(c("Has Primary\n ICD10"=1,"Has ACD"=2))%>%
  kable_styling(position = "center",full_width = FALSE)


## ----alert-delta-plot,fig.cap="Distribution of time taken to register Advanced Care Directive since admission for the encounter."----
primary_enc_alert <- primary_enc %>%
  dplyr::left_join(known_to_hf,by="PERSON_ID") %>%
  dplyr::mutate(ALERT_DELTA = difftime(BEG_EFFECTIVE_DT_TM,ARRIVE_DT_TM,units="days")) %>%
  dplyr::filter(!is.na(ALERT_DELTA))

plot_histogram(
  primary_enc_alert,
  "ALERT_DELTA","Time taken for ACD to be registered since arrival",
  median(primary_enc_alert$ALERT_DELTA,na.rm=TRUE),n_suffix = "encounters") + xlim(-200,200) + xlab("Time (days)")

## -----------------------------------------------------------------------------
n_1_alert <- primary_enc_alert %>%
  dplyr::filter(ALERT_DELTA < 1) %>%
  nrow()

n_3_alert <- primary_enc_alert %>%
  dplyr::filter(ALERT_DELTA < 3) %>%
  nrow()

n_alert <- primary_enc_alert %>%
  dplyr::filter(!is.na(HAS_ALERT)) %>%
  nrow()

median <- as.numeric(median(primary_enc_alert$ALERT_DELTA,na.rm=TRUE))

## ----his-diag-plot, fig.cap="Distribution of historical diagnosis."-----------
his_hf_diag <- diagnosis_history %>%
  dplyr::filter(stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") &
                  PERSON_ID %in% primary_cohort$PERSON_ID)

summary_his_diag <- his_hf_diag %>%
  extract_hf_diagnosis(group_cols = "PERSON_ID") %>%
  dplyr::right_join(primary_cohort,by="PERSON_ID") %>%
  dplyr::select(-HAS_PRIMARY_HF) %>%
  replace(is.na(.),FALSE) %>%
    dplyr::mutate(`No His. HF ICD10` = !`Congestive heart failure` & !`Heart failure, unspecified`
                  & !`Left ventricular failure`)
plot_logical_columns(summary_his_diag,"ICD10","Distribution of Historical ICD10",n_suffix="patients")

## -----------------------------------------------------------------------------
extract_form <- function(df,string,prefix){
  df %<>%
  dplyr::filter(DESCRIPTION == string) %>%
  dplyr::mutate(HAS_FORM = TRUE) %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::arrange(desc(FORM_DT_TM)) %>%
  dplyr::slice(1) %>%
  dplyr::select(PERSON_ID,ENCNTR_ID,HAS_FORM,PARENT_ENTITY_ID,FORM_DT_TM) %>%
    dplyr::rename_with(.fn = ~ paste0(prefix,"_",.x))
  
  return(df)
}
  

## ----form-plot,fig.cap = "Proportion of Patients with an enrolment or referral form."----




form_primary_cohort <- primary_cohort %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure - Management of Cardiac Function Enrolment',"ENROLMENT"), by = c("PERSON_ID" = "ENROLMENT_PERSON_ID")
  ) %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure (MACARF) Referral Form',"REFERRAL"), by = c("PERSON_ID" = "REFERRAL_PERSON_ID")
  ) %>%
  tidyr::replace_na(list(ENROLMENT_HAS_FORM = FALSE, REFERRAL_HAS_FORM = FALSE)) %>%
  dplyr::mutate(NO_FORM = !ENROLMENT_HAS_FORM & !REFERRAL_HAS_FORM)

form_primary_cohort %>%
  dplyr::select(-HAS_PRIMARY_HF) %>%
  plot_logical_columns("Form","Proportion with HF Form") +
 labs(x="")

## ----no-form-disch-plot,fig.cap="Distribution of mode of discharge for patients without either a referral nor enrolment form."----
form_primary_cohort %>%
  dplyr::filter(NO_FORM) %>%
  dplyr::left_join(primary_enc,by="PERSON_ID") %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::summarise(
    DISCH_MODE = dplyr::case_when(
      any(DISCH_DISPOSITION_CD == 'Pt Death without Autopsy') ~ 'Death without Autopsy',
      any(DISCH_DISPOSITION_CD == 'Discharge Own Risk') ~ 'Discharge Own Risk',
      any(stringr::str_detect(DISCH_DISPOSITION_CD, "(?i)transfer")) ~ 'Transfer',
      any(DISCH_DISPOSITION_CD == "Discharge by Hospital") ~ "Discharge by Hospital",
    )
  ) %>%
  proportion_bar_plot("DISCH_MODE","Discharge mode for patients without forms",n_suffix="patients") + xlab("")


## -----------------------------------------------------------------------------

echos <- orders %>%
  dplyr::filter(stringr::str_detect(CATALOG_CD,"(?i)cardio - echo") & ENCNTR_ID %in% primary_enc$ENCNTR_ID) %>%
  dplyr::group_by(ENCNTR_ID) %>%
  dplyr::arrange(desc(STATUS_DT_TM)) %>%
  dplyr::slice(1) %>%
  dplyr::select(ENCNTR_ID,CATALOG_CD,STATUS_DT_TM) %>%
  dplyr::right_join(
    dplyr::select(primary_enc,ENCNTR_ID,ARRIVE_DT_TM),by="ENCNTR_ID") %>%
  dplyr::mutate(HAS_ECHO = !is.na(CATALOG_CD),ECHO_DELTA = difftime(STATUS_DT_TM,ARRIVE_DT_TM,units = "days")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-CATALOG_CD) %>%
  dplyr::rename(ECHO_START_DT_TM = STATUS_DT_TM)


## -----------------------------------------------------------------------------

extract_path_result <- function(df,string,prefix,breaks=NULL,labels=NULL){
  df %<>% dplyr::filter(EVENT_CD == string) %>%
  dplyr::select(ENCNTR_ID,EVENT_START_DT_TM,RESULT_VAL,RESULT_UNITS_CD) %>%
    dplyr::mutate(RESULT_VAL = as.numeric(stringr::str_remove_all(RESULT_VAL, '\"')) )  %>% 
  dplyr::rename("{prefix}_EVENT_START_DT_TM" := EVENT_START_DT_TM,
                
                "{prefix}_RESULT_UNITS_CD" := RESULT_UNITS_CD)
  
  if(!is.null(breaks)){
    df %<>% dplyr::mutate("{prefix}_BIN" := cut(RESULT_VAL,breaks=breaks,labels=labels))
  }
  df %<>% dplyr::rename("{prefix}_RESULT_VAL" := RESULT_VAL)
  
  return(df)
}

## -----------------------------------------------------------------------------
path_tests <- execute_query("SELECT * from PATHOLOGY_EVENT where CATALOG_CD = 'Brain Natriuretic Peptide' or CATALOG_CD = 'Iron Level'")

path_tests %<>%
  dplyr::filter(ENCNTR_ID %in% primary_enc$ENCNTR_ID) %>%
  dplyr::group_by(ENCNTR_ID,EVENT_CD) %>%
  dplyr::arrange(desc(EVENT_START_DT_TM)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()




## ----order-prop-plot, fig.cap="Number and proportion of Encounters with a relevant heart failure order, echocardiogram, brain natriuretic peptide test, serum iron test and transferrin saturation test."----
hf_order<-echos  %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Brain Natriuretic peptide (BNP )", "BNP", c(-Inf,450,900,Inf),c("< 450","450-900","900+")), by="ENCNTR_ID"
  ) %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Iron", "IRON",c(-Inf,9,30.4,Inf),c("< 9","9-30.4","30.4+")), by="ENCNTR_ID"
  ) %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Transferrin Saturation", "TRANSFERRIN_SAT",c(-Inf,20,Inf),c("< 20","20+")), by="ENCNTR_ID"
  ) %>%
  dplyr::mutate(HAS_BNP = !is.na(BNP_EVENT_START_DT_TM),
                HAS_IRON = !is.na(IRON_EVENT_START_DT_TM),
                HAS_TRANSFERRIN_SAT = !is.na(TRANSFERRIN_SAT_EVENT_START_DT_TM))

plot_logical_columns(hf_order,"ORDERS","Proportion of Encounters with Order",n_suffix="encounters")



## ----echo-time-plot,fig.cap="Distribution of the time taken for an echocardiogram."----

mean_delta <- median(hf_order$ECHO_DELTA,na.rm = TRUE)
plot_histogram(hf_order %>% dplyr::filter(ECHO_DELTA < 75),
               "ECHO_DELTA","Distribution of time taken for Echo",
               mean_delta,n_suffix="encounters") + 
    xlab("Time take for echo since admission (days)")

## -----------------------------------------------------------------------------
primary_meds <- medication_order %>%
  dplyr::filter(ENCNTR_ID %in% primary_enc$ENCNTR_ID)
primary_enc_meds <- primary_enc %>%
  dplyr::mutate(HAS_DISCH_MED = ENCNTR_ID %in% primary_meds$ENCNTR_ID)


## ----med-type-plot, fig.cap="The type of medication whether home or discharge medication."----
p1 <- proportion_bar_plot(primary_meds,"ORIG_ORD_AS_FLAG","Type of discharge medication\n")
p2 <- proportion_bar_plot(primary_enc_meds,"HAS_DISCH_MED","Encounters with Discharge Medications\n")

ggpubr::ggarrange(p1,p2,ncol=2,nrow=1,labels=c("A","B"))

## ----med-class-tab------------------------------------------------------------
primary_meds %>%
  dplyr::group_by(MEDICATION_CLASS) %>%
  dplyr::summarise(N = dplyr::n_distinct(PERSON_ID)) %>% 
  dplyr::mutate(prop = round(N/dplyr::n_distinct(primary_meds$PERSON_ID)*100,1)) %>%
  dplyr::arrange(desc(N)) %>% 
  dplyr::top_n(15) %>%
  knitr::kable(caption = "Top 15 discharge medication class.")

## ----path-test-plot,fig.cap="Results of pathology tests."---------------------


p1<-plot_pie_chart(hf_order,"BNP_BIN","BNP Results by Group")
p2<-plot_pie_chart(hf_order,"IRON_BIN","BNP Results by Group")
p3<-plot_pie_chart(hf_order,"TRANSFERRIN_SAT_BIN","BNP Results by Group")

#ggpubr::ggarrange(p1,p2,p3,nrow=3,ncol=1,labels=c("A","B","C"))


p1
p2
p3

## -----------------------------------------------------------------------------
primary_cohort <- cohort %>%
  dplyr::filter(PERSON_ID %in% qual_enc$PERSON_ID) %>%
  dplyr::mutate(HAS_PRIMARY_HF = TRUE)


AGE_breaks <- c(-Inf,18, 25, 45, 65, 80, Inf)
  AGE_labels <- c("<18", "19-24", "25-44", "45-64", "65-80", "80+")
  
primary_enc <- qual_enc %>%
  dplyr::left_join(patient, by = "PERSON_ID") %>%
  dplyr::mutate(AGE = as.integer(difftime(ACTIVE_STATUS_DT_TM,BIRTH_DT_TM, units="weeks")/52.25)) %>%
  dplyr::mutate(AGE_BINNED = cut(AGE, breaks = AGE_breaks, labels = AGE_labels,
                                   right = FALSE)) %>%
    dplyr::mutate(LOS = difftime(DISCH_DT_TM,ARRIVE_DT_TM,units="days"))

## -----------------------------------------------------------------------------
total<- bind_rows(primary_enc, mutate(primary_enc, SEX_CD = "Total")) %>%
  group_by(SEX_CD) %>%
  summarise(
    N = n(),
    Age = round(mean(AGE)),
  ) %>%
  pivot_wider(names_from = "SEX_CD", values_from = c("N", "Age")) %>%
  mutate(p_Female = round(N_Female/N_Total*100, 1),
         p_Male = round(N_Male/N_Total*100, 1)) %>%
  select(
         Females = N_Female, p_Female,
         Males = N_Male, p_Male,
         N_Total,
         Age_Female,
         Age_Male,
         Age_Total) %>%
  flextable() %>%
  set_header_labels(Group = "", p_Female = "%", p_Male = "%", N_Total = "n",
                    Age_Female = "Age (F)", Age_Male = "Age (M)", Age_Total = "Mean Age")
total

## ----age-sex-plot-sec, fig.cap="Distribution of Sex conditioned on Age."------
conditional_bar_plot(primary_enc,"AGE_BINNED","SEX_CD", "Age and Sex") +
  scale_fill_discrete(name = "SEX") + 
  xlab("AGE")

## ----los-dis-plot-sec, fig.cap="Distribution of Length of Stay"---------------

plot_histogram(primary_enc,"LOS","Distribution of LOS",mean(primary_enc$LOS,na.rm=TRUE)) + xlab("LOS (days)") + xlim(0,50)

## ----encntr-type-plot-sec, fig.cap="Distribution of Encounter Type."----------
proportion_bar_plot(primary_enc,"ENCNTR_TYPE_CD","Distribution of Encounter Type.")

## ----acd-tab-sec--------------------------------------------------------------
known_to_hf <- problem %>%
  dplyr::filter(stringr::str_detect(SOURCE_STRING,"(?i)Known to heart failure")) %>%
  dplyr::select(PERSON_ID) %>%
  dplyr::mutate(HAS_ALERT = TRUE)

kk<- primary_cohort %>%
  merge(.,known_to_hf , by="PERSON_ID",all=TRUE) %>%
  replace(is.na(.), FALSE)

prop_tab <- prop.table(table(kk$HAS_PRIMARY_HF,kk$HAS_ALERT))
prop_tab %>% 
  knitr::kable(caption = "Contigency table of Primary HF ICD10 and Advanced Care directive (ACD).",digits=2) %>%
  kableExtra::add_header_above(c("Has Primary\n ICD10"=1,"Has ACD"=2))%>%
  kable_styling(position = "center",full_width = FALSE)


## ----his-diag-plot-sec, fig.cap="Distribution of historical diagnosis."-------
his_hf_diag <- diagnosis_history %>%
  dplyr::filter(stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") &
                  PERSON_ID %in% primary_cohort$PERSON_ID)

summary_his_diag <- his_hf_diag %>%
  extract_hf_diagnosis(group_cols = "PERSON_ID") %>%
  dplyr::right_join(primary_cohort,by="PERSON_ID") %>%
  dplyr::select(-HAS_PRIMARY_HF) %>%
  replace(is.na(.),FALSE) %>%
    dplyr::mutate(`No His. HF ICD10` = !`Congestive heart failure` & !`Heart failure, unspecified`
                  & !`Left ventricular failure`)
plot_logical_columns(summary_his_diag,"ICD10","Distribution of Historical ICD10")

## -----------------------------------------------------------------------------

echos <- orders %>%
  dplyr::filter(stringr::str_detect(CATALOG_CD,"(?i)cardio - echo") & ENCNTR_ID %in% primary_enc$ENCNTR_ID) %>%
  dplyr::group_by(ENCNTR_ID) %>%
  dplyr::arrange(desc(STATUS_DT_TM)) %>%
  dplyr::slice(1) %>%
  dplyr::select(ENCNTR_ID,CATALOG_CD,STATUS_DT_TM) %>%
  dplyr::right_join(
    dplyr::select(primary_enc,ENCNTR_ID,ARRIVE_DT_TM),by="ENCNTR_ID") %>%
  dplyr::mutate(HAS_ECHO = !is.na(CATALOG_CD),ECHO_DELTA = difftime(STATUS_DT_TM,ARRIVE_DT_TM,units = "days")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-CATALOG_CD) %>%
  dplyr::rename(ECHO_START_DT_TM = STATUS_DT_TM)


## -----------------------------------------------------------------------------
path_tests <- execute_query("SELECT * from PATHOLOGY_EVENT where CATALOG_CD = 'Brain Natriuretic Peptide' or CATALOG_CD = 'Iron Level'")

path_tests %<>%
  dplyr::filter(ENCNTR_ID %in% primary_enc$ENCNTR_ID) %>%
  dplyr::group_by(ENCNTR_ID,EVENT_CD) %>%
  dplyr::arrange(desc(EVENT_START_DT_TM)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()




## ----order-prop-plot-sec, fig.cap="Number and proportion of Encounters with a relevant heart failure order, echocardiogram, brain natriuretic peptide test, serum iron test and transferrin saturation test."----
hf_order<-echos  %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Brain Natriuretic peptide (BNP )", "BNP", c(-Inf,450,900,Inf),c("< 450","450-900","900+")), by="ENCNTR_ID"
  ) %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Iron", "IRON",c(-Inf,9,30.4,Inf),c("< 9","9-30.4","30.4+")), by="ENCNTR_ID"
  ) %>%
  dplyr::left_join(
    extract_path_result(path_tests,"Transferrin Saturation", "TRANSFERRIN_SAT",c(-Inf,20,Inf),c("< 20","20+")), by="ENCNTR_ID"
  ) %>%
  dplyr::mutate(HAS_BNP = !is.na(BNP_EVENT_START_DT_TM),
                HAS_IRON = !is.na(IRON_EVENT_START_DT_TM),
                HAS_TRANSFERRIN_SAT = !is.na(TRANSFERRIN_SAT_EVENT_START_DT_TM))

plot_logical_columns(hf_order,"ORDERS","Proportion of Encounter with Order")



## ----echo-time-plot-sec,fig.cap="Distribution of the time taken for an echocardiogram."----

mean_delta <- mean(hf_order$ECHO_DELTA,na.rm = TRUE)
plot_histogram(hf_order %>% dplyr::filter(ECHO_DELTA < 75),
               "ECHO_DELTA","Distribution of time taken for Echo",
               mean_delta) + 
    xlab("Time take for echo since admission (days)")

## ----path-test-plot-sec,fig.cap="Results of pathology tests."-----------------


p1<-plot_pie_chart(hf_order,"BNP_BIN","BNP Results by Group")
p2<-plot_pie_chart(hf_order,"IRON_BIN","BNP Results by Group")
p3<-plot_pie_chart(hf_order,"TRANSFERRIN_SAT_BIN","BNP Results by Group")

#ggpubr::ggarrange(p1,p2,p3,nrow=3,ncol=1,labels=c("A","B","C"))


p1
p2
p3

## ----form-plot-sec,fig.cap = "Proportion of Patients with an enrolment or referral form."----


form_primary_cohort <- primary_cohort %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure - Management of Cardiac Function Enrolment',"ENROLMENT"), by = c("PERSON_ID" = "ENROLMENT_PERSON_ID")
  ) %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure (MACARF) Referral Form',"REFERRAL"), by = c("PERSON_ID" = "REFERRAL_PERSON_ID")
  ) %>%
  tidyr::replace_na(list(ENROLMENT_HAS_FORM = FALSE, REFERRAL_HAS_FORM = FALSE))

plot_logical_columns(form_primary_cohort %>% dplyr::select(-HAS_PRIMARY_HF),"Form","Proportion with HF Form")

