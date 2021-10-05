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

proportion_bar_plot <- function(df,column,title){
  plot<- df %>%
     ggplot2::ggplot(., aes(x=!!rlang::sym(column),fill=!!rlang::sym(column))) +
    ggplot2::geom_bar(aes(y = ..count..),stat="count",alpha=0.5,show.legend = FALSE) +
    ggplot2::xlab(title) + 
    ggplot2::ylab("Count") + 
    ggtitle(paste0(title, " (n = ",nrow(df), ")")) +
    geom_text(
       aes(label=paste(round((..count..)/sum(..count..)*100,0),"%",sep=""),y= ..count..),
       stat='count',
       vjust=-0.25
   ) + 
  theme_minimal()
  
  return(plot)
}

conditional_bar_plot <- function(df,var1,var2,title){
  
  var1<- rlang::sym(var1)
  var2<- rlang::sym(var2)
  plot <- df%>%
  dplyr::count(!!var1,!!var2) %>%
  dplyr::group_by(!!var1) %>%
  dplyr::mutate(prop = prop.table(n)*100) %>%
  ggplot2::ggplot(., aes(x=!!var1,y=n,fill=!!var2)) + 
  geom_bar(stat="identity",position=position_dodge(0.7),width=0.7,alpha=0.5) +
  geom_text(aes(label= paste(round(prop,0),"%",sep=""),y=n),position=position_dodge(0.7),vjust=-0.5) +
  labs(y="Count",x=var1, title = paste0(title, " (N = ",nrow(df), ")")) +
  scale_fill_discrete(name = var2) + 
  theme_minimal()
  
  return(plot)
}

plot_histogram <- function(df,column, title,threshold = NULL){
  var1<- rlang::sym(column)

  df %<>% dplyr::filter(!is.na(!!var1))
  
  
  plot <- df %>%
  ggplot2::ggplot(., aes(x=!!var1,fill=!!var1)) + 
   geom_histogram(fill="lightblue",alpha=0.5) +
  labs(y="Count",x=var1, title = paste0(title, " (N = ",nrow(df), ")"))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    stat_bin(aes(y=..count.., label=ifelse(..count..==0,"",..count..)), geom="text", vjust=-.5) +
  theme_minimal()
    

  
  if(!is.null(threshold)){
    plot <- plot + geom_vline(aes(xintercept=threshold),
            color="red", linetype="dashed", size=1)
  }
  
  return(plot)
}

plot_histogram_group <- function(df,var1,var2,title){
  var1<- rlang::sym(var1)
  var2<- rlang::sym(var2)

  df %<>% dplyr::filter(!is.na(!!var1)) %>%
    dplyr::filter(!is.na(!!var2))
  
  
  plot <- df %>%
  ggplot2::ggplot(., aes(x=!!var1,fill=!!var2,color=!!var2)) + 
   geom_histogram(aes(y=..density..),alpha=0.5, position="identity") +
    geom_density(alpha=0.3)+
  labs(y="Count",x=var1, title = paste0(title, " (N = ",nrow(df), ")"))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_minimal()
    
  
  return(plot)
}

plot_logical_columns <- function(df,var1,title){
  var1<- rlang::sym(var1)
  
  plot_data <- df %>% 
    dplyr::summarise_if(is.logical,sum) %>%
   tidyr::pivot_longer(everything(),values_to = "N",names_to = rlang::as_string(var1)) %>%
  dplyr::mutate(prop = N/nrow(df)*100)

  plot <- plot_data %>%
       ggplot2::ggplot(., aes(x=!!var1,y=N,fill=!!var1)) +
      ggplot2::geom_bar(position="dodge", stat="identity",show.legend = FALSE) +
    geom_text(aes(label= paste(round(prop,0),"%",sep=""),y=N),vjust=-0.5) + 
    labs(y="Count",x=var1, title = paste0(title, " (N = ",nrow(df), ")"))
  
  return(plot)
}


## -----------------------------------------------------------------------------
plot_pie_chart <- function(df,var1,title){
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
    labs(y="Count",x=var1, title = paste0(title, " (N = ",nrow(df), ")"))
    
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

## -----------------------------------------------------------------------------
location_string <- "Royal North Shore"
hf_diag <- diagnosis %>%
  dplyr::filter(stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50"))

qual_enc <- encounter %>%
  dplyr::filter(ENCNTR_ID %in% hf_diag$ENCNTR_ID & LOC_FACILITY_CD == location_string)

cohort <- qual_enc %>%
  dplyr::distinct(PERSON_ID)

## ----dist-enc-plot,fig.cap="Distribution of the number of encounters for each patient."----
counts <- qual_enc %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::summarise(N = dplyr::n())

proportion_bar_plot(counts,"N", "Number of encounters per patient")

## -----------------------------------------------------------------------------
extract_hf_diagnosis <- function(df){
  df %<>%
  dplyr::group_by(PERSON_ID) %>%
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

primary_temp <- cohort %>% 
  dplyr::left_join(hf_diag_primary,by="PERSON_ID") %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::summarise(
    `Congestive heart failure` = any(SOURCE_IDENTIFIER == "I50.0" | SOURCE_IDENTIFIER == "I50"),
    `Heart failure, unspecified` = any(SOURCE_IDENTIFIER == "I50.9"),
    `Left ventricular failure` = any(SOURCE_IDENTIFIER == "I50.1")
  ) %>%
  replace(is.na(.), FALSE) %>%
  dplyr::mutate(`Only secondary HF diagnosis` = !`Congestive heart failure` & !`Heart failure, unspecified` & !`Left ventricular failure`)

hf_diag_secondary <- hf_diag %>%
  dplyr::filter(DIAG_PRIORITY != 1 & ENCNTR_ID %in% qual_enc$ENCNTR_ID)

secondary_temp <- cohort %>% 
  dplyr::left_join(hf_diag_secondary,by="PERSON_ID") %>%
  dplyr::group_by(PERSON_ID) %>%
  dplyr::summarise(
    `Congestive heart failure` = any(SOURCE_IDENTIFIER == "I50.0" | SOURCE_IDENTIFIER == "I50"),
    `Heart failure, unspecified` = any(SOURCE_IDENTIFIER == "I50.9"),
    `Left ventricular failure` = any(SOURCE_IDENTIFIER == "I50.1")
  ) %>%
  replace(is.na(.), FALSE) %>%
  dplyr::mutate(`Only primary HF diagnosis` = !`Congestive heart failure` & !`Heart failure, unspecified` & !`Left ventricular failure`)


hf_diag_primary_count <- hf_diag_primary %>%
  dplyr::group_by(ENCNTR_ID) %>%
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::filter(N > 1)

## ----icd10-dist-plot, fig.cap="Distribution of primary and secondary ICD10 diagnosis."----

p1 <- plot_logical_columns(primary_temp,"ICD10","Primary HF ICD10") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
  ggplot2::ylim(0,2000)

p2 <- plot_logical_columns(secondary_temp,"ICD10","Secondary HF ICD10") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2))+ 
  ggplot2::ylim(0,2000)

ggpubr::ggarrange(p1,p2,labels= c("A","B"), ncol = 1, nrow = 2)

## -----------------------------------------------------------------------------
n_only_prim <-  sum(secondary_temp$`Only primary HF diagnosis`)

## ----primary-diag-secondary-hf-tab--------------------------------------------
only_secondary_cohort <- secondary_temp %>% dplyr::filter(!`Only primary HF diagnosis`)

primary_diag_of_secondary <- diagnosis %>%
  dplyr::filter(!stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") & 
                  PERSON_ID %in% only_secondary_cohort$PERSON_ID &
                  DIAG_PRIORITY == 1 &
                  SOURCE_VOCABULARY_CD == "ICD10-AM" &
                  ENCNTR_ID %in% qual_enc$ENCNTR_ID) 

primary_diag_of_secondary %>%
  dplyr::group_by(SOURCE_STRING) %>%
  dplyr::summarise(N = dplyr::n_distinct(PERSON_ID)) %>% 
  dplyr::mutate(prop = round(N/dplyr::n_distinct(primary_diag_of_secondary$PERSON_ID)*100,1)) %>%
  dplyr::arrange(desc(N)) %>% 
  dplyr::top_n(15) %>%
  knitr::kable(caption = "Top 15 primary diagnosis for those with only a secondary heart failure diagnosis.")


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

## ----age-sex-plot, fig.cap="Distribution of Sex conditioned on Age."----------
conditional_bar_plot(primary_enc,"AGE_BINNED","SEX_CD", "Age and Sex") +
  scale_fill_discrete(name = "SEX") + 
  xlab("AGE")

## ----los-dis-plot, fig.cap="Distribution of Length of Stay"-------------------

plot_histogram(primary_enc,"LOS","Distribution of LOS",mean(primary_enc$LOS,na.rm=TRUE)) + xlab("LOS (days)") + xlim(0,50)

## ----encntr-type-plot, fig.cap="Distribution of Encounter Type."--------------
proportion_bar_plot(primary_enc,"ENCNTR_TYPE_CD","Distribution of Encounter Type.")

## ----acd-tab------------------------------------------------------------------
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


## ----his-diag-plot, fig.cap="Distribution of historical diagnosis."-----------
his_hf_diag <- diagnosis_history %>%
  dplyr::filter(stringr::str_detect(SOURCE_IDENTIFIER,"(?i)I50") &
                  PERSON_ID %in% primary_cohort$PERSON_ID)

summary_his_diag <- extract_hf_diagnosis(his_hf_diag) %>%
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

plot_logical_columns(hf_order,"ORDERS","Proportion of Encounter with Order")



## ----echo-time-plot,fig.cap="Distribution of the time taken for an echocardiogram."----

mean_delta <- mean(hf_order$ECHO_DELTA,na.rm = TRUE)
plot_histogram(hf_order %>% dplyr::filter(ECHO_DELTA < 75),
               "ECHO_DELTA","Distribution of time taken for Echo",
               mean_delta) + 
    xlab("Time take for echo since admission (days)")

## ----path-test-plot,fig.cap="Results of pathology tests."---------------------


p1<-plot_pie_chart(hf_order,"BNP_BIN","BNP Results by Group")
p2<-plot_pie_chart(hf_order,"IRON_BIN","BNP Results by Group")
p3<-plot_pie_chart(hf_order,"TRANSFERRIN_SAT_BIN","BNP Results by Group")

#ggpubr::ggarrange(p1,p2,p3,nrow=3,ncol=1,labels=c("A","B","C"))


p1
p2
p3

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

hf_form <- execute_query("SELECT * from DCP_FORMS_ACTIVITY where DESCRIPTION = 'Heart Failure (MACARF) Referral Form' or DESCRIPTION = 'Heart Failure - Management of Cardiac Function Enrolment'")


form_primary_cohort <- primary_cohort %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure - Management of Cardiac Function Enrolment',"ENROLMENT"), by = c("PERSON_ID" = "ENROLMENT_PERSON_ID")
  ) %>%
  dplyr::left_join(
    extract_form(hf_form,'Heart Failure (MACARF) Referral Form',"REFERRAL"), by = c("PERSON_ID" = "REFERRAL_PERSON_ID")
  ) %>%
  tidyr::replace_na(list(ENROLMENT_HAS_FORM = FALSE, REFERRAL_HAS_FORM = FALSE))

plot_logical_columns(form_primary_cohort %>% dplyr::select(-HAS_PRIMARY_HF),"Form","Proportion with HF Form")

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

summary_his_diag <- extract_hf_diagnosis(his_hf_diag) %>%
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

