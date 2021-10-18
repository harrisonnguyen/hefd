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

plot_logical_columns <- function(df,var1,title,n_suffix=NULL,vjust=-0.5){
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
    geom_text(aes(label= paste(round(prop,0),"%",sep=""),y=N),vjust=vjust) + 
    labs(y="Count",x=var1, title = title)
  
  return(plot)
}


