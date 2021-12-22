#' @export
#' @family data-utility
load_data <- function(table_name){
  method <- config::get("data_retrieve_method")

  if(method =='db'){
    return(load_db(table_name))
  }
  else if(method == 'file'){
    data_dir <- config::get("hf_file_dir")
    path <- file.path(data_dir, paste(table_name,"csv",sep="."))
    return(read.csv(path,fileEncoding="UTF-8-BOM",stringsAsFactors = FALSE))
  }
}

#' @export
#' @family data-utility
load_db <- function(table_name){
  dw <- config::get("datawarehouse")
  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = dw$drive,
                         Server = dw$server,
                         Database = dw$database,
                         UID = dw$uid,
                         PWD = dw$pwd)
  table_name <- toupper(table_name)
  query <- paste("SELECT * FROM ", table_name,sep="")

  # if the table is a blob,
  # columns need to be reordered such that the varchar(max)
  # field is the final column as described in
  # https://github.com/r-dbi/odbc/issues/86
  if(stringr::str_detect(table_name,"(?i)blob")){
    query <- paste("SELECT UPDT_DT_TM,ENCNTR_ID,EVENT_ID,BLOB_LENGTH,EVENT_CD,BLOB_CONTENTS FROM ", table_name,sep="")
  }

  df <- odbc::dbGetQuery(con,query)

  odbc::dbDisconnect(con)
  return(df)
}


#' @export
execute_query <- function(query){

  dw <- config::get("datawarehouse")
  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = dw$drive,
                         Server = dw$server,
                         Database = dw$database,
                         UID = dw$uid,
                         PWD = dw$pwd)

  df <- odbc::dbGetQuery(con,query)

  odbc::dbDisconnect(con)
  return(df)

}

#' @export
get_forms<- function(forms,dcp_forms_activity,pattern){
  dcp_forms_activity %<>% dplyr::filter(stringr::str_detect(DESCRIPTION,pattern))

  forms %<>% dplyr::filter(PARENT_EVENT_ID %in% dcp_forms_activity$PARENT_ENTITY_ID) %>%
    dplyr::left_join(
      dplyr::select(dcp_forms_activity,PARENT_ENTITY_ID,FORM_DT_TM), by = c("PARENT_EVENT_ID" = "PARENT_ENTITY_ID"))
  return(forms)
}


#' @export
write_db <- function(df,con,table_name){
  batch_size <- min(10000,nrow(df))
  indexs <- seq(from = 0, to= nrow(df),batch_size)
  i <- 1
  for(idx in indexs){
    if(i<length(indexs)){
      end <- indexs[[i+1]] -1
      xx<- df[idx:end,]

    }
    else{
      xx<- df[idx:nrow(df),]
      print(nrow(xx))
    }

    data<- RODBCDBI::dbWriteTable(con, xx, name = table_name,
                                  row.names=FALSE,append=TRUE,overwrite=FALSE)

    i<-i+1
  }

}

#' @export
str_to_datetime <- function(df) {
  # get columns
  pattern <- "(?i)(_dt_*tm)|(?i)(_date_time)"
  dttm_cols <- names(df) %>% .[stringr::str_detect(., pattern)]

  # do conversion
  df %<>%  dplyr::mutate_at(dttm_cols,
                            purrr::partial(lubridate::ymd_hms, tz = "Australia/Sydney", truncated = 3))


  return(df)
}

#' @export
get_first_non_na <- function(x){
  y <- dplyr::first(na.omit(x))
  return(y)

}


#' Assign defaults to dotted inputs
#'
#' `...` is a list of arguments, usually coming from `...` passed directly to
#'  the function.
#' @param .default_dots a named list of (typically) expressions, that specify
#'        default values for args in `...` if they aren't given by `...`
#' @return the input `...`,  with any named args from `.default_dots`
#'         missing from `...` being set by `defaults`.
#' @export
#' @keywords internal
set_default_dots <- function(.default_dots, ...) {
  `%||%` <- rlang::`%||%`
  out <- rlang::enexprs(...)
  defaults <- .default_dots

  for (nm in names(defaults)) out[[nm]] <- out[[nm]] %||% defaults[[nm]]

  return(out)
}
