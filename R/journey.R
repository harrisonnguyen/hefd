#' Link Encounters Into Journeys
#'
#' Accepts df of encounters, and groups them by JOURNEY_KEY.
#'  This JOURNEY_KEY groups sets of encounters according to whether they
#'  overlap/their proximity in time
#'
#' @param df The dataframe with rows carrying PID, EID, admit/disch datetimes
#'   Should be cleaned by cleaner first (add example)
#' @param max_merge_gap_hours The maximum number of hours between
#'   the end of one encounter, and the start of the next, such that they still
#'   be grouped as one journey
#' @param ... see section Column Specifications
#' @return A dataframe with an additional variable, JOURNEY_KEY which groups
#'  corresponds to the ENCNTR_KEY of the earliest encounter within the journey.
#'  additionaly, has the following cols: `JOURNEY_START`, `JOURNEY_END`,
#'  `JOURNEY_DAYS`
#'
#' @family journey_building
#' @name link_encounters_by_timestamps
#' @aliases link_encounters_to_journeys
#' @importFrom magrittr  %<>% %>%
#' @importFrom dplyr select group_by do mutate ungroup vars funs
#' @importFrom lubridate as_datetime
#' @export
#'
link_encounters_by_timestamps <- function(
  df, ..., max_merge_gap_hours = 24,
  .default_dots = rlang::exprs(
    person_key = person_key,
    encntr_key = encntr_key,
    journey_key = JOURNEY_KEY
  )){
  `%||%` <- rlang::`%||%`
  expr <- rlang::expr

  args <- set_default_dots(.default_dots, ...)


  #L$debug("Before defaults: arguments look like: %s", pretty_string(args))

  #L$debug("arguments look like: %s", pretty_string(args))

  df %<>%
    dplyr::group_by(!!args$person_key) %>%
    dplyr::do(
      link_encs_in_groups(., max_merge_gap_hours = max_merge_gap_hours, !!! args))

  df %<>%
    journey_length(!!! args) %>%
    journey_order(!!!args)

  return(df)
}
#' Link encounters inside a group
#'
#' @family journey_building
#' @export
#' @keywords internal
link_encs_in_groups <- function(
  encs,
  ...,
  max_merge_gap_hours = 24,
  .default_dots = rlang::exprs(
    journey_key = JOURNEY_KEY,
    encntr_key = ENCNTR_ID,
    admit_dttm = BEG_EFFECTIVE_DT_TM,
    discharge_dttm = DISCH_DT_TM
  )
){

  enstr <- rlang::as_name

  args <- set_default_dots(.default_dots, ...)

  # string args
  strargs <- args %>% purrr::map(enstr)


  intervals <- IRanges::IRanges(start = as.numeric(dplyr::pull(encs, strargs$admit_dttm)),
                                end = as.numeric(dplyr::pull(encs, strargs$discharge_dttm)))

  journey_intervals <- intervals %>%
    IRanges::reduce(min.gapwidth = as.integer(max_merge_gap_hours*3600),
                    with.revmap = TRUE)

  origin_idxs <-
    S4Vectors::mcols(journey_intervals)$revmap %>%
    sapply(FUN = function(x) return(x[[1]]))

  journey_origins <- encs %>%
    dplyr::pull(strargs$encntr_key)

  journey_origins %<>% .[origin_idxs]

  hits <- IRanges::findOverlaps(intervals, journey_intervals)

  encs[strargs$journey_key] <- journey_origins[S4Vectors::to(hits)]
  return(encs)
}


#' @title Terminate Journeys on encounters having particular values
#'
#' @description
#' Helper function for second pass at journey building.
#'     This time, validates / Checks that encounters having characteristics
#'     that indicate the encounter should terminate its journey
#' `r lifecycle::badge("deprecated")`
#'
#' @family journey_building
#' @section TODO:
#'     Set this as internal, wrap it in something
#' @family deprecated
#' @param journeys the dataframe representing the journey-keyed encounters
#' @param ... Conditions to pass on to check an encounter should terminate
#'     its journey
#' @return The df, but with the journey key reworked to reflect the
#'     termination. Journey length, etc, should also be affected.
#' @examples
#' \dontrun{
#' journeys <- encounters %>%
#'    link_encounters_by_timestamps()
#'
#' dispositions <- c("Departed - Treatment Completed", "Death Type 1")
#' rekeyed <- journeys %>%
#'    terminate_journeys_by(DISCH_DISPOSITION %in% dispositions)
#' }
#' @export
terminate_journeys_by <- function(journeys, ...) {
  lifecycle::deprecate_warn(
    "2.0.0",
    "terminate_journeys_by()"
  )

  out <- journeys %>%
    dplyr::mutate(JKEY_OLD = JOURNEY_KEY) %>%
    dplyr::mutate(FINAL_ENC = ...) %>%
    dplyr::rename_at(dplyr::last(names(.)), ~ "IS_FINAL")

  L$info(glue("Categorized {rows} encs as final",
              rows = out %>% dplyr::filter(IS_FINAL) %>% nrow()))
  out %<>%
    dplyr::group_by(PERSON_KEY) %>%
    dplyr::mutate(NEXT_ENC = dplyr::lead(ENCNTR_KEY)) %>%
    dplyr::mutate(NEXT_JOURNEY = dplyr::lead(JOURNEY_KEY)) %>%
    dplyr::mutate(IS_FINAL = IS_FINAL | NEXT_JOURNEY != JOURNEY_KEY) %>%
    dplyr::mutate(IS_FINAL = dplyr::if_else(IS_FINAL, IS_FINAL, IS_FINAL, missing = TRUE))

  L$info(glue("Categorized {rows} encs as final based on position",
              rows = out %>% dplyr::filter(IS_FINAL) %>% nrow()))

  out %<>%
    dplyr::do(split_on_finals(.)) %>%
    dplyr::mutate(JOURNEY_KEY = as.integer(JOURNEY_KEY)) %>%
    journey_length()


  return(out)
}

#' Split journeys on FINAL_ENC == TRUE
#'
#' Given a df w/ JOURNEY_KEY, ENCNTR_KEY, FINAL_ENC cols
#'    Where FINAL ENC is a logical, terminate jouneys and
#'    begin new ones when you see FINAL_ENC == true
#' @keywords internal
#' @param group the group given by a dplyr::group_by %>% dplyr::do
#'      pipe on which to operate
split_on_finals <- function(group){

  current_journey <- group[[1, 'JOURNEY_KEY']]
  prev_journey <- 0
  for (i in 1:nrow(group)){
    if (group[[i, 'JOURNEY_KEY']] == prev_journey){
      group[[i, 'JOURNEY_KEY']] <- current_journey
    }
    # If you reach a final, better update to next
    if (group[[i, 'IS_FINAL']]){
      prev_journey <- current_journey
      current_journey <- group[[i , 'NEXT_ENC']]
    }
  }
  return(group)
}

#' Set start/end times for journeys
#'
#' @family journey_building
#' @keywords internal
#' @export
#' @param journeys the journey df
#' @param ... see section Column Specification
#' @return the df, but w/ start, end, and LOS in days
journey_length <- function(
  journeys,
  ...,
  .default_dots = rlang::exprs(
    journey_key = JOURNEY_KEY,
    journey_start = JOURNEY_START,
    journey_end = JOURNEY_END,
    journey_days = JOURNEY_DAYS,
    admit_dttm = ADMIT_DTTM,
    discharge_dttm = DISCH_DTTM,
  )
){
  enstr <- rlang::as_name

  args <- set_default_dots(.default_dots, ...)

  # string args
  strargs <- args %>% purrr::map(enstr)

  out <- journeys %>%
    dplyr::group_by(!!args$journey_key) %>%
    dplyr::mutate(!! strargs$journey_start := min(!!args$admit_dttm),
                  !! strargs$journey_end   := max(!!args$discharge_dttm),
                  !! strargs$journey_days  := as.double(!!args$journey_end - !!args$journey_start,
                                                        units = "days")) %>%
    dplyr::ungroup()

  return(out)
}

# Apac Linking ---------------

# Helper for relink_apac_to_journeys (groupwise)
relink_apac_to_journey <- function(
  person_journeys,
  ...,
  .default_dots = rlang::exprs(
    person_key = person_key,
    encntr_key = encntr_key, journey_key = JOURNEY_KEY,
    admit_dttm = admit_dttm, discharge_dttm = discharge_dttm,
    journey_start = JOURNEY_START, journey_end = JOURNEY_END,
    max_gap_hrs = 24
  )
) {
  args <- set_default_dots(.default_dots, ...)
  enstr <- rlang::as_name
  hours <- lubridate::hours
  # Transform dttms to integers
  # We want to find whether apac overlapped the
  # journey start (within max_gap_hrs)
  # or the journey end (within max_gap_hrs)

  person_journeys %<>%
    dplyr::mutate_at(
      dplyr::vars(
        !!args$admit_dttm, !!args$discharge_dttm,
        !!args$journey_start, !!args$journey_end),
      as.numeric
    )
  # Handle case where a person doesnt have any apacs
  person_lacks_apacs <- person_journeys %>% {
    any(
      is.na(.[[enstr(args$admit_dttm)]]) |
        is.na(.[[enstr(args$discharge_dttm)]])
    )
  }

  if (person_lacks_apacs) {
    L$info(
      "No APAC found for: %s",
      unique(person_journeys[[enstr(args$person_key)]]))

    person_journeys %<>%
      dplyr::select(!!args$journey_key) %>%
      dplyr::mutate(apac_start = FALSE, apac_end = FALSE)

    return(person_journeys)
  }

  # Dplyr doesnt support IRanges columns, do it separately

  # Apac span is full range
  apac_range <- person_journeys %>%
    {
      IRanges::IRanges(
        start = .[[enstr(args$admit_dttm)]],
        end   = .[[enstr(args$discharge_dttm)]]
      )
    }

  journey_edge_nms <- c(enstr(args$journey_start), enstr(args$journey_end))
  # Treat everything as numeric ranges, but the journey_start
  # and journey_end ranges only have length of 1 as they are instants

  for (nm in journey_edge_nms) {
    # If journey start or journey end overlap any apac, flag it
    intervals <- person_journeys %>%
      .[[nm]] %>%
      IRanges::IRanges(width = 1)

    person_journeys %<>%
      dplyr::mutate(
        !!nm := IRanges::overlapsAny(
          intervals,
          apac_range,
          maxgap =  as.numeric(hours(!!args$max_gap_hrs)))
      )
  }

  person_journeys %<>%
    dplyr::select(
      !!args$journey_key,
      apac_start = !!args$journey_start,
      apac_end = !!args$journey_end)

  person_journeys
}

#' @title Flag Journeys with the presence of APAC @ Start or end
#'
#'
#' @description
#' Given the set of APAC encounters, and linked journeys, find which
#' journeys began or ended on APAC
#' `r lifecycle::badge("experimental")`
#'
#' @section journeys->apac_start apac_end:
#' `apac_start` and `apac_end`, given by [relink_apac_to_journeys()]
#'  corresponds to whether the patient was admitted
#'  from, or discharged to APAC care at the start / end of their journey.
#'
#' @param journeys a df having been linked by [link_encounters_by_timestamps()]
#'        having `person_key`, `journey_start`/`end`, `journey_key`
#' @param apac df containing only APAC encounters (`"APAC"` in `facility`
#'        field), as well as `admit/discharge``_dttm`, and `person_key`.
#' @inheritParams set_default_dots
#' @param max_gap_hrs the maximum gap between APAC and the start/end of a
#'        journey for which they may still be considered related,
#' @export
#' @family journey_building
relink_apac_to_journeys <- function(
  journeys, apac,
  ...,
  .default_dots = rlang::exprs(
    person_key = person_key,
    encntr_key = encntr_key, journey_key = JOURNEY_KEY,
    admit_dttm = admit_dttm, discharge_dttm = discharge_dttm,
    journey_start = JOURNEY_START, journey_end = JOURNEY_END,
    max_gap_hrs = 24
  )
) {
  enstr <- rlang::as_name
  args <- set_default_dots(.default_dots, ...)

  # Strip down the APAC and journey dfs to the keys and vals they need to join
  out <- journeys %>%
    dplyr::select(!!args$person_key, !!args$journey_key,
                  !!args$journey_start, !!args$journey_end) %>%
    dplyr::distinct(!!args$journey_key, .keep_all = TRUE)

  apac %<>%
    dplyr::select(!!args$person_key, !!args$admit_dttm, !!args$discharge_dttm)


  # join the dfs on person key, as we will operate within person key groups
  out %<>%
    dplyr::left_join(apac, by = enstr(args$person_key), suffix = c("", ".apac"))
  # Perform the groupwise linkage

  out %<>%
    dplyr::group_by(!!args$person_key) %>%
    dplyr::do(relink_apac_to_journey(., !!!args)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::matches(enstr(args$person_key)))

  # Restore the previous info
  out %<>%
    dplyr::right_join(
      journeys,
      by = enstr(args$journey_key),
      suffix = c(".apac", ""))

  out
}

#' Give a order for each encounter based on their journey key in ascending order
#' e.g. for a journey with 3 encounters, the first encounter will have rank 1,
#' second with rank 2 and so on
#' @family journey_building
#' @keywords internal
#' @export
#' @param journeys the journey df
#' @param ... see section Column Specification
#' @return the df, journey_order, the final encounter
journey_order <- function(
  journeys,
  ...,
  .default_dots = rlang::exprs(
    journey_key = JOURNEY_KEY,
    discharge_dttm = DISCH_DTTM,
    encntr_key = ENCNTR_ID,
    updt_dttm = UPDT_DT_TM
  )
){
  enstr <- rlang::as_name

  args <- set_default_dots(.default_dots, ...)

  # string args
  strargs <- args %>% purrr::map(enstr)


  out <- journeys %>%
    dplyr::group_by(!!args$journey_key) %>%
    dplyr::mutate(ENCNTR_ORDER = rank(!!args$discharge_dttm,ties.method="first"),
                  N_ENCNTR = dplyr::n(),
                  UPDT_DT_TM = max(!!args$updt_dttm,na.rm = TRUE))

  final_encntr <- out %>%
    dplyr::filter(ENCNTR_ORDER == N_ENCNTR) %>%
    dplyr::select(!!args$journey_key,!!args$encntr_key)

  out %<>%
    dplyr::left_join(final_encntr,by=strargs$journey_key,suffix=c("","_END")) %>%
    dplyr::ungroup()

  return(out)
}

#' Extracts the unique journey_id and assigns an order for each of the patient's
#' journeys and calculates the time between subsequent journeys
#'
#'
#' @family journey_building
#' @keywords internal
#' @export
#' @param journeys the journey df
#' @param last_journey_number a df containing journey_key and the most recent journey rank number
#' @param ... see section Column Specification
#' @return the df,
#' @return updt_dt_tm the most recent update of the encounters within the journey
create_journey_table <- function(
  journeys,
  last_journey_number = NULL,
  ...,
  .default_dots = rlang::exprs(
    journey_key = JOURNEY_KEY,
    journey_start = JOURNEY_START_DT_TM,
    journey_end = JOURNEY_END_DT_TM,
    journey_day = JOURNEY_DAYS,
    updt_dttm = UPDT_DT_TM,
    encntr_key_end = ENCNTR_ID_END,
    person_key = PERSON_ID,
    last_journey_order = LASt_JOURNEY_ORDER
  )
){
  enstr <- rlang::as_name

  args <- set_default_dots(.default_dots, ...)
  # string args
  strargs <- args %>% purrr::map(enstr)


  out <- journeys %>%
    dplyr::select(!!args$person_key, !!args$journey_key,
                  !!args$journey_start,
                  !!args$journey_end,
                  !!args$journey_day, !!args$encntr_key_end,
                  N_ENCNTR,UPDT_DT_TM) %>%
    dplyr::distinct(!!args$journey_key,.keep_all = TRUE)

  # %>%
  #   dplyr::group_by(!!args$person_key) %>%
  #   dplyr::mutate(JOURNEY_ORDER = rank(!!args$journey_start,ties.method="first"))
  #
  # if(!is.na(last_journey_number)){
  #   out %<>%
  #     dplyr::left_join(last_journey_number,by = strargs$person_key) %>%
  #     dplyr::mutate(JOURNEY_ORDER = JOURNEY_ORDER + !!args$last_journey_order) %>%
  #     dplyr::select(-!!args$last_journey_order)
  # }


  return(out)
}

