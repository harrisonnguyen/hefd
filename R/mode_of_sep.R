# ~~~~~~~~~~~~~~~~~~ ====
# Grouping/Behaviour Specifications =====

#' Rules for mode of separation
#' @name mode_of_separation_rules
#' @family mode_of_separation
#' @family encounter
NULL


#' @rdname mode_of_separation_rules
#' @export
mode_of_sep_groupings <- function(){

  out <- list()
  death <- list(
    title = "death",
    keywords = c("death", "dead", "died", "deceased"),
    suffix = c("On Arrival", "in ED", "with Autopsy", "without Autopsy"),
    suffix.class = c("on arrival", "in ed", "with autopsy", "without autopsy")
  )

  transfer <- list(
    title = "transfer",
    keywords = c("trans", "transfer"),
    suffix = c("in *(the)? *area\\/region", "out\\s*(side|of)\\s*area\\/region",
               "Nurs.*Home", "Psych", "hopsital"),
    suffix.class = c("internal_lhd", "external_lhd",
                     "RACF", "psychiatric", "hospital")
  )
  dama <- list(
    title = "dama",
    keywords = c("Own Risk", "Did not wait", "Refused", "Fail to attend", "Absconded")
  )
  home <- list(
    title = "home",
    keywords = c("Treatment Completed", "Discharged on leave", "Discharge by Hospital",
                 "No treatment required", "Intervention Completed")
  )

  out$home <- home
  out$death <- death
  out$transfer <- transfer
  out$dama <- dama
  return(out)
}







#' @section Base Rules:
#'  The following patterns/rules are used to relate the extracted top level
#'  classifications to an output mode of separation.
#' @export
#' @rdname mode_of_separation_rules
enc_sep_base_patterns <- function(first_pass = mode_sep_first,
                                  transfer = mode_sep_transfer,
                                  death = mode_sep_death,
                                  dests = destination,
                                  simple_vals = c("death", "dama", "home"),
                                  transfer_vals = c("transfer")){
  first_pass <- rlang::enexpr(first_pass)
  transfer <- rlang::enexpr(transfer)
  death <- rlang::enexpr(death)
  dests <- rlang::enexpr(dests)

  patterns <- rlang::exprs(
    !!first_pass %in% !!simple_vals ~ !!first_pass,
    !!first_pass %in% !!transfer_vals ~ enc_sep_case_transfer(!!transfer, !! dests))

  return(patterns)
}


#' @rdname mode_of_separation_rules
#' @section Transfer Rules:
#'  The following rules are called from the enc_sep_case_transfer function,
#'  as part of the evaluation of the base ruleset.
#' @export
enc_sep_transfer_patterns <- function(transfer = mode_sep_transfer,
                                      dests = destination){
  transfer = rlang::enexpr(transfer)
  dests = rlang::enexpr(dests)

  g <- function(...){
    use_env <- rlang::caller_env()
    glue::glue(..., .envir = use_env) %>% as.character() %>%
      return()
  }


  as_is <- c("RACF", "private Hospital")
  psych <- "psychiatric"
  pvt <- "private hospital"
  pub <- "public hospital"
  racf <- "RACF"

  psych_patterns <- rlang::exprs(
    tolower(!!dests) == !!pvt ~ !!g("{pvt} {psych}"),
    TRUE                      ~ !!g("{pub} {psych}")
  )

  all_patterns <- rlang::exprs(
    purrr::map_lgl(!!transfer, ~(!!psych %in% .)) ~ dplyr::case_when(
      !!!psych_patterns),
    purrr::map_lgl(!!transfer, ~(!!racf %in% .))  ~ !!racf,
    !!dests %in% !!as_is    ~ !!dests,
    TRUE                    ~ !!pub
  )

  return(all_patterns)
}


enc_sep_case_transfer <- function(transfer, dests){
  patterns <- enc_sep_transfer_patterns(!!transfer, !!dests)

  dplyr::case_when(!!!patterns)
}

find_keywords <- function(col, mapping){

  indexer <- mapping %>%
    dplyr::mutate(match_locs = purrr::map2(
      keywords, approx.match,
      function(keywords, approx.match){
        # was private$L
        out <- purrr::map2(keywords, approx.match,
                           ~ agrep(.x, col, max.distance = .y, ignore.case = TRUE, fixed = FALSE))
        return(out)
      }))

  return(indexer)
}

mapping_colnames <- function() {

  map_cols = c(
    "title", "keywords", "approx.match", "false.positive.matches",
    "prefix", "suffix", "prefix.approx.match",
    "suffix.approx.match", "suffix.descriptions", "prefix.class",
    "suffix.class", "ignore.prefix", "ignore.suffix",
    "ignore.prefix.approx.match", "ignore.suffix.approx.match")

  return(map_cols)
}

serial_as_tbl <- function(serial_list){
  nms <- serial_list %>%
    purrr::map(names) %>% purrr::reduce(union)

  out <- serial_list %>%
    purrr::transpose(.names = nms) %>% tibble::as_tibble()
  # Fill in the blanks for any missing cols
  missing <- mapping_colnames() %>% .[! . %in% nms]

  missing %<>%
    purrr::set_names() %>%
    purrr::map(~ NA_character_)

  out %<>%
    tibble::add_column(!!!missing) %>%
    dplyr::mutate_at(c("title"), as.character)
  return(out)
}

add_mapping_entries <- function(mappings){
  approx_filler <- function(kw, am){

    # If approx.match isn't set, it defaults to 0.0
    if (rlang::is_empty(am) | rlang::is_na(am)) am <- 0.0

    # If approx.match is given as a scalar, repeat that for all kw
    if (length(am) == 1) am %<>% rep_len(length.out = length(kw))

    # If approx.match was a missing, it's now defaulted everything to 0.0
    # If was a scalar, that's been repeated for all
    if (length(am) == length(kw)) return(am)
    msg <- glue::glue("For {pretty_string(kw)} invalid approx.match given!",
                      "length(approx.match) = {length(am)}",
                      "length(keywords)  = {length(kw)}")
    stop(msg)
  }

  mappings %<>%
    dplyr::mutate(approx.match = purrr::map2(keywords, approx.match,
                                             approx_filler))

  return(mappings)
}




add_keyword_cols <- function(.df, col,indexer){

  # By unnesting keywords, approxmatch, match_locs
  # and unnesting match_locs again, a unique index for each row is made
  unnest_cols <- c("keywords", "approx.match", "match_locs")
  preserve <-  indexer %>% names() %>%
    setdiff(purrr::map_chr(unnest_cols,rlang::as_name))

  indexer %<>%
    tidyr::unnest(unnest_cols, .preserve = dplyr::one_of(preserve)) %>%
    dplyr::mutate_at(unnest_cols[1:2], unlist) %>%
    tidyr::unnest(unnest_cols[3], .preserve = dplyr::one_of(preserve))

  # We need idx so we can join it on match locs
  # A nested tibble col will be added for each corresponding match
  out <- .df %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::nest_join(indexer, by = c("idx" = "match_locs"), name = "indexer")

  return(out)

}


# ~~~~~~~~~~~~~~ ========
# Classification Implementations =========

#' Classify the Mode of Separation for Encounters
#'
#' @family encounters
#' @family mode_of_separation
#' @export
#' @param encounters the encounter df, with a `disch_disposition` column.
#' @param enc_udf_dests the user defined field table, with a `destination`
#'        column, as given by [encounter_udf_add_dest()]
#' @param grouping_map a [contextractr::Contextractr] / `CLACK` compatible
#'        (serialized) grouping, conforming to
#'        [contextracr::Contextractr$add_serial()].
#'        Likely produced by [mode_of_sep_groupings()]
#' @return the `encounters` df, but with the extra columns as specified in the
#'         section Output.
#' @section Output:
#'  These will accumulate/collapse common combinations of `destination`
#'  values with `disch_disposition` values
#'
#'  * `enc_separation_mode`
#'    * This will correspond to some aggregation between destination and
#'      discharge disposition.
#'      * In particular, it will work on the output classifications made
#'        by `grouping_map`.
#'      * See [enc_sep_transfer_patterns()] and [enc_sep_base_patterns()]
#'        for a more detailed description of these rules.
#'  * `enc_separation_comment`
#'    * This adds some extra qualification to the grouping obtained-
#'      Presently this checks the `suffix.class` extracted from
#'      `grouping_map` in particular:
#'      * For the case of "transfer", it will extract
#'        `internal/external_lhd`, if detected
#'      * For the case of "death", it will extract any suffix class
#'        detected.
encounter_classify_separation <- function(encounters, enc_udf_dests, grouping_map = NULL,
                                          encounter_key = "ENCNTR_ID",
                                          dc_disp = "DISCH_DISPOSITION_CD",
                                          dest = "DESTINATION"){
  #L <- logging::getLogger(log_names$encounter)
  `%||%` <- rlang::`%||%`

  pexpr <- rlang::parse_expr
  g <- glue::glue
  enstr <- as.character


  dc_disp <- dc_disp
  dc_disp_sym <- pexpr(dc_disp)

  dest <- dest
  dest_sym <- pexpr(dest)

  # The output of a ctx$locate_keywords(df, colname) tends to be
  # appended columns: colname_groups , colname_keywords

  # Build these suffixes, as both string and symbol for NSE

  col_sfx <- c("groups" = "GROUPS", "keywords" = "KEYWORDS")
  cols <- col_sfx %>% purrr::map(~g("{dc_disp}_{.x}"))
  cols_syms <- cols %>% purrr::map(pexpr)

  selection <- rlang::enquo(dc_disp_sym)
  col <- selection %>% rlang::as_name()
  group_col <- glue::glue("{col}_GROUPS") %>% as.character()
  kw_col    <- glue::glue("{col}_KEYWORDS") %>% as.character()

  # Begin work to classify encs  =======
  # First pass - Find the keywords

  out <- enc_udf_dests %>%
    encounter_udf_add_dest() %>%
    dplyr::right_join(encounters,
                      by = encounter_key,
                      suffix = c("", ".encs"))

  grouping_map <- mode_of_sep_groupings()
  indx <- out %>%
    dplyr::pull(!!dc_disp_sym) %>%
    find_keywords(grouping_map %>%
                    serial_as_tbl() %>%
                    add_mapping_entries())


  out %<>% add_keyword_cols(!!dc_disp_sym, indx) %>%
    dplyr::mutate(!!group_col := purrr::map(indexer, "title")) %>%
    dplyr::mutate_at(group_col, ~ purrr::map(., unique)) %>%
    dplyr::mutate_at(group_col,~ paste0(.)) %>%
    dplyr::select(-idx,-indexer)


  out %<>%
    dplyr::rename(mode_sep_first = !!cols_syms$groups) %>%
    dplyr::mutate(mode_sep_first = purrr::map_chr(
      mode_sep_first, ~ .[[1]] %||% NA_character_))# %>%
    #dplyr::select(-c(!!cols_syms$keywords))

  # Now only map across the ones with suffixes, ie - death + transfer

  #next_map <- grouping_map %>%
  #  .[names(.) %in% c("death", "transfer")] %>%
  #  suffix_to_keywords()

  # Make new columns that capture whether these extra suffixes were found
  #for (nm in names(next_map)){
  #  ctx <- contextractr::Contextractr$new(serial = next_map[[nm]])

  #  out %<>% ctx$locate_keywords(!!dc_disp_sym)
  #  out %<>% dplyr::rename(!! g("mode_sep_{nm}") := !! cols_syms$groups,
  #                         !! g("keywords_{nm}") := !! cols_syms$keywords)
  #  out %<>% dplyr::select(-c(!!pexpr(g("keywords_{nm}"))))
  #}

  out %<>% classify_sep_dest(prefix = mode_sep, top_suffix = first, dests = DESTINATION)


  return(out)
}





#' Classify modes of sep using extracted groups & destination
#'
#' @usage classify_sep_dest(df, prefix = mode_sep, top_suffix = first,
#'                          dests = destination)
#' @keywords internal
classify_sep_dest <- function(df, ...){
  #L <- logging::getLogger(log_names$encounter)

  `%||%` <- rlang::`%||%`
  g <- glue::glue
  enstr <- rlang::as_name
  sym <- rlang::sym

  case <- dplyr::case_when
  if_else <- dplyr::if_else

  dots <- rlang::enexprs(...)
  defaults <- rlang::exprs(prefix = mode_sep, top_suffix = first, dests = destination)
  for (nm in names(defaults)) dots[[nm]] <- dots[[nm]] %||% defaults[[nm]]

  prefix <- dots$prefix
  top_suffix <- dots$top_suffix

  first_col <- g("{enstr(prefix)}_{enstr(top_suffix)}") %>% sym()
  death_col <- g("{enstr(prefix)}_death") %>% sym()
  xfers_col <- g("{enstr(prefix)}_transfer")  %>% sym()
  dests <- dots$dests

  patterns <- enc_sep_base_patterns(!!first_col, !!dests,
                                    !!death_col, !!dests)



  out <- df %>%
    dplyr::mutate(
      ENC_SEPARATION_MODE = dplyr::case_when(!!!patterns))


  out %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ENC_SEPARATION_MODE")), tolower) %>%
    dplyr::select(-dplyr::starts_with(enstr(prefix)))

  out
}

#'  classifies the destination given by
#' `Referred to Facility`
#'
#' classifies the destination given by
#' `Referred to Facility` value in `ENCOUNTER_UDF` table
#' to either private,racf or the given public hospital
encounter_udf_add_dest <- function(df,value_col = "VALUE_CD") {

  value_col <- rlang::sym(value_col)

  private_hospital_pattern <- paste0(
    "(?i)",
    c("Private Hosp", "PVTE","Macquarie University Hospital", "Private"),
    collapse = "|")

  racf_pattern <- paste0(
    "(?i)",
    c("ACF", "Aged Care", "Agred Care", "Retirement", "Nursing", "Lodge",
      "Residential", "Village", "Hostel", "Aurrum", "Chisholm", "ARV",
      "Loretto", "Ashburn", "Amity", "Riverglen", "Court",
      "HammondCare", "Apartments"),
    collapse = "|")

  df %<>%
    dplyr::mutate(
      DESTINATION = dplyr::case_when(
        stringr::str_detect(!!value_col, private_hospital_pattern) ~ "private Hospital",
        stringr::str_detect(!!value_col, racf_pattern)             ~ "RACF",
        TRUE                                    ~ as.character(!!value_col)
      ))

  return(df)
}


#' Promote suffix to own extraction group
#'
#' Promotes `suffix.class` + `suffix` pair `->` `title` + `keywords`, as in
#'     For each record, this becomes a whole new mapping, (ie if you have
#'     a list of `contextractr::Contextractr` mappings, each mapping becomes a
#'     list of mappings, and to give them to a
#'     `contextractr::Contextractr$new()` call, you would index into a top
#'     level element and give it that.
#'
#'     Helper function for encounter transforms until suffix extraction is
#'     implemented on contextractr.
#'
#' @param grouping_map a [contextractr::Contextractr$add_serial()] compatible
#'        list
#' @return the `grouping_map`, but each group element is replaced with a list
#'         of groups built from its subelements, changing:
#'         * `suffix` -> `keyword`
#'         * `suffix.class` -> `title`
#'
#' @examples
#' mapping <- list(
#'   death = list(title = "death",
#'     keywords = c("death", "dead", "died", "deceased"),
#'     suffix = c("with Autopsy", "without Autopsy"),
#'     suffix.class = c("with autopsy", "without autopsy")),
#'   transfer = list(
#'     title = "transfer",
#'     keywords = c("trans", "transfer"),
#'     suffix = c("Nurs.*Home", "Psych", "hopsital"),
#'     suffix.class = c("RACF", "psychiatric", "hospital")))
#'
#' out <- mapping %>% suffix_to_keywords()
#'
#' out %>% yaml::as_yaml() %>% cat()
#'
#' \dontrun{
#' ctx <- contextractr::Contextractr$new(serial = out$death)
#'
#' df <- fake_df()
#' df %<>% ctx$locate_keywords(fake_col)
#' }
#' @keywords internal
suffix_to_keywords <- function(grouping_map){
  out <- grouping_map %>%
    purrr::map(
      function(record){
        out <- record %>%
          .[names(.) %in% c("suffix", "suffix.class")] %>%
          purrr::pmap(function(suffix, suffix.class){
            out <- list(title = suffix.class, keywords = suffix)
            return(out)
          })
        return(out)
      }
    )

  return(out)
}
