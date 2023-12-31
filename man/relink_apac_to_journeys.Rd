% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/journey.R
\name{relink_apac_to_journeys}
\alias{relink_apac_to_journeys}
\title{Flag Journeys with the presence of APAC @ Start or end}
\usage{
relink_apac_to_journeys(
  journeys,
  apac,
  ...,
  .default_dots = rlang::exprs(person_key = person_key, encntr_key = encntr_key,
    journey_key = JOURNEY_KEY, admit_dttm = admit_dttm, discharge_dttm = discharge_dttm,
    journey_start = JOURNEY_START, journey_end = JOURNEY_END, max_gap_hrs = 24)
)
}
\arguments{
\item{journeys}{a df having been linked by \code{\link[=link_encounters_by_timestamps]{link_encounters_by_timestamps()}}
having \code{person_key}, \code{journey_start}/\code{end}, \code{journey_key}}

\item{apac}{df containing only APAC encounters (\code{"APAC"} in \code{facility}
field), as well as \verb{admit/discharge``_dttm}, and \code{person_key}.}

\item{.default_dots}{a named list of (typically) expressions, that specify
default values for args in \code{...} if they aren't given by \code{...}}

\item{max_gap_hrs}{the maximum gap between APAC and the start/end of a
journey for which they may still be considered related,}
}
\description{
Given the set of APAC encounters, and linked journeys, find which
journeys began or ended on APAC
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\section{journeys->apac_start apac_end}{

\code{apac_start} and \code{apac_end}, given by \code{\link[=relink_apac_to_journeys]{relink_apac_to_journeys()}}
corresponds to whether the patient was admitted
from, or discharged to APAC care at the start / end of their journey.
}

\seealso{
Other journey_building: 
\code{\link{create_journey_table}()},
\code{\link{journey_length}()},
\code{\link{journey_order}()},
\code{\link{link_encounters_by_timestamps}()},
\code{\link{link_encs_in_groups}()},
\code{\link{terminate_journeys_by}()}
}
\concept{journey_building}
