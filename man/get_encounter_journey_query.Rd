% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_queries.R
\name{get_encounter_journey_query}
\alias{get_encounter_journey_query}
\title{Extracts the encounters that haven't been allocated a journey id}
\usage{
get_encounter_journey_query()
}
\description{
\code{join_tab} extracts encounters from the encounter table and encounter history table
\code{last_journey} gets the last journey currently for each patient
\code{last_encounter} gets the encounters associated for those journeys
using the above, we extract the encounters that haven't been allocated a journey key
and the most recent encounters for each patient, just in case their journeys
are continued with the most recent set of new encounters
}
\details{
be aware that we assume that each encounter can only be associated with one person_id
across the encounter and encounter history tables
}
\seealso{
Other query: 
\code{\link{get_acd_problem_query}()},
\code{\link{get_diagnosis_query}()},
\code{\link{get_disch_med_query}()},
\code{\link{get_discharge_referral}()},
\code{\link{get_echos_query}()},
\code{\link{get_hf_alert_query}()},
\code{\link{get_hf_encounter_query}()},
\code{\link{get_hfenrolment_form_query}()},
\code{\link{get_hfhomevisit_form_query}()},
\code{\link{get_hfreferral_form_query}()},
\code{\link{get_historical_hf_diag_query}()},
\code{\link{get_pathology_query}()},
\code{\link{get_previous_encounter_query}()},
\code{\link{get_referred_to_facility_query}()}
}
\concept{query}
