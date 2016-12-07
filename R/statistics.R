
query_args <- function(l) {
  paste(names(l), l, sep="=", collapse = "&")
}


statistic_form_values <- function(by, state, cancer, race, sex, age) {
  ret <- list()
  if(by == "county" && (is.null(state) || state == "all")) {
    ret$state = "99"
  }
  else if(by == "county" && !is.null(state)) {
    stopifnot(state %in% maps::state.fips$abb)

    ret$state = maps::state.fips$fips[maps::state.fips$abb == state][1]
  }
  else if(by == "state") {
    ret$state = "00"
  }

  ret$cancer <- cancer_value(cancer)
  ret$race <- race_value(race)
  ret$sex <- sex_value(sex)
  ret$age <- age_value(age)

  return(ret)
}

incidence_table_url <- function(by, state, cancer, race, sex, age) {
  vals <- statistic_statistic_form_values(by, state, cancer, race, sex, age)

  args <- list(
    stateFIPS = vals$state,
    cancer = vals$cancer,
    race = vals$race,
    type = "incd",
    sortVariableName = "rate",
    sortOrder = "desc",
    output = "1"
  )

  if(!(cancer %in% c("childhood (ages <20, all sites)", "childhood (ages <15, all sites)"))) {
    args$age = vals$age
  }

  url <- paste0("https://statecancerprofiles.cancer.gov/incidencerates/index.php?", query_args(args))

  return(url)
}

mortality_table_url <- function(by, state, cancer, race, sex, age) {
  vals <- statistic_form_values(by, state, cancer, race, sex, age)

  args <- paste0(c(vals$state, vals$cancer, vals$race, vals$sex, vals$age, "0", "1", "1", "6"), collapse = "&")
  url <- paste0("https://statecancerprofiles.cancer.gov/cgi-bin/deathrates/data.pl/death.csv?", args)

  return(url)
}

download_csv <- function(url, dest = tempfile(), delete_rows = c(), skip = 0, ...) {
  download.file(url, destfile = dest)
  raw <- readLines(dest)

  if(length(delete_rows) > 0) {
    raw <- raw[-delete_rows]
  }

  # Cut off the first 8 lines
  raw <- raw[skip:length(raw)]

  # Find the first blank line, then cut off everything after
  last_row <- min(which(raw == ""))
  raw <- raw[1:last_row]

  dat <- readr::read_csv(paste(raw, collapse="\n"), ...)
  readr::write_csv(dat, dest)

  return(dat)
}

#' @param statistic "incidence" or "mortality"
#' @param by "county" or "state"
#' @param state restrict data download to one state; specify using state abbreviation (e.g. "MA"). Leave as NULL to retrieve data for all states.
#' @param cancer download data for a specific cancer type, see details for available options
#' @param race download data for a specific race
#' @param sex download data for a specific gender
#' @param age download data for a specific age group

#' @details
#'
#' Cancers:
#' \enumerate{
#'   \item all
#'   \item bladder
#'   \item brain & ONS
#'   \item breast (female)
#'   \item breast (female in situ)
#'   \item cervix
#'   \item colon & rectum
#'   \item esophagus
#'   \item kidney & renal pelvis
#'   \item leukemia
#'   \item liver & bile duct
#'   \item lung & bronchus
#'   \item non-hodgkin lymphoma
#'   \item melanoma of the skin
#'   \item oral cavity & pharynx
#'   \item ovary
#'   \item pancreas
#'   \item prostate
#'   \item stomach
#'   \item thyroid
#'   \item uterus"
#' }
#'
#' @examples
#' \dontrun {
#' # Download county level bladder cancer incidence data
#' dat <- cancer_statistics(statistic = "incidence", by = "county", cancer = "bladder")
#' }
#'
#' @export
cancer_statistics <- function(statistic = "incidence", by = "county", state = NULL, cancer = "all", race = "all", sex = "all", age = "all") {

  if(statistic == "incidence") {
    url <- incidence_table_url(by, state, cancer, race, sex, age)
    skip = 9
    value_column = "incidence_rate"
    delete_rows = c()
    col_names <- c(
      "county",
      "fips",
      "incidence_rate",
      "incidence_rate_95_confint_lower",
      "incidence_rate_95_confint_upper",
      "average_annual_count",
      "recent_trend_description",
      "recent_trend",
      "recent_trend_95_confint_lower",
      "recent_trend_95_confint_upper"
    )
  }
  else {
    url <- mortality_table_url(by, state, cancer, race, sex, age)
    skip = 13
    value_column = "mortality_rate"
    delete_rows <- c(13)
    col_names <- c(
      "county",
      "fips",
      "met_objective",
      "mortality_rate",
      "mortality_rate_95_confint_lower",
      "mortality_rate_95_confint_upper",
      "average_deaths_per_year",
      "recent_trend",
      "recent_5_year_trend",
      "recent_trend_95_confint_lower",
      "recent_trend_95_confint_upper"
    )
  }

  dat <- download_csv(url,
                      delete_rows = delete_rows,
                      skip = skip,
                      col_names = col_names,
                      col_types = strrep("c", length(col_names)))

  dat$suppressed <- dat[[value_column]] == "*"

  dat[, value_column] <- ifelse(dat[, value_column] %in% c("*", "¶ "), NA, dat[, value_column])
  dat[, value_column] <- stringr::str_replace(dat[[value_column]], "#", "")
  dat[, value_column] <- as.numeric(dat[[value_column]])

  return(dat)
}
