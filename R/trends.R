trend_url <- function() {

}

trend_form_values <- function(by, state, cancer, race, sex, age) {
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

#' @title cancer_trends
#' @description trends in cancer mortality over time
#'
#' @export
cancer_trends <- function(trend = "incidence", state = NULL) {
  if(trend == "incidence" && !is.null(state)) {
    stop("Cancer incidence trends not available by state.")
  }

  if(trend == "incidence") {

  }
  else {

  }
}
