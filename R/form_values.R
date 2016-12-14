cancer_value <- function(cancer_str) {
  cancer_types <- list(
    "all" = "001",
    "bladder" = "071",
    "brain & ons" = "076",
    "breast (female)" = "055",
    "breast (female in situ)" = "400",
    "cervix" = "057",
    "childhood (ages <15, all sites)" = "516",
    "childhood (ages <20, all sites)" = "515",
    "colon & rectum" = "020",
    "esophagus" = "017",
    "kidney & renal pelvis" = "072",
    "leukemia" = "090",
    "liver & bile duct" = "035",
    "lung & bronchus" = "047",
    "non-hodgkin lymphoma" = "086",
    "melanoma of the skin" = "053",
    "oral cavity & pharynx" = "086",
    "ovary" = "061",
    "pancreas" = "040",
    "prostate" = "066",
    "stomach" = "018",
    "thyroid" = "080",
    "uterus" = "058"
  )

  if(!(cancer_str %in% names(cancer_types))) {
    stop(paste(cancer_str, " is not a recognized cancer type"))
  }

  return(cancer_types[[cancer_str]])
}

race_value <- function(race_str) {
  race_types <- list(
    "all" = "00",
    "white incl. hispanic" = "01",
    "white hispanic" = "06",
    "white non-hispanic" = "07",
    "black incl. hispanic" = "02",
    "hispanic" = "05",
    "american indian/AK native incl. hispanic" = "03",
    "asian/pacific islander incl. hispanic" = "04"
  )

  stopifnot(race_str %in% names(race_types))

  return(race_types[[race_str]])
}

sex_value <- function(sex_str) {
  sex_types <- list(
    "all" = "0",
    "males" = "1",
    "females" = "2"
  )

  stopifnot(sex_str %in% names(sex_types))

  return(sex_types[[sex_str]])
}

age_value <- function(age_str) {
  age_types <- list(
    "all" = "001",
    "<50" = "009",
    "50+" = "136",
    "<65" = "006",
    "65+" = "157"
  )

  stopifnot(age_str %in% names(age_types))

  return(age_types[[age_str]])
}

statistic_value <- function(statistic_str) {
  statistic_types <- list(
    "incidence" = "01",
    "mortality" = "02"
  )
}
