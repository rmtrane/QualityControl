#' Recode DR Severity Level
#'
#' This function takes a vector of DR Severity Levels and returns the recoded values.
#' 'Not applicable' is set to NA
#'
#' @param DRSeverity character vector; Severity levels to be recoded
#'
#' @export

recode_DRsev <- function(DRSeverity){

  DRSeverity <- case_when(DRSeverity == 'Not applicable' ~ as.character(NA),
                          TRUE ~ str_sub(DRSeverity, start = 1, end = 2))

  recode <- case_when(
    DRSeverity %in% c(10, 12) ~ 1,
    DRSeverity %in% c(14, 15, 20) ~ 2,
    DRSeverity %in% c(35) ~ 3,
    DRSeverity %in% c(43) ~ 4,
    DRSeverity %in% c(47) ~ 5,
    DRSeverity %in% c(53) ~ 6,
    DRSeverity %in% c(60, 61) ~ 7,
    DRSeverity %in% c(65) ~ 8,
    DRSeverity %in% c(71) ~ 9,
    DRSeverity %in% c(75) ~ 10,
    DRSeverity %in% c(81) ~ 11,
    DRSeverity %in% c(85) ~ 12,
    TRUE ~ as.numeric(NA)
  ) %>% factor(levels = c(1:12, NA))

  return(recode)

}

