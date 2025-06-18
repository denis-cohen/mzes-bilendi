## ---- Bilendi: Set name of dta  ----
bilendi_dta_path <- "dat-pretest/Mitarbeiter_Befragung_EN_Dummy_Data.dta"

## ---- Start up ----
.First <- function() {
  ## ---- Generate folders ----
  dir.create(paste0(getwd(), "/r"), showWarnings = F)
  dir.create(paste0(getwd(), "/rmd"), showWarnings = F)
  dir.create(paste0(getwd(), "/pdf"), showWarnings = F)
  dir.create(paste0(getwd(), "/dat"), showWarnings = F)
  dir.create(paste0(getwd(), "/aux-dat-internal"), showWarnings = F)
  dir.create(paste0(getwd(), "/aux-dat-external"), showWarnings = F)
  dir.create(paste0(getwd(), "/dat"), showWarnings = F)
  dir.create(paste0(getwd(), "/csv"), showWarnings = F)
  dir.create(paste0(getwd(), "/csv/univariate"), showWarnings = F)
  dir.create(paste0(getwd(), "/csv/bivariate"), showWarnings = F)
  dir.create(paste0(getwd(), "/csv/regression"), showWarnings = F)
  dir.create(paste0(getwd(), "/csv/open-text"), showWarnings = F)

  ## ---- Run setup script ----
  source("r/setup.r")
  
  ## ---- Print welcome message ----
  cat("\nWelcome to your R-Project:", basename(getwd()), "\n")
}
