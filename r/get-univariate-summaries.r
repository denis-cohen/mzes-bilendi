# Import pre-processed data ----
load("dat/processed-data.rdata")

# Post-processing ----
## Deselect open-ended field ----
dat_quant <- dat_quant %>%
  dplyr::select(-dplyr::contains("oe"))

## Recode refusal/don't know to NA ----
dat_quant <- dat_quant %>%
  dplyr::mutate(
    dplyr::across(
      c(V5_6, V5_7, V5_10),
      ~sjlabelled::set_na(., na = "6 = 'Don't know")
    )
  )

## Identify continuous and factor variables ----
quantitative_vars <- names(dat_quant)
continuous_vars <- c("V1_1",
                     "V1_2",
                     "V5_4",
                     "V5_6",
                     "V5_7",
                     "V5_10")
categorical_vars <- quantitative_vars[!(quantitative_vars %in% continuous_vars)]


# Generate tables ----
# Tabulate and append
univariate_summary <- tidyr::tibble()
for (var in quantitative_vars) {
  question <- sjlabelled::get_label(dat_quant[[var]])
  frequency_distribution <- dat_quant %>%
    dplyr::select(dplyr::all_of(var)) %>%
    dplyr::mutate(label = sjlabelled::to_label(!!as.name(var))) %>%
    dplyr::count(!!as.name(var), label) %>%
    dplyr::rename(value = !!as.name(var),
                  freq = n) %>%
    dplyr::mutate(var = var,
                  question = question) %>%
    dplyr::relocate(var, question, .before = value) %>%
    dplyr::mutate(label = as.character(label))
  
  if (!is.numeric(frequency_distribution$value)) {
    frequency_distribution <- frequency_distribution %>%
      dplyr::mutate(value = NA_integer_)
  }
  
  if (var %in% continuous_vars) {
    frequency_distribution <- dplyr::bind_cols(
      frequency_distribution,
      dat_quant %>%
        dplyr::summarize(
          mean = mean(!!as.name(var), na.rm = TRUE),
          std_dev = sd(!!as.name(var), na.rm = TRUE),
          median = median(!!as.name(var), na.rm = TRUE),
          perc25 = quantile(!!as.name(var), probs = .25, na.rm = TRUE),
          perc75 = quantile(!!as.name(var), probs = .75, na.rm = TRUE)
        )
    )
  } else {
    frequency_distribution <- frequency_distribution %>%
      mutate(
        mean = freq / sum(freq),
        std_dev = NA_real_,
        median = NA_real_,
        perc25 = NA_real_,
        perc75 = NA_real_
      )
  }
  
  univariate_summary <- dplyr::bind_rows(univariate_summary,
                                         frequency_distribution)
}

# Export
univariate_summary %>%
  readr::write_excel_csv("csv/univariate/univariate-summary.csv")
