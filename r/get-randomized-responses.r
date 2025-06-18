## ---- Load pre-processed data ----
load("dat/processed-data.rdata")

## ---- Post-processing of text questions for manual deanonymization ----
# Desired output:
#   - All valid free-text responses
#   - Without any personal identifying information
#   - randomized within question groups to avoid sequencing and clustering

## Wide-to-long, drop empty cells
responses <- dat_qual %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  tidyr::pivot_longer(
    cols = dplyr::matches("r[1-8]$"),
    names_to = c("question", "response"),
    names_pattern = "(.*)r([1-8])",
    values_to = "value"
  ) %>%
  dplyr::mutate(value = dplyr::if_else(nchar(value) < 2, NA_character_, value)) %>%
  tidyr::drop_na(value)

## Randomization, arrangement, output
responses <- responses %>%
  dplyr::arrange(question, id, response) %>%
  dplyr::select(-id, -response) %>%
  dplyr::group_by(question) %>%
  dplyr::mutate(pos = sample(seq_len(dplyr::n()),
                             size = dplyr::n(),
                             replace = FALSE)) %>%
  dplyr::arrange(question, pos) %>%
  dplyr::select(-pos) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response_num = dplyr::row_number()) %>%
  # dplyr::left_join(
  #   question_texts,
  #   by = c("var" = "name")
  # ) %>%
  dplyr::select(response_num,
                question,
                value) %>%
  dplyr::mutate(
    anonymized_by_respondi = NA,
    flagged_by_respondi = NA
  )

## ---- Export as CSV ----
## Pre-anonymization (for reference)
responses %>%
  readr::write_excel_csv("csv/open-text/pre-anonymization.csv")

## Version for manual anonymization
if (file.exists("csv/open-text/post-anonymization.csv")) {
  stop(
    paste0(
      'The file csv/open-text/post-anonymization.csv ',
      'already exists. It will not be overwritten as potential ',
      ' manual edits would be lost. ',
      'If you want to replace the file, please overwrite it ',
      'manually with a copy of csv/open-text/pre-anonymization.csv.'
    )
  )
} else {
  responses %>%
    readr::write_excel_csv("csv/open-text/post-anonymization.csv")
}
