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

## Number of answered questions per respondents
responses %>%
  dplyr::group_by(question) %>%
  dplyr::summarize(num_respondents = length(unique(id)),
                   num_responses = dplyr::n()) %>%
  dplyr::ungroup() %>%
  readr::write_csv("csv/open-text/open-text-summaries.csv")