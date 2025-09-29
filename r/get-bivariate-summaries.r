# Import pre-processed data ----
load("dat/processed-data.rdata")

## ---- Generate tables ----
## Tabulate and append
main_quantitative_vars <- dat_quant %>%
  dplyr::select(matches("^V[0-9]"),
                neg_experiences_sum,
                any_neg_experiences,
                any_neg_experiences_colleagues,
                -c(V5_8r6oe, V11_2r5oe)) %>%
  names()

continuous_vars <- c("V1_1",
                     "V1_2")

# Conditioning variables that meet the N>=5 criterion
conditioning_vars <- c(
  "personal_occup_condition",
  "personal_gender_cis_male",
  "personal_parent",
  "personal_1stgen",
  "personal_lgbtq",
  "personal_handic",
  "personal_ethnic",
  "personal_citiz",
  "personal_length_employ", # 2x NA, problem?
  "neg_experiences_sum",
  "any_neg_experiences",
  "any_neg_experiences_colleagues",
  "num_div_capped",
  "num_div_incl_nonbin_capped",
  "V5_1"
)

bivariate_summary <- tidyr::tibble()
for (var in main_quantitative_vars) {
  for (group in conditioning_vars) {
    if (!(startsWith(var, "self") &
          startsWith(group, "self")) &
        # no conditioning by itself
        var != group) {
      frequency_distribution <- dat_quant %>%
        dplyr::rename(value = var,
                      subgroup = group) %>%
        dplyr::select(value, subgroup) %>%
        dplyr::mutate(subgroup = as.character(subgroup)) %>% # new
        dplyr::group_by(subgroup, value, .drop = FALSE) %>%
        dplyr::tally() %>%
        dplyr::rename(freq = n) %>%
        dplyr::group_by(subgroup, .drop = FALSE) %>%
        dplyr::mutate(subgroup_size = sum(freq)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(var = var,
                      conditioning_var = group) %>%
        dplyr::select(var,
                      conditioning_var,
                      subgroup,
                      subgroup_size,
                      value,
                      freq)
      
      if (var %in% continuous_vars) {
        frequency_distribution <- frequency_distribution %>%
          dplyr::left_join(
            dat_quant %>%
              dplyr::rename(value = var,
                            subgroup = group) %>%
              dplyr::select(value, subgroup) %>%
              dplyr::mutate(subgroup = as.character(subgroup)) %>% # new
              dplyr::group_by(subgroup, .drop = FALSE) %>%
              dplyr::mutate(
                value_num = dplyr::if_else(
                  is.na(value) | value %in% c("Not applicable"),
                  NA_integer_,
                  as.integer(value) - 1L
                )
              ) %>%
              dplyr::summarize(
                mean = mean(value_num, na.rm = TRUE),
                std_dev = sd(value_num, na.rm = TRUE),
                median = median(value_num, na.rm = TRUE),
                perc25 = quantile(value_num, probs = .25, na.rm = TRUE),
                perc75 = quantile(value_num, probs = .75, na.rm = TRUE)
              ),
            by = "subgroup"
          )
      } else {
        frequency_distribution <- frequency_distribution %>%
          dplyr::group_by(subgroup, .drop = FALSE) %>%
          dplyr::mutate(
            mean = freq / sum(freq),
            std_dev = NA_real_,
            median = NA_real_,
            perc25 = NA_real_,
            perc75 = NA_real_
          )
      }
      
      ## Enforce minimum subgroup size
      frequency_distribution <- frequency_distribution %>%
        dplyr::mutate_at(
          .vars = vars(freq, mean, std_dev, median, perc25, perc75),
          .funs = ~ ifelse(subgroup_size < 5, NA, .)
        )
      
      ## Append
      bivariate_summary <- dplyr::bind_rows(bivariate_summary,
                                            frequency_distribution)
    }
  }
}

## Collapse information for subgroups with N < 5
bivariate_summary <- bivariate_summary %>%
  dplyr::mutate(
    value = as.character(value),
    value = ifelse(subgroup_size < 5, "Subgroup too small", value)
  ) %>%
  distinct()

## Add question text
labels_tbl <- tibble::tibble(
  var = names(dat_quant),
  question = sjlabelled::get_label(dat_quant)
)

bivariate_summary <- bivariate_summary %>%
  dplyr::left_join(labels_tbl, by = "var")


bivariate_summary %>%
  readr::write_excel_csv("csv/bivariate/bivariate-summary.csv")
