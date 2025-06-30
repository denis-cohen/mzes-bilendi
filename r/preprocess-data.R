## Data import ----
## Load
dat <- rio::import(bilendi_dta_path)

## Filter to (de facto) completes
dat_proc <- dat %>%
  dplyr::filter(status == 3 | (status == 4 & !is.na(vdropout) & vdropout >= 71))


## Open-text/character columns ----
## Post-hoc reinforcement of filters
dat_proc <- dat %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("V2_5"),
      ~ dplyr::if_else(V2_1 %in% 1:2, .x, NA_character_)
    ),
    dplyr::across(
      dplyr::starts_with("V5_11"),
      ~ dplyr::if_else(V5_1 == 1, .x, NA_character_)
    ),
    dplyr::across(
      dplyr::starts_with("V5_12"),
      ~ dplyr::if_else(V5_1 == 1, .x, NA_character_)
    ),
    dplyr::across(
      dplyr::starts_with("V6_5"),
      ~ dplyr::if_else(V6_1r6 == 0 & V6_1r7 == 0, .x, NA_character_)
    ),
    dplyr::across(
      dplyr::starts_with("V6_7"),
      ~ dplyr::if_else(V6_1r6 == 0 &
                         V6_1r7 == 0 & V6_6 == 2, .x, NA_character_)
    ),
    dplyr::across(
      dplyr::starts_with("V6_10"),
      ~ dplyr::if_else(
        V6_1r6 == 0 & V6_1r7 == 0 & V6_8 == 2 &
          (
            V6_9r1 == 1 |
              V6_9r2 == 1 |
              V6_9r3 == 1 | V6_9r4 == 1 | V6_9r5 == 1 | V6_9r6 == 1
          ),
        .x,
        NA_character_
      )
    ),
    dplyr::across(
      dplyr::starts_with("V8_1"),
      ~ dplyr::if_else((V7_1r5 == 0 &
                          V7_1r6 == 0) |
                         (V6_1r6 == 0 & V6_1r7 == 0),
                       .x,
                       NA_character_
      )
    ),
    dplyr::across(
      dplyr::starts_with("V8_2"),
      ~ dplyr::if_else((V7_1r5 == 0 &
                          V7_1r6 == 0) |
                         (V6_1r6 == 0 & V6_1r7 == 0),
                       .x,
                       NA_character_
      )
    )
  )

## Re-apply labels
dat_proc <- dat_proc  %>%
  sjlabelled::copy_labels(dat)

## Selection
dat_qual <- dat_proc %>%
  dplyr::select(
    # Working conditions hampering
    dplyr::starts_with("V1_3"),
    # Improve work atmosphere
    dplyr::starts_with("V1_4"),
    # Location of office (filter: V2_1 %in% 1:2)
    dplyr::starts_with("V2_5"),
    # Anything else about working space
    dplyr::starts_with("V2_12"),
    # Wishes research lab
    dplyr::starts_with("V3_1"),
    # Prepare for career (filter: V5_1 == 1)
    dplyr::starts_with("V5_11"),
    # Anything else about careers (filter: V5_1 == 1)
    dplyr::starts_with("V5_12"),
    # Anonymised description of incident (filter: V6_1r6 == 0 & V6_1r7 == 0)
    dplyr::starts_with("V6_5"),
    # Negative consequences how (filter: V6_1r6 == 0 & V6_1r7 == 0 & V6_6 == 2)
    dplyr::starts_with("V6_7"),
    # Experience in seeking help (filter: V6_1r6 == 0 & V6_1r7 == 0 & V6_8 == 2 & (V6_9r1 == 1 | V6_9r2 == 1 | V6_9r3 == 1 | V6_9r4 == 1 | V6_9r5 == 1 |  V6_9r6 == 1))
    dplyr::starts_with("V6_10"),
    # What kind of support most helpful (filter: V7_1r5 == 0 & V7_1r6 == 0 | V6_1r6 == 0 & V6_1r7 == 0)
    dplyr::starts_with("V8_1"),
    # Preventive measures (filter: V7_1r5 == 0 & V7_1r6 == 0 | V6_1r6 == 0 & V6_1r7 == 0)
    dplyr::starts_with("V8_2"),
    # Structural disadvantages: workplace conditions
    dplyr::starts_with("V9_1"),
    # Suggestions for staff groups
    dplyr::starts_with("V10_"),
    # Final comments/questions/suggestions
    dplyr::starts_with("V12_1")
  )

## Extract labels
labels_qual <- dat_qual %>%
  dplyr::select(dplyr::ends_with("r1")) %>%
  purrr::map_chr(sjlabelled::get_label)


## Numeric/factor columns ----
dat_quant <- dat_proc %>%
  dplyr::select(dplyr::matches("^V", ignore.case = FALSE)) %>%
  dplyr::select(-dplyr::all_of(names(dat_qual)))

## Extract labels
labels_quant <- dat_quant %>%
  purrr::map_chr(sjlabelled::get_label)

## ---- Save ----
save(dat_proc, dat_qual, dat_quant, labels_qual, labels_quant, file = "dat/processed-data.rdata")
