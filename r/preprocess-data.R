# Data import ----
# Load
dat <- rio::import(bilendi_dta_path)


# Filter to (de facto) completes ----
dat_proc <- dat %>%
  dplyr::filter(status == 3 |
                  (status == 4 & !is.na(vdropout) & vdropout >= 71))


# Post-hoc reinforcement of filters ----
## Open-text columns ----
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

## Quantitative items----
dat_proc <- dat_proc %>%
  # V2_* office space
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::any_of(c(
        "V2_2", "V2_6", dplyr::starts_with("V2_7"), "V2_8"
      )),
      .fns = ~ dplyr::if_else(V2_1 == 1, ., NA_integer_)
    ),
    V2_3 = dplyr::if_else(V2_1 == 1 & V2_2 == 2, V2_3, NA_integer_),
    V2_4 = dplyr::if_else(V2_1 != 3, V2_4, NA_integer_)
  ) %>%
  # V5_* qualification phase
  dplyr::mutate(dplyr::across(
    .cols = c(
      all_of(c("V5_4", "V5_5", "V5_6", "V5_7", "V5_10")),
      starts_with("V5_2"),
      starts_with("V5_3"),
      starts_with("V5_8"),
      starts_with("V5_9"),
      -V5_8r6oe
    ),
    .fns = ~ dplyr::if_else(V5_1 == 1, ., NA_integer_)
  )) %>%
  # V6_* negative experiences
  dplyr::mutate(
    neg_experiences_sum = rowSums(dplyr::across(all_of(
      c("V6_1r1", "V6_1r2", "V6_1r3", "V6_1r4", "V6_1r5")
    )), na.rm = TRUE),
    any_neg_experiences = as.integer(V6_1r6 == 1 | V6_1r7 == 1)
  ) %>%
  dplyr::mutate(dplyr::across(
    .cols = c(V6_2r1:V6_4r3, V6_6),
    .fns = ~ dplyr::if_else(any_neg_experiences == 1, ., NA_integer_)
  )) %>%
  # V6_* negative experiences, seeking help
  dplyr::mutate(dplyr::across(
    .cols = c(starts_with("V6_9r")),
    .fns = ~ dplyr::if_else(any_neg_experiences == 1 &
                              V6_8 == 2, ., NA_integer_)
  )) %>%
  # V6_* negative experiences, no help
  dplyr::mutate(dplyr::across(
    .cols = c(starts_with("V6_11r")),
    .fns = ~ dplyr::if_else(any_neg_experiences == 1 &
                              V6_8 == 1, ., NA_integer_)
  )) %>%
  # V7_* negative experiences, colleagues
  dplyr::mutate(any_neg_experiences_colleagues = as.integer(V7_1r5 == 1 |
                                                             V7_1r6 == 1)) %>%
  dplyr::mutate(dplyr::across(
    .cols = c(
      all_of(c("V7_5", "V7_6")),
      starts_with("V7_2r"),
      starts_with("V7_3r"),
      starts_with("V7_4r")
    ),
    .fns = ~ dplyr::if_else(any_neg_experiences_colleagues == 1, ., NA_integer_)
  ))

## Re-apply labels ----
dat_proc <- dat_proc  %>%
  sjlabelled::copy_labels(dat)


# Open-text/character columns ----
## Selection ----
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

## Extract labels ----
labels_qual <- dat_qual %>%
  dplyr::select(dplyr::ends_with("r1")) %>%
  purrr::map_chr(sjlabelled::get_label)



# Numeric/factor columns ----
## Selection ----
dat_quant <- dat_proc %>%
  dplyr::select(dplyr::matches("^V", ignore.case = FALSE)) %>%
  dplyr::select(-dplyr::all_of(names(dat_qual))) %>%
  dplyr::select(V1_1:V12_1r8)


## Extract labels ----
labels_quant <- dat_quant %>%
  purrr::map_chr(sjlabelled::get_label)


## Generated variables ----
dat_quant <- dat_quant %>%
  dplyr::rename(
    personal_occup = V11_1,
    personal_gender = V11_2,
    personal_genderbirth = V11_3,
    # new
    personal_parent = V11_4r1,
    personal_1stgen = V11_4r2,
    personal_lgbtq  = V11_4r3,
    personal_handic = V11_4r4,
    personal_ethnic = V11_4r5,
    personal_relig  = V11_4r6,
    personal_citiz  = V11_4r7,
    personal_lang   = V11_4r8,
    personal_other  = V11_4r9,
    personal_none   = V11_4r10,
    personal_refuse = V11_4r11,
  ) %>%
  # gender variable including open response ("other")
  dplyr::mutate(personal_gender_incl_open = factor(
    dplyr::case_when(
      personal_gender == 1 ~ "Female",
      personal_gender == 2 ~ "Male",
      personal_gender == 3 ~ "Non-binary or gender-queer",
      personal_gender == 4 ~ "Agender",
      personal_gender == 5 ~ V11_2r5oe,
      personal_gender == 6 ~ "Prefer not to say"
    )
  )) %>%
  dplyr::rowwise() %>%
  # Counter diversity characteristics (excl. gender)
  dplyr::mutate(num_div = sum(
    c(
      personal_parent,
      personal_1stgen,
      personal_handic,
      personal_lgbtq,
      personal_ethnic,
      personal_relig,
      personal_citiz,
      personal_lang,
      personal_other # new
    ),
    na.rm = TRUE
  )) %>%
  dplyr::mutate(
    personal_gender_cis_male = dplyr::case_when(
      # Male + transgender
      personal_gender == 2 & personal_genderbirth == 2 ~ 1,
      # Female/Non-binary or gender queer/agender/other + cis/transgender
      personal_gender %in% c(1, 3, 4, 5) &
        personal_genderbirth %in% c(1, 2) ~ 0,
      # prefer not to say gender or prefer not to say transgender
      personal_gender == 6 | personal_genderbirth == 3 ~ NA_integer_
    )
  ) %>%
  dplyr::mutate(
    # counter diversity characteristics, incl. non-binary
    num_div_incl_nonbin = dplyr::if_else(personal_gender == 3, num_div + 1L, num_div),
    # counter diversity characteristics, incl. non-cis male (this was "nonmale" in 2022)
    # non-cis male includes "female" (1), "non-binary or gender-queer" (3),
    #                       "agender" (4), "other" (5), and "male"+transgender
    num_div_incl_noncis_male = dplyr::if_else(personal_gender_cis_male == 1, num_div + 1L, num_div)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(
    .vars = vars("num_div"),
    .funs = ~ dplyr::if_else(personal_refuse == 1, NA_integer_, .)
  ) %>%
  dplyr::mutate_at(
    .vars = vars("num_div_incl_nonbin"),
    .funs = ~ dplyr::if_else(personal_refuse == 1 |
                               personal_gender == 6, NA_integer_, .)
  ) %>%
  dplyr::mutate_at(
    .vars = vars("num_div_incl_noncis_male"),
    .funs = ~ dplyr::if_else(
      personal_refuse == 1 |
        personal_gender == 6 |
        personal_genderbirth == 3,
      NA_integer_,
      .
    )
  ) %>%
  # Cap counters at 3
  dplyr::mutate_at(.vars = vars(starts_with("num_div")),
                   .funs = list(capped = ~ dplyr::if_else(. > 3, 3, .))) %>%
  dplyr::mutate_at(
    .vars = vars(starts_with("num_div") & ends_with("capped")),
    .funs = ~ factor(., levels = c(0:3), labels = c(0:2, "3 or more"))
  )

# ---- Save ----
save(dat_proc, dat_qual, dat_quant, labels_qual, labels_quant, file = "dat/processed-data.rdata")
