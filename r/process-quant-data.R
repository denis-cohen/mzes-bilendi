load("dat/processed-data.rdata")
dat <- dat_quant %>% 
  dplyr::rename(
    personal_occup = V11_1,
    personal_gender = V11_2,
    personal_genderbirth = V11_3, # new
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
  dplyr::mutate(
    personal_gender_incl_open = factor(
      dplyr::case_when(personal_gender == 1 ~ "Female",
                       personal_gender == 2 ~ "Male",
                       personal_gender == 3 ~ "Non-binary or gender-queer",
                       personal_gender == 4 ~ "Agender",
                       personal_gender == 5 ~ V11_2r5oe,
                       personal_gender == 6 ~ "Prefer not to say")
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
      personal_gender %in% c(1,3,4,5) & personal_genderbirth %in% c(1,2) ~ 1,
      # prefer not to say gender or prefer not to say transgender
      personal_gender == 6 | personal_genderbirth == 3 ~ NA_integer_
    )
  ) %>%
  dplyr::mutate(
    # counter diversity characteristics, incl. non-binary
    num_div_incl_nonbin = dplyr::if_else(personal_gender == 3,
                                         num_div + 1L,
                                         num_div),
    # counter diversity characteristics, incl. non-cis male (this was "nonmale" in 2022)
    # non-cis male includes "female" (1), "non-binary or gender-queer" (3), 
    #                       "agender" (4), "other" (5), and "male"+transgender
    num_div_incl_noncis_male = dplyr::if_else(personal_gender_cis_male == 1,
                                              num_div + 1L,
                                              num_div)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(.vars = vars("num_div"),
                   .funs = ~ifelse(personal_refuse == 1, NA_integer_, .)) %>%
  dplyr::mutate_at(.vars = vars("num_div_incl_nonbin"),
                   .funs = ~ifelse(personal_refuse == 1 |
                                     personal_gender == 6, NA_integer_, .)) %>%
  dplyr::mutate_at(.vars = vars("num_div_incl_noncis_male"),
                   .funs = ~ifelse(personal_refuse == 1 |
                                     personal_gender == 6 |
                                     personal_genderbirth == 3, NA_integer_, .)) %>%
  # Cap counters at 3 (should we do this again?)
  #dplyr::mutate_at(.vars = vars(starts_with("num_div")),
  #                 .funs = ~ifelse(. > 3, 3, .)) %>%
  #dplyr::mutate_at(.vars = vars(starts_with("num_div")),
  #                 .funs = ~ factor(., 
  #                                  levels = c(0:3),
  #                                  labels = c(0:2, "3 or more"))) %>%
  
  # Post-hoc filter enforcement
  # V2_2 to V2_8: office space
  dplyr::mutate(
    dplyr::across(
      .cols = c(all_of(c("V2_2", "V2_3", "V2_4", "V2_6", "V2_8")),
                starts_with("V2_7r")),
      .fns = ~ ifelse(V2_1 %in% c(3, 4), NA, .) # !!! check whether conditions is correct !!!
    )
  ) %>%
  # V5_* qualification phase
  dplyr::mutate(
    dplyr::across(
      .cols = c(all_of(c("V5_4", "V5_5", "V5_6", "V5_7", "V5_10")), 
                starts_with("V5_2"),
                starts_with("V5_3"),
                starts_with("V5_8"),
                starts_with("V5_9"),
                starts_with("V5_11"),
                starts_with("V5_12")),
      .fns = ~ ifelse(V5_1 %in% c(2, 3), NA, .) # !!! check whether conditions is correct !!!
    )
  ) %>%
  # V6_* negative experiences
  dplyr::mutate(
    neg_experiences_sum = rowSums(
      dplyr::across(all_of(c("V6_1r1",
                             "V6_1r2",
                             "V6_1r3",
                             "V6_1r4",
                             "V6_1r5"))),
      na.rm = TRUE
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(all_of(c("V6_8", "V6_6", "V6_8", "V6_12")),
                starts_with("V6_2r"),
                starts_with("V6_3r"),
                starts_with("V6_4r")),
      .fns = ~ ifelse(neg_experiences_sum > 0, ., NA) # !!! check whether conditions is correct !!!
    )
  ) %>%
  # V6_* negative experiences, seeking help
  dplyr::mutate(
    dplyr::across(
      .cols = c(starts_with("V6_9r")),
      .fns = ~ ifelse(V6_8 %in% c(1, 3), NA, .) # !!! check whether conditions is correct !!!
    )
  ) %>%
  # V6_* negative experiences, no help
  dplyr::mutate(
    dplyr::across(
      .cols = c(starts_with("V6_11r")),
      .fns = ~ ifelse(V6_8 %in% c(2, 3), NA, .) # !!! check whether conditions is correct !!!
    )
  ) %>%
  # V7_* negative experiences, colleagues
  dplyr::mutate(
    neg_experiences_colleagues_sum = rowSums(
      dplyr::across(all_of(c("V7_1r1",
                             "V7_1r2",
                             "V7_1r3",
                             "V7_1r4"))),
      na.rm = TRUE
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(all_of(c("V7_5", "V7_6")),
                starts_with("V7_2r"),
                starts_with("V7_3r"),
                starts_with("V7_4r")),
      .fns = ~ ifelse(neg_experiences_colleagues_sum > 0, ., NA) # !!! check whether conditions is correct !!!
    )
  )
