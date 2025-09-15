if (!("pacman" %in% utils::installed.packages()))
  install.packages("pacman")

pacman::p_load(
  dplyr,
  rmarkdown,
  here,
  ggplot2,
  viridisLite,
  tidyr,
  rio,
  sjlabelled,
  sjmisc
)
