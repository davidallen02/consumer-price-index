key <- readRDS("data/key.RDS") %>% dplyr::mutate(variable = security)

monthly_growth <- pamngr::join_sheets(c("cpsfhome",
                             "cpiqfafs",
                             "cpupencm",
                             "cpshge",
                             "cpstnv",
                             "cpstuctr",
                             "cpsctot",
                             "cpumcmdy",
                             "cpshshlt",
                             "cpsstran",
                             "cpumserv")) %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 1) %>%
  dplyr::slice_max(dates, n = 1) %>%
  dplyr::left_join(key, by = "variable") %>%
  dplyr::mutate(
    label = LONG_COMP_NAME %>%
      stringr::str_remove_all("US CPI Urban Consumers ") %>%
      stringr::str_remove_all("US CPI ") %>%
      stringr::str_remove_all(" SA")
  ) %>%
  dplyr::select(-security, -INDX_SOURCE, -LONG_COMP_NAME)
  

weights <- pamngr::join_sheets(c("cpivalbv",
                                 "cpivappa",
                                 "cpivedcc",
                                 "cpivedcs",
                                 "cpivenco",
                                 "cpivense",
                                 "cpivfafh",
                                 "cpivfoah",
                                 "cpivhfsu",
                                 "cpivhhop",
                                 "cpivmcsv",
                                 "cpivmedc",
                                 "cpivnevh",
                                 "cpivopsr",
                                 "cpivotgo",
                                 "cpivrecc",
                                 "cpivrecs",
                                 "cpivshlt",
                                 "cpivtcmf",
                                 "cpivtrss",
                                 "cpivuctk",
                                 "cpivwstc")) %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::slice_max(dates, n = 1) %>%
  dplyr::left_join(key, by = "variable") %>%
  dplyr::mutate(
    label = LONG_COMP_NAME %>%
      stringr::str_remove_all("Relative Importance of ") %>%
      stringr::str_remove_all("%") %>%
      stringr::str_remove_all("NSA")
  ) %>%
  dplyr::select(-LONG_COMP_NAME, -INDX_SOURCE, -security, -dates)

dat <- monthly_growth %>%
  dplyr::left_join(weights, by = "label")