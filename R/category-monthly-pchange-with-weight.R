key <- readRDS("data/key.RDS") %>% dplyr::mutate(variable = security)

monthly_growth <- pamngr::join_sheets(c("cpsffood",
                             "cpupener",
                             "cpupcxfe",
                             "cpupsxen")) %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 1) %>%
  dplyr::slice_max(dates, n = 1) %>%
  dplyr::left_join(key, by = "variable") %>%
  dplyr::mutate(
    label = LONG_COMP_NAME %>%
      stringr::str_remove_all("US CPI Urban Consumers ") %>%
      # stringr::str_remove_all("US CPI ") %>%
      stringr::str_remove_all(" SA") %>%
      stringr::str_trim(),
    growth = value
  ) %>%
  dplyr::select(-security, -INDX_SOURCE, -LONG_COMP_NAME, -value, -variable)
  

weights <- pamngr::join_sheets(c("cpivfood",
                                 "cpivener",
                                 "cpivclfe",
                                 "cpivsles")) %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::slice_max(dates, n = 1) %>%
  dplyr::left_join(key, by = "variable") %>%
  dplyr::mutate(
    label = LONG_COMP_NAME %>%
      stringr::str_remove_all("Relative Importance of ") %>%
      stringr::str_remove_all("%") %>%
      stringr::str_remove_all(" Commodities") %>%
      stringr::str_trim(),
    weight = value 
  ) %>%
  dplyr::select(-LONG_COMP_NAME, -INDX_SOURCE, -security, -dates, -value, -variable)

dat <- monthly_growth %>%
  dplyr::left_join(weights, by = "label") 

period <- dat %>% dplyr::select(dates) %>% dplyr::pull() %>% unique() %>% format ("%B %Y")

p <- dat %>%
  ggplot2::ggplot(ggplot2::aes(x = label, y = growth, size = weight)) +
  ggplot2::geom_point(color = "#850237") +
  ggplot2::scale_size(range = c(5, 15))

p <- p %>% 
  pamngr::pam_plot(
    plot_title = "Consumer Price Index",
    plot_subtitle = paste(period, "Monthly Percent Change, Seasonally Adjusted")
  )

p %>% pamngr::all_output("category-monthly-pchange-with-weight")