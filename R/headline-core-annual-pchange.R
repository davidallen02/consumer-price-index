pamngr::join_sheets(c("cpi indx", "cpupaxfe")) %>%
  set_colnames(c("dates", "Headline CPI", "Core CPI")) %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 12) %>%
  dplyr::slice_max(dates, n = 72) %>%
  pamngr::lineplot() %>%
  pamngr::pam_plot(
    plot_title = "Consumer Price Index",
    plot_subtitle = "Annual Percent Change, Seasonally Adjusted"
  ) %>%
  pamngr::all_output("annual-pchange")