pamngr::get_data("cpupaxfe") %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 1) %>%
  dplyr::slice_max(dates, n = 36) %>% 
  pamngr::barplot(fill = "variable") %>%
  pamngr::pam_plot(
    plot_title = "Consumer Price Index",
    plot_subtitle = "Monthly Percent Change in Prices Excluding Food & Energy, Seasonally Adjusted",
    show_legend = FALSE
  ) %>%
  pamngr::all_output("core-monthly-pchange")