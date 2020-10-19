dat <- pamngr::get_data("cpi indx") %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 1) %>%
  dplyr::slice_max(dates, n = 36)

p <- dat %>%
  pamngr::barplot(fill = "variable") %>%
  pamngr::pam_plot(
    plot_title = "Consumer Price Index",
    plot_subtitle = "Monthly Percent Change, Seasonally Adjusted",
    show_legend = FALSE
  )

p %>% pamngr::all_output("headline-monthly-pchange")
