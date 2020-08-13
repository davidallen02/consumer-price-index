library(magrittr)


# Headline Monthly Percent Change ---------------------------------------------------
pamngr::get_data("cpi indx") %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::pchange(k = 1) %>%
  dplyr::slice_max(dates, n = 36) %>% 
  pamngr::barplot(fill = "variable") %>%
  pamngr::pam_plot(
    plot_title = "Consumer Price Index",
    plot_subtitle = "Monthly Percent Change, Seasonally Adjusted",
    show_legend = FALSE
  ) %>%
  pamngr::all_output("monthly-change")


# Core Monthly Percent Change -------------------------------------------------------
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
  pamngr::all_output("core-monthly-change")



# Headline, Core Annual Change ------------------------------------------------------

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



# Category Monthly Change -----------------------------------------------------------

dat <- pamngr::join_sheets(c("cpsfhome",
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
  dplyr::slice_max(dates, n = 1)

period <- dat %>% dplyr::select(dates) %>% dplyr::pull() %>% unique() %>% format ("%B %Y")

key <- readRDS("data/key.RDS") 

cat_names <- key %>%
  dplyr::mutate(
    full = LONG_COMP_NAME %>% 
      stringr::str_remove_all("US CPI Urban Consumers ") %>%
      stringr::str_remove_all("US CPI ") %>%
      stringr::str_remove(" SA") %>%
      stringr::str_replace_all(" ", "\n"),
    variable = security
  ) %>%
  dplyr::select(variable, full)

p <- dat %>%
  dplyr::mutate(dates = dates %>% as.character()) %>%
  dplyr::left_join(cat_names, by = "variable") %>%
  pamngr::barplot(x = "full", y = "value", fill = "dates") %>%
  pamngr::pam_plot(
  plot_title = "Consumer Price Index",
  plot_subtitle = paste(period, "Monthly Percent Change, Seasonally Adjusted"),
  show_legend = FALSE
) %>%
  pamngr::all_output("category_monthly_pchange")




