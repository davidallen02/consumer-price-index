devtools::install_github("davidallen02/pamngr", force = TRUE)
library(magrittr)


# Headline Monthly Percent Change ---------------------------------------------------
pamngr::get_data("cpi_indx") %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::mutate(
    m_chg = value %>% 
      subtract(dplyr::lag(value)) %>% 
      divide_by(value) %>%
      multiply_by(100)
  ) %>%
  dplyr::select(c("dates", "m_chg")) %>%
  dplyr::slice_max(dates, n = 36) %>% 
  reshape2::melt(id.vars = "dates") %>%
  pamngr::barplot() %>%
  pamngr::pam.plot(
    plot.title = "Consumer Price Index",
    plot.subtitle = "Monthly Percent Change, Seasonally Adjusted",
    show.legend = FALSE
  ) %>%
  pamngr::ppt_output("monthly-change.png")


# Core Monthly Percent Change -------------------------------------------------------
pamngr::get_data("cpupaxfe") %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::mutate(
    m_chg = value %>% 
      subtract(dplyr::lag(value)) %>% 
      divide_by(value) %>%
      multiply_by(100)
  ) %>%
  dplyr::select(c("dates", "m_chg")) %>%
  dplyr::slice_max(dates, n = 36) %>% 
  reshape2::melt(id.vars = "dates") %>%
  pamngr::barplot() %>%
  pamngr::pam.plot(
    plot.title = "Consumer Price Index",
    plot.subtitle = "Monthly Percent Change in Prices Excluding Food & Energy, Seasonally Adjusted",
    show.legend = FALSE
  ) %>%
  pamngr::ppt_output("core-monthly-change.png")



# Headline, Core Annual Change ------------------------------------------------------

dplyr::left_join(
  x = pamngr::get_data("cpi_indx") %>% reshape2::melt(id.vars = "dates") %>% pamngr::pchange(k = 12),
  y = pamngr::get_data("cpupaxfe") %>% reshape2::melt(id.vars = "dates") %>% pamngr::pchange(k = 12), 
  by = "dates") %>%
  set_colnames(c("dates", "Headline CPI", "Core CPI")) %>%
  dplyr::slice_max(dates, n = 120) %>%
  reshape2::melt(id.vars = "dates") %>%
  pamngr::lineplot() %>%
  pamngr::pam.plot(
    plot.title = "Consumer Price Index",
    plot.subtitle = "Annual Percent Change, Seasonally Adjusted"
  ) %>%
  pamngr::ppt_output("annual-pchange.png")



# Category Monthly Change -----------------------------------------------------------

df1  <- pamngr::get_data("cpsfhome") %>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpsfhome"))
df2  <- pamngr::get_data("cpiqfafs")%>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpiqfafs"))
df3  <- pamngr::get_data("cpupencm")%>% reshape2::melt(id.vars = "dates")%>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpupencm"))
df4  <- pamngr::get_data("cpshge")%>% reshape2::melt(id.vars = "dates")%>% dplyr::select(c("dates","value"))  %>% pamngr::pchange() %>% set_colnames(c("dates", "cpshge"))
df5  <- pamngr::get_data("cpstnv")%>% reshape2::melt(id.vars = "dates")%>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpstnv"))
df6  <- pamngr::get_data("cpstuctr")%>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value"))%>% pamngr::pchange() %>% set_colnames(c("dates", "cpstuctr"))
df7  <- pamngr::get_data("cpsctot")%>% reshape2::melt(id.vars = "dates")%>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpsctot"))
df8  <- pamngr::get_data("cpumcmdy")%>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value"))%>% pamngr::pchange() %>% set_colnames(c("dates", "cpumcmdy"))
df9  <- pamngr::get_data("cpshshlt")%>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value"))  %>% pamngr::pchange() %>% set_colnames(c("dates", "cpshshlt"))
df10 <- pamngr::get_data("cpsstran")%>% reshape2::melt(id.vars = "dates")%>% dplyr::select(c("dates","value")) %>% pamngr::pchange() %>% set_colnames(c("dates", "cpsstran"))
df11 <- pamngr::get_data("cpumserv")%>% reshape2::melt(id.vars = "dates") %>% dplyr::select(c("dates","value"))%>% pamngr::pchange() %>% set_colnames(c("dates", "cpumserv"))


dat <- dplyr::left_join(df1, df2, by = "dates") %>%
  dplyr::left_join(df3,  by = "dates") %>%
  dplyr::left_join(df4,  by =  "dates") %>%
  dplyr::left_join(df5,  by = "dates") %>%
  dplyr::left_join(df6,  by = "dates") %>%
  dplyr::left_join(df7,  by = "dates") %>%
  dplyr::left_join(df8,  by = "dates") %>%
  dplyr::left_join(df9,  by = "dates") %>%
  dplyr::left_join(df10, by = "dates") %>%
  dplyr::left_join(df11, by = "dates") %>%
  tail(1)

period <- dat %>% dplyr::select(dates) %>% dplyr::pull() %>% format ("%B %Y")

cat_names <- readxl::read_excel("data.xlsx", sheet = "key", col_names = c("variable", "full")) %>%
  dplyr::mutate(
    full = full %>% 
      stringr::str_remove_all("US CPI Urban Consumers ") %>%
      stringr::str_remove_all("US CPI ") %>%
      stringr::str_remove(" SA") %>%
      stringr::str_replace_all(" ", "\n")
  )

p <- dat %>%
  reshape2::melt(id.vars = "dates") %>%
  dplyr::left_join(cat_names, by = "variable") %>%
  ggplot2::ggplot(ggplot2::aes(full, value)) +
  ggplot2::geom_bar(stat = "identity", fill = "#850237") 

p %>% pamngr::pam.plot(
  plot.title = "Consumer Price Index",
  plot.subtitle = paste(period, "Monthly Percent Change, Seasonally Adjusted")
) %>%
  pamngr::ppt_output("category_pchange.png")




