library(tidyverse)
library(lubridate)
library(janitor)
library(gganimate)
library(rcartocolor)

cases_raw <- read_csv("./time_series_covid19_confirmed_global.csv")
cases_data <- cases_raw %>%
  pivot_longer(cols= matches("\\d+/\\d+/\\d+"), 
               names_to = "date", 
               values_to = "cases") %>% 
  clean_names() %>% 
  transmute(country_region, date = mdy(date), cases) %>% 
  filter(!country_region %in% c("Others" ,"China","Diamond Princess")) %>%
  group_by(country_region, date) %>% 
  summarise(cases = sum(cases))

cases_data <- cases_data %>% 
  filter(cases >= 150)%>%
  filter(n() >= 10) %>%
  arrange(date, .by_group = TRUE)%>%
  mutate(days_since_150 = (date-first(date)) / ddays(1)) %>%
  ungroup()


cases_data <- cases_data %>% 
  bind_rows(
    tibble(country_region = "33% daily rise", 
           days_since_150 = 0:28) %>%
      mutate(cases = 150*1.33^days_since_150)
  ) %>% 
  mutate(line_type = ifelse(country_region == "33% daily rise", "2", "1")) 

 

cases_data <- cases_data %>% 
  mutate(country_region = fct_infreq(country_region))


 

annotations <- cases_data %>% 
  group_by(country_region) %>% 
  filter(days_since_150 == max(days_since_150)) %>% 
  mutate(label_country = ifelse(country_region %in% levels(country_region)[1:12],
                                as.character(country_region), ""))

 

cases_data <- cases_data %>% 
  mutate(color_label = forcats::fct_collapse(country_region,
                                             "#D63D32" = "Switzerland",
                                             "#888888" = "Italy",
                                             "#6699CC" = "Iran",
                                             "#661100" = "Germany",
                                             "#882255" = "France",
                                             "#999933" = "United Kingdom",
                                             "#44AA99" = "US",
                                             "#332288" = "Spain",
                                             "#117733" = "Korea, South",
                                             "#DDCC77" = "Netherlands",
                                             "#1D6996" = "Singapore",
                                             "#855C75" = "Japan",
                                             "black"   = "33% daily rise",
                                             other_level = "grey90"),
         color_label = fct_relevel(color_label, "grey90")) %>%
  arrange(color_label) %>%
  mutate(country_region = fct_inorder(country_region)) %>% 
  mutate(country_label = ifelse(color_label == "grey90", "", as.character(country_region)))



annotations <- cases_data %>% 
  group_by(country_region) %>% 
  filter(days_since_150 == max(days_since_150))


 

p=ggplot(data = cases_data, 
       mapping = aes(x = days_since_150, 
                     y = cases, 
                     color = color_label,
                     group = country_region)) +
  geom_line(mapping = aes(linetype = line_type), 
            size = 0.7, alpha =0.9) +
  scale_x_continuous(limits = c(0, max(cases_data$days_since_150) +6))+
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(150, 500, 2000,  10000, 60000)) +
  theme_minimal() +
  scale_color_identity()+
  # we change the data to cases_data
  shadowtext::geom_shadowtext(data = cases_data, 
                              mapping = aes(x = days_since_150, 
                                            y = cases, 
                                            label = country_label),
                              hjust=-0.1, vjust = 0, bg.color = "white") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm"),
    plot.caption = ggtext::element_markdown()
  )+
  coord_cartesian(clip = "off") +
  labs(x = "Number of days since 150th case", 
       y = "Total Number of Cases",
       title = "Total number of COVID-19 cases",
       subtitle =  "Outside of China")+#,
      # caption = "<span style = 'font-size:8pt;color:#888888'>Data Source: Local Governments <br> John Hopkins University,<br> World Health Organization </span>")+ 
transition_reveal(days_since_150) 


animate(p, height = 300, width = 350)
anim_save("../figure/covid.gif")
