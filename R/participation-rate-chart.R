library(tidyverse)

# Load raw data, downloaded from statcan in the database loader format

dat <- read_csv("data/ontario-participation-rate-by-sex-and-educ.csv")

# clean names to make autocomplete easier

dat <- dat %>% 
  
  janitor::clean_names()


# select key variables and pivot such that population and participation 
# rate are both individual variables

dat <- dat %>% 
  
  select(
    year = ref_date, 
    province = geo, 
    education = educational_attainment, 
    name = labour_force_characteristics, 
    value = value, 
    sex
  ) %>% 
  
  pivot_wider(
    names_from = "name", 
    values_from = "value"
  ) %>% 
  
  janitor::clean_names()

# create post_secondary variable to collapse education levels on 
# combine post_secondary and sex into one variable

dat <- dat %>%
  
  mutate(
    post_secondary = case_when(
      education %>% str_detect("Postsecondary|University") ~ "PSE", 
      TRUE ~ "Non-PSE"
    ), 
    pse_sex = glue::glue("{post_secondary} ({sex})")
  )

# summarize participation_rate by pse_sex and year

dplt <- dat %>% 
  
  group_by(
    pse_sex, 
    year
  ) %>% 
  
  summarize(
    participation_rate = weighted.mean(participation_rate, w = population)
  )


plot <- dplt %>% 
  
  # initialize ggplot
  ggplot(
    # the group aesthetic is important for line charts
    aes(year, participation_rate, group = pse_sex)
  ) + 
  
  # add geom_line
  geom_line(aes(color = pse_sex), lwd = 1)  + 
  
  # chart aesthetics
  bptheme::theme_blueprint() + 
  scale_x_continuous(limits = c(1990, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018)) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) + 
  scale_color_viridis_d() + 
  
  # chart labels
  labs(
    title = "Participation Rates by Sex and PSE Attainment",
    subtitle = "Labour force participation rate for Ontarians aged 25 to 54", 
    y = NULL, 
    caption = "Statistics Canada Table 14-10-0020-01"
  )

# save chart as png impage
ggsave("output/participation-rates.png", plot = plot, height = 5, width = 8, units = "in", dpi = 400, type = "cairo-png")

