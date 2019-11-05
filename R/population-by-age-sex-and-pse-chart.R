library(tidyverse)

# Load data from excel workbook
dat <- readxl::read_excel("data/Copy of ontario post-sec education breakdown data.xlsx", sheet = 3) 

# Reshape data so that pse, total, and pct are variables
dat_long <- dat %>% 
  
  pivot_longer(
    cols = c(matches("total|pct"))
  ) %>% 
  mutate(
    pse = name %>% str_remove("_pct|_total"), 
    var = name %>% str_extract("pct|total")
  ) %>% 
  select(
    -name
  ) %>% 
  pivot_wider(
    names_from = "var", 
    values_from = "value"
  )

dat_clean <- dat_long %>% 
  mutate(
    # Relevel age variable so that 25 to 64 is on the bottom of the chart
    Age = fct_relevel(Age, "25 to 64", after = Inf),
    # Change "Both sexes" to "All"
    Sex = Sex %>% str_replace("Both sexes", "All"),
    # Use glue::glue to create labels with literate placeholder strings
    total_label = ifelse(total > 1E6, glue::glue("{round(total / 1E6, 1)}M"), glue::glue("{scales::comma(total / 1000)}K")),
    label = glue::glue("{scales::percent(pct, accuracy = 1)} ({total_label}) ")
  )

plot <- dat_clean %>% 
  
  # Initialize ggplot with x = Sex and y = pct
  ggplot(aes(Sex, pct, fill = pse, group = pse)) + 
  geom_col() + 
  # Add labels as a text geometry layer
  geom_text(
    aes(label = label),
    position = position_stack(), 
    hjust = "right", 
    family = "Segoe UI Semilight"
  ) + 
  # Facet on age
  facet_wrap(~Age, scales = "free_y", ncol = 1) + 
  
  # Flip coordinates on their side so the y axis is tracked left to right
  coord_flip() + 
  
  # Add theme elements 
  bptheme::theme_blueprint() + 
  bptheme::scale_fill_blueprint(discrete = TRUE, guide = guide_legend(reverse = TRUE)) + 
  
  # delete unnecessary labels
  labs(x = NULL, y = NULL, fill = NULL) + 
  
  # Add a percentage scale for the y axis
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), expand = c(0, 0))

# save plot as a png
ggsave("output/ontario-pse.png",plot = plot, height = 8, width = 7.5, units = "in", dpi = 400, type = "cairo-png")
