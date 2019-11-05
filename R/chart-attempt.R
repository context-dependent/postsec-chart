library(tidyverse)

dat <- readxl::read_excel("data/Copy of ontario post-sec education breakdown data.xlsx", sheet = 3) 


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

extrafont::loadfonts()

plot <- dat_long %>% 
  mutate(
    Age = fct_relevel(Age, "25 to 64", after = Inf),
    Sex = Sex %>% str_replace("Both sexes", "All"), 
    total_label = ifelse(total > 1E6, glue::glue("{round(total / 1E6, 1)}M"), glue::glue("{scales::comma(total / 1000)}K")),
    label = glue::glue("{scales::percent(pct, accuracy = 1)} ({total_label}) ")
  ) %>% 
  ggplot(aes(Sex, pct, fill = pse, group = pse)) + 
  geom_col() + 
  geom_text(aes(label = label),
            position = position_stack(), hjust = "right", family = "Segoe UI Semilight") + 
  facet_wrap(~Age, scales = "free_y", ncol = 1) + 
  coord_flip() + 
  bptheme::theme_blueprint() + 
  bptheme::scale_fill_blueprint(discrete = TRUE, guide = guide_legend(reverse = TRUE)) + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), expand = c(0, 0))

ggsave("output/ontario-pse.png",plot = plot, height = 8, width = 7.5, units = "in", dpi = 400, type = "cairo-png")
