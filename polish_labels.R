library(tidyverse)
library(marginaleffects)
library(ggplot2)
library(patchwork)
library(insight)

data_limited <- readRDS("data_limited.rds") |>
  mutate(across(where(is.factor), droplevels))

# Tlumaczymy allignment RAZ, na starcie -- dzieki temu wszystkie kolejne
# scale_color_manual(), legendy i osie X automatycznie dostaja spojne
# polskie etykiety, bez ryzyka niedopasowania nazw.
data_limited <- data_limited |>
  mutate(allignment = factor(allignment,
                             levels = c("Not aligned", "Aligned"),
                             labels = c("Niezgodni", "Zgodni")))


theme_small <- theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# -- Models ---------------------------------------------------
m_allignment <- glm(
  KID_1_dummy ~ edu + marital_status + gen_health_1 + allignment,
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_age <- glm(
  KID_1_dummy ~ edu + marital_status + gen_health_1 + allignment * age_group,
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_edu <- glm(
  KID_1_dummy ~ marital_status + gen_health_1 + allignment * edu,
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_support <- glm(
  KID_1_dummy ~ edu + marital_status + gen_health_1 + allignment * political_category_1,
  family = binomial(link = "cloglog"), data = data_limited
)

# -- Custom 5-year age groups ---------------------------------
# UWAGA: jesli data_limited zawiera wiek spoza docelowego zakresu
# (np. dzieci/osoby starsze), przefiltruj wiersze PRZED droplevels(),
# np.: data_limited <- data_limited |> filter(age >= 15, age <= 49)
data_limited$age_group_1 <- cut(
  data_limited$age,
  breaks = seq(0, 100, by = 5),
  labels = paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-"),
  right = FALSE
)

data_limited <- data_limited |>
  mutate(age_group_1 = droplevels(age_group_1))

# sanity check
levels(data_limited$age_group_1)
table(data_limited$age_group_1)

# refit modelu na oczyszczonych danych
m_allignment_age_1 <- glm(
  as.formula(paste("KID_1_dummy ~", "edu + marital_status + gen_health_1 +",
                   "+ allignment * age_group_1")),
  family = binomial(link = "cloglog"), data = data_limited
)

summary(m_allignment_age_1)

# tylko grupy wieku z danymi
age_levels_used <- names(table(data_limited$age_group_1)[table(data_limited$age_group_1) > 0])

preds_age_1 <- avg_predictions(
  m_allignment_age_1,
  variables  = list(allignment  = levels(data_limited$allignment),
                    age_group_1 = age_levels_used),
  conf_level = 0.83
) |> as.data.frame()

p_age_1 <- ggplot(preds_age_1,
                  aes(x = age_group_1, y = estimate,
                      ymin = conf.low, ymax = conf.high,
                      color = allignment)) +
  geom_pointrange(linewidth = 1.2, fatten = 3,
                  position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.15, linewidth = 1.0,
                position = position_dodge(width = 0.5)) +
  ylim(-0.001, 0.012) +
  labs(x = "", y = "Przewidywane prawdopodobieństwo",
       title = "Zgodność × Grupa wieku",
       color = "Zgodność") +
  scale_color_brewer(palette = "Set1") +
  theme_small

p_age_1

# -- Predictions: base alignment -------------------------------
preds_base <- avg_predictions(
  m_allignment,
  variables  = "allignment",
  conf_level = 0.83
) |> as.data.frame()

p_base <- ggplot(preds_base,
                 aes(x = allignment, y = estimate,
                     ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(linewidth = 1.2, fatten = 3) +
  geom_errorbar(width = 0.15, linewidth = 1.0) +
  ylim(-0.001, 0.012) +
  labs(x = "", y = "Przewidywane prawdopodobieństwo",
       title = "Ogólna zgodność") +
  theme_small

# -- Predictions: alignment x age group -------------------------
preds_age <- avg_predictions(
  m_allignment_age,
  variables  = list(allignment = levels(data_limited$allignment),
                    age_group  = levels(data_limited$age_group)),
  conf_level = 0.83
) |> as.data.frame()

p_age <- ggplot(preds_age,
                aes(x = age_group, y = estimate,
                    ymin = conf.low, ymax = conf.high,
                    color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Niezgodni" = "#CC0000",
                                "Zgodni"     = "#004180"),
                     name = "Zgodność") +
  ylim(-0.001, 0.015) +
  labs(x = "", y = "Przewidywane prawdopodobieństwo",
       title = "Zgodność × Grupa wieku") +
  theme_small +
  theme(legend.position = "bottom")

# -- Predictions: alignment x education --------------------------
preds_edu <- avg_predictions(
  m_allignment_edu,
  variables  = list(allignment = levels(data_limited$allignment),
                    edu        = levels(data_limited$edu)),
  conf_level = 0.83
) |>
  as.data.frame() |>
  mutate(edu_label = factor(
    ifelse(edu == "1", "niskie/średnie", "wysokie"),
    levels = c("niskie/średnie", "wysokie")
  ))

preds_support <- avg_predictions(
  m_allignment_support,
  newdata   = data_limited,
  variables = list(
    allignment            = levels(data_limited$allignment),
    political_category_1  = levels(data_limited$political_category_1)
  ),
  conf_level = 0.83
) |>
  as.data.frame()

p_edu <- ggplot(preds_edu,
                aes(x = edu_label, y = estimate,
                    ymin = conf.low, ymax = conf.high,
                    color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Niezgodni" = "#CC0000",
                                "Zgodni"     = "#004180"),
                     name = "Zgodność") +
  ylim(-0.001, 0.015) +
  labs(x = "", y = "Przewidywane prawdopodobieństwo",
       title = "Zgodność × Edukacja") +
  theme_small +
  theme(legend.position = "bottom")

# Diagnostyka: pokaz w konsoli, jakie wartosci faktycznie ma ta zmienna,
# zanim probujemy je przetlumaczyc -- pozwala od razu zobaczyc, czy ponizszy
# recode() cokolwiek trafia.
cat("Rzeczywiste poziomy political_category_1:\n")
print(levels(preds_support$political_category_1))

# case_when zamiast factor(levels=,labels=): jesli jakas wartosc NIE pasuje
# do ponizszych warunkow, zostaje pokazana w oryginalnej postaci zamiast
# zamienic sie w NA (co dzialo sie poprzednio przy "Right-wing"/"Left-wing").
preds_support <- preds_support |>
  mutate(political_category_1 = case_when(
    political_category_1 == "1" ~ "Partia \n Konserwatywna",
    political_category_1 == "2" ~ "Partia \n Lewicowa",
    TRUE ~ as.character(political_category_1)
  ))

p_support <- ggplot(preds_support,
                    aes(x = political_category_1, y = estimate,
                        ymin = conf.low, ymax = conf.high,
                        color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Niezgodni" = "#CC0000",
                                "Zgodni"     = "#004180"),
                     name = "Zgodność") +
  ylim(-0.001, 0.015) +
  labs(x = "", y = "Przewidywane prawdopodobieństwo",
       title = "Zgodność × Popierana partia") +
  theme_small +
  theme(legend.position = "bottom")

# -- Patchwork --------------------------------------------------
poster_theme <- theme_bw(base_size = 20) +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    axis.title    = element_text(size = 20),
    axis.text     = element_text(size = 20),
    legend.text   = element_text(size = 20),
    legend.title  = element_text(size = 20),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 20)
  )

p_all <- ((p_base + poster_theme | p_support + poster_theme) /
            (p_age_1 + poster_theme | p_edu + poster_theme)) +
  plot_annotation(
    # title      = "Zgodność polityczna a dzietność",
    subtitle   = "Średnie przewidywane prawdopodobieństwa (83% CI)",
    tag_levels = "A",
    theme      = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13)
    )
  ) +
  theme(legend.position = "bottom")

p_all

ggsave("plot_alignment_all.png", p_all,
       width = 14, height = 16, dpi = 300)