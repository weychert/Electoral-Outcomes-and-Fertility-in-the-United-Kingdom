# ============================================================
# 00  Setup
# ============================================================
source("00_setting_work_space.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(patchwork)

theme_small <- theme(
  axis.text        = element_text(face = "bold", color = "black", size = 11),
  axis.text.x      = element_text(face = "bold", color = "black", size = 11),
  axis.text.y      = element_text(face = "bold", color = "black", size = 11),
  axis.title.x     = element_text(face = "bold", color = "black", size = 11),
  axis.title.y     = element_text(face = "bold", color = "black", size = 11),
  plot.title       = element_text(face = "bold", size = 13, color = "black"),
  strip.text       = element_text(face = "bold", size = 12),
  panel.grid.major = element_line(color = "grey90"),
  panel.background = element_rect(fill = "white", color = NA)
)

# Shared Y-axis across ALL FOUR panels -- same scale everywhere so
# the same height means the same probability in every panel.
# y_break_step is the ONE place to change gridline spacing (e.g. 0.002
# for fewer lines); y_limits_common covers the max range needed (panels
# B/D reach 0.015, so all four panels use that range).
y_break_step    <- 0.001
y_limits_common <- c(-0.001, 0.015)
y_breaks_common <- seq(0, 0.015, by = y_break_step)

# ============================================================
# 01  Load & filter
# ============================================================
data <- readRDS("output/1_may_data_split_5.rds") |>
  filter(!is.na(KID_1_dummy)) |>
  filter(!is.na(interview_date))

# ============================================================
# 02  Recode variables
# ============================================================
data <- data |>
  mutate(
    political_category_1 = sjlabelled::as_character(political_category_1),
    political_category_1 = case_when(
      political_category_1 == "conservative"             ~ 1,
      political_category_1 == "labour"                   ~ 2,
      political_category_1 == "no political oreintation" ~ 3,
      political_category_1 == "other"                    ~ 4
    )
  )

data <- data |>
  mutate(
    party_rules = case_when(
      date >= "1991-01-01" & date <= "1997-06-01" ~ 1,
      date >= "2010-06-01"                         ~ 1,
      date >  "1997-06-01" & date <  "2010-06-01" ~ 2
    ),
    allignment = as.integer(party_rules == political_category_1),
    allignment = ifelse(
      vote4 == 3 & date > "2010-06-01" & date < "2015-06-01", 1L, allignment
    ),
    # Labels set ONCE, right here, when the variable is created --
    # everything downstream (models, avg_predictions, plots) inherits
    # them automatically, no need to translate/relabel anywhere else.
    allignment = factor(allignment, levels = c(0, 1),
                        labels = c("Not aligned", "Aligned"))
  )

# ============================================================
# 03  Forward fill
# ============================================================
data <- data |>
  arrange(pidp, date) |>
  group_by(pidp) |>
  fill(edu, marital_status, vote4,
       gen_health_1, political_category_1,
       .direction = "down") |>
  ungroup()

# ============================================================
# 04  Factor recoding
# ============================================================
covs <- "age + age_2 + edu + marital_status + gen_health_1"
# NOTE: age_2 is not created in this script -- assumed to come from
# 00_setting_work_space.R or already be present in the .rds file.
# If a model throws "object 'age_2' not found", check there first.

make_factors_all <- function(df) {
  df |> mutate(
    edu          = factor(as.integer(edu)),
    gen_health_1 = factor(as.integer(gen_health_1)),
    marital_status = factor(
      case_when(
        as.integer(marital_status) %in% c(2, 3) ~ "married/cohabiting",
        as.integer(marital_status) == 1          ~ "single",
        TRUE                                     ~ "other"
      ),
      levels = c("single", "married/cohabiting", "other")
    )
  )
}

# ============================================================
# 05  Analysis sample
# ============================================================
data_limited <- data |>
  filter(political_category_1 %in% c(1, 2)) |>
  make_factors_all()

# FIX: political_category_1 was a plain number (1/2) up to this point,
# never converted to a factor -- this was the real cause of the
# "Alignment x Support" panel issues (avg_predictions had no clean
# groups to build from, and earlier label fixes were patching the
# predictions data AFTER the fact). Doing it once, here, same as
# edu / age_group_1 below.
data_limited <- data_limited |>
  mutate(political_category_1 = factor(political_category_1,
                                       levels = c(1, 2),
                                       labels = c("Conservative", "Labour")))

# 5-year age groups (needed for panel C)
data_limited$age_group_1 <- cut(
  data_limited$age,
  breaks = seq(0, 100, by = 5),
  labels = paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-"),
  right = FALSE
)
data_limited <- data_limited |>
  mutate(age_group_1 = droplevels(age_group_1))

# ============================================================
# 06  Models
# ============================================================
m_allignment <- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment")),
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_edu <- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment * edu")),
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_support <- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment * political_category_1")),
  family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_age_1 <- glm(
  as.formula(paste("KID_1_dummy ~", "edu + marital_status + gen_health_1 +",
                   "+ allignment * age_group_1")),
  family = binomial(link = "cloglog"), data = data_limited
)

# ============================================================
# 07  Predictions & panels
# ============================================================

# ── A: base alignment ────────────────────────────────────────
preds_base <- avg_predictions(
  m_allignment, variables = "allignment", conf_level = 0.83
) |> as.data.frame()

p_base <- ggplot(preds_base,
                 aes(x = allignment, y = estimate,
                     ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(linewidth = 1.2, fatten = 3) +
  geom_errorbar(width = 0.15, linewidth = 1.0) +
  scale_y_continuous(limits = y_limits_common, breaks = y_breaks_common) +
  labs(x = "", y = "Predicted probability",
       title = "Overall Alignment") +
  theme_small

# ── B: alignment x supported party ───────────────────────────
preds_support <- avg_predictions(
  m_allignment_support,
  variables  = list(allignment            = levels(data_limited$allignment),
                    political_category_1 = levels(data_limited$political_category_1)),
  conf_level = 0.83
) |> as.data.frame()

p_support <- ggplot(preds_support,
                    aes(x = political_category_1, y = estimate,
                        ymin = conf.low, ymax = conf.high,
                        color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Not aligned" = "#CC0000",
                                "Aligned"     = "#004180"),
                     name = "Alignment") +
  scale_y_continuous(limits = y_limits_common, breaks = y_breaks_common) +
  labs(x = "", y = "Predicted probability",
       title = "Alignment × Supported Party") +
  theme_small +
  theme(legend.position = "bottom")

# ── C: alignment x age group ─────────────────────────────────
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
  scale_y_continuous(limits = y_limits_common, breaks = y_breaks_common) +
  labs(x = "", y = "Predicted probability",
       title = "Alignment × Age Group",
       color = "Alignment") +
  scale_color_brewer(palette = "Set1") +
  theme_small

# ── D: alignment x education ─────────────────────────────────
preds_edu <- avg_predictions(
  m_allignment_edu,
  variables  = list(allignment = levels(data_limited$allignment),
                    edu        = levels(data_limited$edu)),
  conf_level = 0.83
) |>
  as.data.frame() |>
  mutate(edu_label = factor(
    ifelse(edu == "1", "Low/middle", "High"),
    levels = c("Low/middle", "High")
  ))

p_edu <- ggplot(preds_edu,
                aes(x = edu_label, y = estimate,
                    ymin = conf.low, ymax = conf.high,
                    color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Not aligned" = "#CC0000",
                                "Aligned"     = "#004180"),
                     name = "Alignment") +
  scale_y_continuous(limits = y_limits_common, breaks = y_breaks_common) +
  labs(x = "", y = "Predicted probability",
       title = "Alignment × Education") +
  theme_small +
  theme(legend.position = "bottom")

# ============================================================
# 08  Patchwork -- p_all
# ============================================================
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

p_all <- ((p_base+poster_theme | p_support+poster_theme) / (p_age_1+poster_theme | p_edu+poster_theme ))+
  plot_annotation(
    # title      = "Political Alignment and Fertility",
    subtitle   = "Average predicted probabilities (83% CI)",
    tag_levels = "A",
    theme      = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13)
    )
  ) +
  # plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p_all
ggsave("plot_alignment_all.png", p_all,
       width = 14, height = 16, dpi = 300)

# ============================================================
# 09  Regression table -> docx
# ============================================================
modelsummary(
  list(
    "Alignment"                   = m_allignment,
    "Alignment × Supported Party" = m_allignment_support,
    "Alignment × Age group"       = m_allignment_age_1,
    "Alignment × Education"       = m_allignment_edu
  ),
  exponentiate = TRUE,
  stars        = TRUE,
  conf_level   = 0.83,
  gof_map      = c("nobs", "aic", "bic"),
  output       = "table_3_16_maj_2026.docx"
)