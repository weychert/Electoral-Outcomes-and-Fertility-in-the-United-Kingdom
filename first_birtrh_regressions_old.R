# ============================================================
# 00  Setup
# ============================================================
source("00_setting_work_space.R")
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(pandoc)
library(modelsummary)
library(patchwork)

# ── Shared plot themes ───────────────────────────────────────
x <- 20
theme_x <- theme(
  axis.text        = element_text(face = "bold", color = "black", size = x),
  axis.text.x      = element_text(face = "bold", color = "black", size = x),
  axis.text.y      = element_text(face = "bold", color = "black", size = x),
  axis.title.x     = element_text(face = "bold", color = "black", size = x),
  axis.title.y     = element_text(face = "bold", color = "black", size = x),
  legend.text      = element_text(face = "bold", color = "black", size = x),
  legend.title     = element_text(face = "bold", color = "black", size = x),
  plot.title       = element_text(face = "bold", size = 20, color = "black"),
  plot.subtitle    = element_text(face = "bold", size = 20, color = "black")
)

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
    ),
    voteeuref = as.integer(voteeuref)
  )

data <- data |>
  mutate(
    ele1997_1 = as.integer(date > as.Date("1997-05-01") & political_category_1 == 1),
    ele1997_2 = as.integer(date > as.Date("1997-05-01") & political_category_1 == 2),
    ele2001_1 = as.integer(date > as.Date("2001-06-01") & political_category_1 == 1),
    ele2001_2 = as.integer(date > as.Date("2001-06-01") & political_category_1 == 2),
    ele2005_1 = as.integer(date > as.Date("2005-05-01") & political_category_1 == 1),
    ele2005_2 = as.integer(date > as.Date("2005-05-01") & political_category_1 == 2),
    ele2010_1 = as.integer(date > as.Date("2010-05-01") & political_category_1 == 2),
    ele2010_2 = as.integer(date > as.Date("2010-05-01") & political_category_1 == 1),
    ele2015_1 = as.integer(date > as.Date("2015-05-01") & political_category_1 == 2),
    ele2015_2 = as.integer(date > as.Date("2015-05-01") & political_category_1 == 1),
    ele2017_1 = as.integer(date > as.Date("2017-05-01") & political_category_1 == 2),
    ele2017_2 = as.integer(date > as.Date("2017-05-01") & political_category_1 == 1),
    ele2019_1 = as.integer(date > as.Date("2019-12-01") & political_category_1 == 2),
    ele2019_2 = as.integer(date > as.Date("2019-12-01") & political_category_1 == 1),
    brexit_ref_happened = as.integer(date >= as.Date("2016-07-01")),
    brexit_happened     = as.integer(date >= as.Date("2020-02-01")),
    brexit_1    = as.integer(date > as.Date("2016-07-01") & voteeuref == 1),
    brexit_2    = as.integer(date > as.Date("2016-07-01") & voteeuref == 2),
    brexit_2020 = as.integer(date >= as.Date("2020-02-01") & voteeuref == 1),
    party_rules = case_when(
      date >= "1991-01-01" & date <= "1997-06-01" ~ 1,
      date >= "2010-06-01"                         ~ 1,
      date >  "1997-06-01" & date <  "2010-06-01" ~ 2
    ),
    allignment = as.integer(party_rules == political_category_1),
    allignment = ifelse(
      vote4 == 3 & date > "2010-06-01" & date < "2015-06-01", 1L, allignment
    ),
    allignment = factor(allignment, levels = c(0, 1),
                        labels = c("Not aligned", "Aligned")),
    post_1997 = ifelse(date > "1997-05-01", 1L, 0L),
    post_2001 = ifelse(date > "2001-06-01", 1L, 0L),
    post_2005 = ifelse(date > "2005-05-01", 1L, 0L),
    post_2010 = ifelse(date > "2010-05-01", 1L, 0L),
    post_2015 = ifelse(date > "2015-05-01", 1L, 0L),
    post_2017 = ifelse(date > "2017-06-01", 1L, 0L)
  )

# ============================================================
# 03  Forward fill
# ============================================================
data <- data |>
  arrange(pidp, date) |>
  group_by(pidp) |>
  fill(edu, marital_status, vote4, voteeuref,
       gen_health_1, political_category_1,
       .direction = "down") |>
  ungroup()

# ============================================================
# 04  Factor recoding — consistent across ALL elections
# ============================================================
covs <- "age + age_2 + edu + marital_status + gen_health_1"

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
# 05  Analysis samples
# ============================================================

write_dta(data, "output/data_elections.dta")

# political_category_1 == "conservative"             ~ 1,
# political_category_1 == "labour"                   ~ 2,

data_limited <- data |>
  filter(political_category_1 %in% c(1, 2)) |>
  make_factors_all()

election_windows <- list(
  ele1997 = c("1994-05-01", "2000-06-01"),
  ele2001 = c("1998-06-01", "2004-06-01"),
  ele2005 = c("2002-05-01", "2008-05-01"),
  ele2010 = c("2007-05-01", "2013-05-01"),
  ele2015 = c("2012-05-01", "2018-05-01"),
  ele2017 = c("2014-05-01", "2020-05-01"),
  ele2019 = c("2016-12-01", "2022-12-01"),
  brexit  = c("2016-01-01", "2020-01-01")
)

election_data <- lapply(names(election_windows), function(nm) {
  w   <- election_windows[[nm]]
  var <- paste0(nm, "_1")
  data |>
    filter(date >= as.Date(w[1]) & date <= as.Date(w[2])) |>
    make_factors_all() |>
    mutate(
      !!var := factor(.data[[var]], levels = c(0, 1),
                      labels = c("Winning party", "Losing party"))
    )
})
names(election_data) <- names(election_windows)

write_dta(data, "output/data_elections.dta")

# ============================================================
# 06  Models
# ============================================================

# ── Overall alignment ────────────────────────────────────────
m_allignment <- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment")),
  family = binomial(link = "cloglog"), data = data_limited
)

table(m_allignment$data$allignment)

m_interaction <- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment * age")),
  family = binomial(link = "cloglog"), data = data_limited
)

# ── One model per election ───────────────────────────────────
election_models <- lapply(names(election_windows), function(nm) {
  var <- paste0(nm, "_1")
  tryCatch(
    glm(
      as.formula(paste("KID_1_dummy ~", covs, "+", var)),
      family = binomial(link = "cloglog"),
      data   = election_data[[nm]]
    ),
    error = function(e) { cat("ERROR in", nm, ":", e$message, "\n"); NULL }
  )
})

names(election_models) <- names(election_windows)

failed <- names(election_models)[sapply(election_models, is.null)]
cat("\nFailed models:", paste(failed, collapse = ", "), "\n")

# ============================================================
# 07  Regression tables → Word
# ============================================================
modelsummary(
  list("Alignment" = m_allignment, "Alignment × Age" = m_interaction),
  exponentiate = TRUE, stars = TRUE,
  gof_map      = c("nobs", "aic", "bic"),
  output       = "table_alignment.docx"
)


election_labels <- setNames(
  election_models,
  sapply(names(election_models), function(nm)
    ifelse(nm == "brexit", "Brexit",
           paste("Election", gsub("ele", "", nm))))
)

modelsummary(
  election_labels,
  exponentiate = TRUE, stars = TRUE,
  gof_map      = c("nobs", "aic", "bic"),
  output       = "table_elections.docx"
)

# ============================================================
# 08  Plots
# ============================================================
plot_avg_predictions <- function(model, var, title = NULL,
                                 ylim_vals = c(0, 0.009), conf = 0.95) {
  avg_predictions(model, variables = var, conf_level = conf) |>
    as.data.frame() |>
    ggplot(aes(x = .data[[var]], y = estimate,
               ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(linewidth = 1.2, fatten = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 1.0) +
    ylim(ylim_vals) +
    labs(x = "", y = "Predicted probability", title = title) +
    theme_small
}

# ── Alignment plots ──────────────────────────────────────────
p_allignment  <- plot_avg_predictions(m_allignment,  "allignment", "Alignment")
p_interaction <- plot_avg_predictions(m_interaction, "allignment", "Alignment × Age")

ggsave("plot_allignment.png",             p_allignment,
       width = 8, height = 5, dpi = 300)
ggsave("plot_allignment_interaction.png", p_interaction,
       width = 8, height = 5, dpi = 300)

# ── Helper functions ─────────────────────────────────────────
build_election_preds <- function(conf_level) {
  bind_rows(lapply(names(election_models), function(nm) {
    if (is.null(election_models[[nm]])) return(NULL)
    var   <- paste0(nm, "_1")
    label <- ifelse(nm == "brexit", "Brexit",
                    paste("Election", gsub("ele", "", nm)))
    avg_predictions(election_models[[nm]], variables = var,
                    conf_level = conf_level) |>
      as.data.frame() |>
      mutate(election = label, group = as.character(.data[[var]]))
  }))
}

order_elections <- function(preds) {
  ord <- unique(preds$election)
  ord <- c(ord[ord != "Brexit"], "Brexit")
  preds |> mutate(election = factor(election, levels = ord))
}

make_facet_plot <- function(preds, ylim_vals, subtitle) {
  ggplot(preds, aes(x = group, y = estimate,
                    ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(linewidth = 1.0, fatten = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 0.9) +
    facet_wrap(~ election, ncol = 3) +
    ylim(ylim_vals) +
    labs(x = "", y = "Predicted probability",
         title    = "Political Alignment and Fertility by Election",
         subtitle = subtitle) +
    theme_small
}

# ── Facet plot: all elections, 95% CI ────────────────────────
p_facet_95 <- build_election_preds(0.95) |>
  order_elections() |>
  make_facet_plot(c(-0.01, 0.03), "Average predicted probabilities (95% CI)")

p_facet_95
ggsave("plot_elections_facet.png", p_facet_95,
       width = 16, height = 12, dpi = 300)

# ── Facet plot: no 2019, 83% CI ──────────────────────────────
p_facet_83 <- build_election_preds(0.83) |>
  filter(election != "Election 2019") |>
  order_elections() |>
  make_facet_plot(c(-0.001, 0.01), "Average predicted probabilities (83% CI)")

poster_theme <- theme_bw(base_size = 30) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30)
  )

p_facet_83 = p_facet_83+poster_theme
ggsave("plot_elections_facet_no_2019.png", p_facet_83,
       width = 16, height = 12, dpi = 300)

library(dplyr)
library(tidyr)

# ── Full analytical sample ────────────────────────────────────
desc_sample <- data_limited |>
  filter(!is.na(KID_1_dummy)) |>
  filter(!is.na(interview_date))

# ── Helper: count and percent ─────────────────────────────────
pct <- function(x) round(100 * sum(x, na.rm = TRUE) / sum(!is.na(x)), 1)

# ── Age groups ───────────────────────────────────────────────
cat("=== AGE ===\n")
desc_sample |>
  mutate(age_group = case_when(
    age %in% 18:24 ~ "18-24",
    age %in% 25:34 ~ "25-34",
    age %in% 35:44 ~ "35-44"
  )) |>
  count(age_group) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

# ── Education ────────────────────────────────────────────────
cat("\n=== EDUCATION ===\n")
desc_sample |>
  mutate(edu_i = as.integer(edu)) |>
  count(edu_i) |>
  mutate(
    label = case_when(edu_i == 1 ~ "low/middle", edu_i == 2 ~ "high", TRUE ~ "missing"),
    pct   = round(100 * n / sum(n), 1)
  ) |>
  print()

# ── Marital status ───────────────────────────────────────────
cat("\n=== MARITAL STATUS ===\n")
desc_sample |>
  mutate(ms = as.integer(marital_status)) |>
  count(ms) |>
  mutate(
    label = case_when(
      ms == 1 ~ "single",
      ms == 2 ~ "cohabiting",
      ms == 3 ~ "married",
      TRUE    ~ "other/missing"
    ),
    pct = round(100 * n / sum(n), 1)
  ) |>
  print()

# ── Self-rated health ────────────────────────────────────────
cat("\n=== HEALTH ===\n")
desc_sample |>
  mutate(gh = as.integer(gen_health_1)) |>
  count(gh) |>
  mutate(
    label = case_when(
      gh == 1 ~ "poor",
      gh == 2 ~ "fair",
      gh == 3 ~ "good/excellent",
      TRUE    ~ "missing"
    ),
    pct = round(100 * n / sum(n), 1)
  ) |>
  print()

# ── Political party support ──────────────────────────────────
cat("\n=== POLITICAL PARTY ===\n")
desc_sample |>
  count(political_category_1) |>
  mutate(
    label = case_when(
      political_category_1 == 1 ~ "Conservative",
      political_category_1 == 2 ~ "Labour",
      political_category_1 == 3 ~ "Other",
      political_category_1 == 4 ~ "No orientation",
      TRUE                      ~ "missing"
    ),
    pct = round(100 * n / sum(n), 1)
  ) |>
  print()

# ── EU referendum vote ───────────────────────────────────────
cat("\n=== EU REFERENDUM VOTE ===\n")
desc_sample |>
  filter(voteeuref %in% c(1, 2)) |>
  count(voteeuref) |>
  mutate(
    label = case_when(voteeuref == 1 ~ "Remain", voteeuref == 2 ~ "Leave"),
    pct   = round(100 * n / sum(n), 1)
  ) |>
  print()

# ── Outcome ──────────────────────────────────────────────────
cat("\n=== OUTCOME ===\n")
cat("First birth events:  ", sum(desc_sample$KID_1_dummy == 1, na.rm = TRUE), "\n")
cat("Total person-months: ", nrow(desc_sample), "\n")
cat("Event rate:          ", round(100 * mean(desc_sample$KID_1_dummy, na.rm = TRUE), 3), "%\n")

# ── Unique individuals ───────────────────────────────────────
cat("\n=== SAMPLE SIZE ===\n")
cat("Person-months:       ", format(nrow(desc_sample), big.mark = ","), "\n")
cat("Unique individuals:  ", format(n_distinct(desc_sample$pidp), big.mark = ","), "\n")
cat("Date range:          ", format(min(desc_sample$date)), "to", format(max(desc_sample$date)), "\n")

# ── Election window samples ──────────────────────────────────
cat("\n=== ELECTION WINDOW SAMPLE SIZES ===\n")
for (nm in names(election_windows)) {
  w   <- election_windows[[nm]]
  var <- paste0(nm, "_1")
  n   <- nrow(election_data[[nm]])
  n1  <- sum(as.integer(election_data[[nm]][[var]]) == 1, na.rm = TRUE)
  n0  <- sum(as.integer(election_data[[nm]][[var]]) == 0, na.rm = TRUE)
  cat(sprintf("%-10s  N = %7s  losing = %6s  winning = %6s\n",
              nm,
              format(n,  big.mark = ","),
              format(n1, big.mark = ","),
              format(n0, big.mark = ",")))


  
  
  
  
################################################################################
library(dplyr)
library(readr)

# ── Analytical sample ─────────────────────────────────────────
ds <- data_limited |>
  filter(!is.na(KID_1_dummy)) |>
  filter(!is.na(interview_date))

# ── Helper ────────────────────────────────────────────────────
make_row <- function(variable, category, n_cat, n_total) {
  data.frame(
    Variable = variable,
    Category = category,
    Percent  = round(100 * n_cat / n_total, 1),
    stringsAsFactors = FALSE
  )
}

n_total <- nrow(ds)

# ── Age ───────────────────────────────────────────────────────
age_rows <- bind_rows(
  make_row("Age", "18-24", sum(ds$age %in% 18:24, na.rm=TRUE), n_total),
  make_row("Age", "25-34", sum(ds$age %in% 25:34, na.rm=TRUE), n_total),
  make_row("Age", "35-44", sum(ds$age %in% 35:44, na.rm=TRUE), n_total)
)

# ── Education ─────────────────────────────────────────────────
edu_int <- as.integer(ds$edu)
edu_rows <- bind_rows(
  make_row("Education", "Low & middle (below tertiary)", sum(edu_int == 1, na.rm=TRUE), n_total),
  make_row("Education", "High (tertiary degree or above)", sum(edu_int == 2, na.rm=TRUE), n_total)
)

# ── Marital status ────────────────────────────────────────────
ms_int <- as.integer(ds$marital_status)
ms_rows <- bind_rows(
  make_row("Marital/partnership status", "Single",                   sum(ms_int == 1, na.rm=TRUE), n_total),
  make_row("Marital/partnership status", "Married or cohabiting",    sum(ms_int %in% c(2,3), na.rm=TRUE), n_total),
  make_row("Marital/partnership status", "Separated/divorced/widowed", sum(ms_int == 4, na.rm=TRUE), n_total)
)

# ── Health ────────────────────────────────────────────────────
gh_int <- as.integer(ds$gen_health_1)
gh_rows <- bind_rows(
  make_row("Self-rated health", "Poor",          sum(gh_int == 1, na.rm=TRUE), n_total),
  make_row("Self-rated health", "Fair",          sum(gh_int == 2, na.rm=TRUE), n_total),
  make_row("Self-rated health", "Good/excellent",sum(gh_int == 3, na.rm=TRUE), n_total)
)

# ── Political party ───────────────────────────────────────────
pol <- ds$political_category_1
pol_rows <- bind_rows(
  make_row("Political party support", "Conservative",         sum(pol == 1, na.rm=TRUE), n_total),
  make_row("Political party support", "Labour",               sum(pol == 2, na.rm=TRUE), n_total),
  make_row("Political party support", "Other party",          sum(pol == 4, na.rm=TRUE), n_total),
  make_row("Political party support", "No political orientation", sum(pol == 3, na.rm=TRUE), n_total)
)

# ── EU referendum ─────────────────────────────────────────────
eu <- ds |> filter(as.integer(voteeuref) %in% c(1, 2))
n_eu <- nrow(eu)
eu_rows <- bind_rows(
  make_row("EU referendum vote (2016)", "Remain", sum(as.integer(eu$voteeuref) == 1, na.rm=TRUE), n_eu),
  make_row("EU referendum vote (2016)", "Leave",  sum(as.integer(eu$voteeuref) == 2, na.rm=TRUE), n_eu)
)

# ── Outcome & sample size ─────────────────────────────────────
outcome_rows <- data.frame(
  Variable = c(
    "First birth (per person-month)",
    "Number of person-months",
    "Number of women",
    "Observation period"
  ),
  Category = c(
    "Event rate",
    "Full analytical sample",
    "",
    ""
  ),
  Percent = c(
    round(100 * mean(ds$KID_1_dummy, na.rm=TRUE), 3),
    nrow(ds),
    n_distinct(ds$pidp),
    NA
  ),
  stringsAsFactors = FALSE
)

# ── Combine & export ──────────────────────────────────────────
desc_table <- bind_rows(
  age_rows, edu_rows, ms_rows, gh_rows,
  pol_rows, eu_rows, outcome_rows
)

# Clean up: blank out repeated variable names
desc_table <- desc_table |>
  mutate(Variable = ifelse(duplicated(Variable), "", Variable))

write_csv(desc_table, "descriptive_statistics.csv")
cat("Saved: descriptive_statistics.csv\n")
print(desc_table)
}



################################################################################
# ── Models ───────────────────────────────────────────────────
data_limited <- data_limited |>
mutate(age_group = factor(
  case_when(
    age %in% 18:24 ~ "18-24",
    age %in% 25:34 ~ "25-34",
    age %in% 35:44 ~ "35-44"
  ),
  levels = c("18-24", "25-34", "35-44")
))

m_allignment <- glm(
as.formula(paste("KID_1_dummy ~", covs, "+ allignment")),
family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_age <- glm(
as.formula(paste("KID_1_dummy ~", "edu + marital_status + gen_health_1 +", "+ allignment * age_group")),
family = binomial(link = "cloglog"), data = data_limited
)

summary(m_allignment_age)
m_allignment_edu <- glm(
as.formula(paste("KID_1_dummy ~", covs, "+ allignment * edu")),
family = binomial(link = "cloglog"), data = data_limited
)

m_allignment_support<- glm(
  as.formula(paste("KID_1_dummy ~", covs, "+ allignment * political_category_1")),
  family = binomial(link = "cloglog"), data = data_limited
)

summary(m_allignment_support)

# ── Tables ───────────────────────────────────────────────────
modelsummary(
list(
  "Alignment"             = m_allignment,
  "Alignment × Supported Party" = m_allignment_support,
  "Alignment × Age group" = m_allignment_age,
  "Alignment × Education" = m_allignment_edu
),
exponentiate = TRUE, stars = TRUE,
gof_map      = c("nobs", "aic", "bic"),
output       = "table_alignment_interactions.docx"
)

############################

# With custom labels
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
# should now show ONLY the levels actually used, e.g. 15-19 ... 40-44

# refit the model on this cleaned data
m_allignment_age_1 <- update(m_allignment_age_1, data = data_limited)

m_allignment_age_1<- glm(
  as.formula(paste("KID_1_dummy ~", "edu + marital_status + gen_health_1 +", 
                   "+ allignment * age_group_1")),
  family = binomial(link = "cloglog"), data = data_limited
)

summary(m_allignment_age_1)

# Option 1: filter to levels with data
age_levels_used <- names(table(data_limited$age_group_1)[table(data_limited$age_group_1) > 0])

preds_age_1 <- avg_predictions(
  m_allignment_age_1,
  variables  = list(allignment  = levels(data_limited$allignment),
                    age_group_1 = age_levels_used),
  conf_level = 0.83
) |> as.data.frame()

p_age_1 = ggplot(preds_age_1,
       aes(x = age_group_1, y = estimate,
           ymin = conf.low, ymax = conf.high,
           color = allignment)) +
  geom_pointrange(linewidth = 1.2, fatten = 3,
                  position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.15, linewidth = 1.0,
                position = position_dodge(width = 0.5)) +
  ylim(-0.001, 0.012) +
  labs(x = "", y = "Predicted probability",
       title = "Alignment × Age Group",
       color = "Alignment") +
  scale_color_brewer(palette = "Set1") +  # or "Dark2", "Set2", etc.
  theme_small

p_age_1

# ── Predictions: base alignment ──────────────────────────────
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
labs(x = "", y = "Predicted probability",
     title = "Overall Alignment") +
theme_small

# ── Predictions: alignment × age group ───────────────────────
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
scale_color_manual(values = c("Not aligned" = "#CC0000",
                              "Aligned"     = "#004180"),
                   name = "") +
ylim(-0.001, 0.015) +
labs(x = "", y = "Predicted probability",
     title = "Alignment × Age Group") +
theme_small +
theme(legend.position = "bottom")

# ── Predictions: alignment × education ───────────────────────
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
preds_support <- avg_predictions(
  m_allignment_support,
  variables = list(
    allignment = unique(insight::get_data(m_allignment_support)$allignment),
    political_category = unique(insight::get_data(m_allignment_support)$political_category)
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
scale_color_manual(values = c("Not aligned" = "#CC0000",
                              "Aligned"     = "#004180"),
                   name = "") +
ylim(-0.001, 0.015) +
labs(x = "", y = "Predicted probability",
     title = "Alignment × Education") +
theme_small +
theme(legend.position = "bottom")

preds_support$political_category <- factor(
  preds_support$political_category,
  levels = c("Right-wing", "Left-wing"),  # adjust to match your actual level names
  labels = c("Conservative", "Labour")
)
p_support<-ggplot(preds_support,
                  aes(x = political_category, y = estimate,
                      ymin = conf.low, ymax = conf.high,
                      color = allignment, group = allignment)) +
  geom_pointrange(linewidth = 1.0, fatten = 3,
                  position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0.15, linewidth = 0.9,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Not aligned" = "#CC0000",
                                "Aligned"     = "#004180"),
                     name = "") +
  ylim(-0.001, 0.015) +
  labs(x = "", y = "Predicted probability",
       title = "Alignment × Alignment x Supported party") +
  theme_small +
  theme(legend.position = "bottom")

# ── Patchwork ─────────────────────────────────────────────────
poster_theme <- theme_bw(base_size = 20) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
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

modelsummary(
  list(
    "Alignment"             = m_allignment,
    "Alignment × Supported Party" = m_allignment_support,
    "Alignment × Age group" = m_allignment_age_1,
    "Alignment × Education" = m_allignment_edu
  ),
  exponentiate = TRUE, stars = TRUE,
  gof_map      = c("nobs", "aic", "bic"),
  output       = "table_3_16_maj_2026.docx"
)

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

m_allignment

# ── Individual panels with poster theme applied ───────────────
p_base   <- p_base    + poster_theme
ggsave("p_base_final.png", p_base,
       width = 25, height = 13, dpi = 300)

p_support<- p_support + poster_theme
ggsave("p_support_final.png", p_support,
       width = 25, height = 13, dpi = 300)

p_age_1   <- p_age_1  + poster_theme
ggsave("p_age_1_final .png", p_age_1,
       width = 25, height = 13, dpi = 300)
p_edu_final     <- p_edu     + poster_theme
ggsave("p_edu_final.png", p_edu_final,
       width = 25, height = 13, dpi = 300)

