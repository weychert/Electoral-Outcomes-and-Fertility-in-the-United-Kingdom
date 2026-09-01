================================================================================
  ELECTORAL OUTCOMES AND FERTILITY IN THE UNITED KINGDOM
  README
================================================================================

Project   Does political loss affect fertility? Evidence from UK general
          elections and the Brexit referendum (1991-2020)
Author    Ewa Weychert
Data      UK Data Service study 6931 - Understanding Society (UKHLS) waves
          1-11 (2009-2020) and Harmonised BHPS waves 1-18 (1991-2009)

--------------------------------------------------------------------------------
WHAT'S IN THIS REPO
--------------------------------------------------------------------------------

  00_setting_work_space.R          Paths + package setup. Source()'d by every
                                    other script - always run/edit this first.

  BHPS_UKHLS_master_file.R         Builds the person-level master file:
                                    interview dates/status, demographics,
                                    political variables, weights, employment
                                    status. Sourced from BHPS + UKHLS raw data.
                                    -> output/master_bhps_ukhls_wide.rds

  BHPS_UKHLS_fertility.R           Builds fertility histories per person from
                                    three sources: family matrix (xhhrel),
                                    non-resident children (natchild/childnt
                                    files). Reshapes to wide, one row per
                                    person with birth year/month/sex per child.
                                    -> output/bhps_ukhls_fertility.rds

  official.R                       Main analysis script. Builds the person-
                                    month survival dataset, merges political
                                    alignment/election-loss treatment
                                    variables and covariates, runs cloglog
                                    survival models, produces marginal effects
                                    plots.
                                    -> output/1_may_data_split_5.rds/.dta
                                    -> figure_1.jpg

  polish_labels.R                  Takes the fitted models' data
                                    (data_limited.rds) and produces the Polish-
                                    language versions of the alignment plots
                                    (labels translated: "Zgodni"/"Niezgodni")
                                    for presentation/publication use.

  first_birtrh_regressions_old.R   Earlier exploratory first-birth
                                    regressions. Kept for reference only -
                                    NOT part of the current pipeline.

  data_limited.rds                 Trimmed model-ready dataset (subset of
                                    columns used for the final regressions
                                    and plots) used by polish_labels.R.

  descriptive_statistics.csv       Descriptive statistics table for the
                                    analytic sample.

  table_3_16_maj_2026.docx         Regression results table (as of 16 May
                                    2026 draft).

  plot_alignment_all.png           Marginal effects plot - political
                                    alignment and first birth.

--------------------------------------------------------------------------------
RUN ORDER
--------------------------------------------------------------------------------

  1. 00_setting_work_space.R   (edit folder_personal to your local data path)
  2. BHPS_UKHLS_master_file.R
  3. BHPS_UKHLS_fertility.R
  4. official.R
  5. polish_labels.R            (optional - Polish-language plot versions)

  first_birtrh_regressions_old.R is archived and does not need to be run.

--------------------------------------------------------------------------------
DATA REQUIREMENTS
--------------------------------------------------------------------------------

  UK Data Service study 6931 (UKDA-6931-stata), not included in this repo.
  Required files, referenced via folder_main_uk / folder_bhps_1 in
  00_setting_work_space.R:

    ukhls/xwavedat_protect.dta        cross-wave person characteristics
    ukhls/xwaveid_protect.dta         UKHLS interview status
    bhps/xwaveid_bh_protect.dta       BHPS interview status
    ukhls/xhhrel_protect.dta          household relationships / children
    ukhls/[a-k]_indresp_protect.dta   UKHLS individual responses (waves 1-11)
    ukhls/[a-k]_indall_protect.dta    UKHLS individual dates
    bhps/b[a-r]_indresp_protect.dta   BHPS individual responses (waves 1-18)
    bhps/b[b,k,l]_childnt_protect.dta BHPS non-resident children
    ukhls/a_natchild_protect.dta      UKHLS non-resident children, wave a
    ukhls/f_natchild_protect.dta      UKHLS non-resident children, wave f

  Set the correct path at the top of 00_setting_work_space.R:
    folder_personal = "/your/path/to/data/"

--------------------------------------------------------------------------------
KEY VARIABLES
--------------------------------------------------------------------------------

  Outcome
    KID_1_dummy          First birth indicator (1 = birth in next 9 months)

  Treatment (election "losers" flagged 1 after the election date)
    ele1997_1 / ele2001_1 / ele2005_1   Conservative supporters (Labour wins)
    ele2010_1 / ele2015_1 / ele2019_1   Labour supporters (Conservative wins)
    brexit_1                            Remain voters, post-June 2016
    allignment                          Aligned / Not aligned with government

  Covariates
    age, age_2, edu, marital_status, gen_health_1

  Political classification
    political_category_1   1 = Conservative, 2 = Labour, 3 = other,
                            4 = no orientation

  Sample
    Women aged 18-44, observed from first interview date onward. Election
    models restricted to Conservative/Labour supporters; Brexit model
    restricted to referendum voters (voteeuref == 1 or 2).

--------------------------------------------------------------------------------
MODEL
--------------------------------------------------------------------------------

  Complementary log-log (cloglog) GLM - discrete-time survival model for
  first birth, e.g.:

    KID_1_dummy ~ age + age_2 + edu + marital_status + gen_health_1 + allignment

  Estimated within a 6-year window around each electoral event (3 years
  before / 3 years after).

--------------------------------------------------------------------------------
PACKAGES
--------------------------------------------------------------------------------

  Core        dplyr, tidyr, haven, ggplot2, patchwork, lubridate, stringr,
              reshape2, data.table
  Modelling   marginaleffects, insight, survival, survminer
  Labels      sjlabelled, labelled, sjmisc, sjPlot
  Other       DataExplorer, purrr, collapse, Hmisc

  Installed/loaded automatically by 00_setting_work_space.R.

--------------------------------------------------------------------------------
NOTES
--------------------------------------------------------------------------------

  - Raw survey data files (.dta) are NOT included in this repo - request
    access via the UK Data Service (study 6931).
  - .rds/.dta output files with individual-level data should stay out of
    version control unless anonymised; consider a .gitignore for output/.
  - first_birtrh_regressions_old.R is archived, not run in the pipeline.

================================================================================
