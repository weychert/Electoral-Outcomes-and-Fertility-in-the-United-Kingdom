================================================================================
  POLITICAL LOSS AND FERTILITY — UK ANALYSIS
  README
================================================================================

Project:  Does political loss affect fertility? Evidence from UK general
          elections and the Brexit referendum (1991–2020)
Author:   [Your name]
Date:     May 2026

--------------------------------------------------------------------------------
SUGGESTED FILE RENAMES
--------------------------------------------------------------------------------

Current name                      Suggested rename
--------------------------------  -----------------------------------------------
00_setting_work_space.R           00_setup.R
BHPS_UKHLS_master_file.R          01_build_master.R
BHPS_UKHLS_fertility.R            02_build_fertility.R
first_birtrh_regressions_old.R    03_regressions_archive.R   (archive, not in pipeline)
official.R                        04_analysis_main.R

--------------------------------------------------------------------------------
PIPELINE OVERVIEW
--------------------------------------------------------------------------------

Run scripts in this order:

  Step 1.  00_setup.R
           Sets all folder paths and installs/loads required packages.
           MUST be run first — all other scripts call source("00_setup.R").

  Step 2.  01_build_master.R
           Builds the person-level master file from BHPS and UKHLS cross-wave
           data. Extracts interview dates, political preferences, health,
           education, marital status, and region for all waves (1991–2019).
           Output: output/master_bhps_ukhls_wide.rds
                   output/1_may_2025_both_bhps_ukhls.rds

  Step 3.  02_build_fertility.R
           Constructs fertility histories for all respondents using three
           sources: family matrix (xhhrel_protect.dta), non-resident children
           (natchild files), and BHPS childnt files. Merges into wide format
           with one row per person and columns for each child's birth year,
           month, and sex.
           Output: output/bhps_ukhls_fertility.rds

  Step 4.  04_analysis_main.R  (formerly official.R)
           Main analysis script. Builds the person-month survival dataset,
           merges political and health covariates, creates election treatment
           variables, runs complementary log-log models for each election and
           Brexit, produces regression tables (.docx) and marginal effects
           plots (.png).
           Input:  output/1_may_data_split_5.rds
           Output: table_alignment.docx
                   table_elections.docx
                   plot_allignment.png
                   plot_elections_facet.png
                   plot_elections_facet_no_2019.png

  [Archive]  03_regressions_archive.R  (formerly first_birtrh_regressions_old.R)
           Earlier exploratory regressions. Not part of the current pipeline.
           Kept for reference only.

--------------------------------------------------------------------------------
DATA REQUIREMENTS
--------------------------------------------------------------------------------

The analysis uses UK Data Service study 6931:
  Understanding Society: Waves 1-11, 2009-2020 and Harmonised BHPS:
  Waves 1-18, 1991-2009 (UKDA-6931-stata)

Required files (in folder_main_uk / folder_bhps_1):
  ukhls/xwavedat_protect.dta       — cross-wave person characteristics
  ukhls/xwaveid_bh_protect.dta     — BHPS interview status
  ukhls/xhhrel_protect.dta         — household relationships / children
  ukhls/[a-k]_indresp_protect.dta  — UKHLS individual responses (waves 1-11)
  ukhls/[a-k]_indall_protect.dta   — UKHLS individual dates
  bhps/b[a-r]_indresp_protect.dta  — BHPS individual responses (waves 1-18)
  bhps/b[a-k]_childnt_protect.dta  — BHPS non-resident children (waves b,k,l)
  ukhls/a_natchild_protect.dta     — UKHLS non-resident children wave a
  ukhls/f_natchild_protect.dta     — UKHLS non-resident children wave f

Set the correct path in 00_setup.R:
  folder_personal = "/your/path/to/data/"

--------------------------------------------------------------------------------
KEY VARIABLES
--------------------------------------------------------------------------------

Outcome:
  KID_1_dummy         First birth indicator (1 = birth in next 9 months, 0 = no)

Treatment variables (election losers get 1 after election date):
  ele1997_1           Conservative supporters after 1997 (Labour win)
  ele2001_1           Conservative supporters after 2001 (Labour win)
  ele2005_1           Conservative supporters after 2005 (Labour win)
  ele2010_1           Labour supporters after 2010 (Conservative win)
  ele2015_1           Labour supporters after 2015 (Conservative win)
  ele2019_1           Labour supporters after 2019 (Conservative win)
  brexit_1            Remain voters after June 2016 referendum

Covariates:
  age / age_2         Age and age squared
  edu                 Education (1 = low/middle, 2 = high degree)
  marital_status      single / married+cohabiting / other
  gen_health_1        Self-rated health (1=poor to 3=excellent)

Political classification:
  political_category_1   1=Conservative, 2=Labour, 3=other, 4=no orientation

Sample:
  Women aged 18-44, observed from first interview date onwards.
  Election models restricted to Conservative and Labour supporters only.
  Brexit model restricted to referendum voters (voteeuref == 1 or 2).

--------------------------------------------------------------------------------
MODEL SPECIFICATION
--------------------------------------------------------------------------------

  Complementary log-log (cloglog) GLM — appropriate for rare events in
  discrete-time survival analysis of first birth.

  Formula:
    KID_1_dummy ~ age + age_2 + edu + marital_status + gen_health_1 + [treatment]

  Estimated separately for each electoral event within a 6-year window
  (3 years before and 3 years after the election date).

--------------------------------------------------------------------------------
OUTPUT FILES
--------------------------------------------------------------------------------

  table_alignment.docx            Overall political alignment models
  table_elections.docx            All 7 election models side by side
  plot_allignment.png             Marginal predictions — alignment
  plot_allignment_interaction.png Marginal predictions — alignment x age
  plot_elections_facet.png        Facet plot — all elections (95% CI)
  plot_elections_facet_no_2019.png Facet plot — excluding 2019 (83% CI)

--------------------------------------------------------------------------------
PACKAGE REQUIREMENTS
--------------------------------------------------------------------------------

  Core:     dplyr, tidyr, haven, ggplot2, patchwork, lubridate
  Modelling: marginaleffects, modelsummary, pandoc, survival, survminer
  Labels:   sjlabelled, labelled, sjmisc
  Other:    data.table, DataExplorer, stringr, reshape2, fixest

  All packages are installed automatically by 00_setup.R on first run.

--------------------------------------------------------------------------------
NOTES
--------------------------------------------------------------------------------

  - The person-month dataset (1_may_data_split_5.rds) is produced by
    04_analysis_main.R and saved to the output/ folder for reuse.
  - Filter !is.na(interview_date) is critical: it restricts observations
    to months after each person's first interview (forward-filled interview
    date is NA before the interview takes place).
  - Filter !is.na(KID_1_dummy) removes person-months where the 9-month
    lead outcome cannot be computed (end of observation window).
  - Election treatment variables have no upper date bound — windowing to
    the 6-year period happens at the analysis stage via date filters.

================================================================================
