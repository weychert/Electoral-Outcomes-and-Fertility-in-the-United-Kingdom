#----------------------------------------#
#      LabFam Individual Biographies     #
#----------------------------------------#

# Version 1.0
# Date 27/06/2024

# This file contains all resources needed to run the LIB codes. The file
# contains two parts: a list of all paths and a list of all packages required
#  to run the LIB codes.

# Please edit the paths to those that you will be using.
# In the default version all databases are stored within a common (root) folder
# If that is not the case in your system, modify the paths accordingly.


# Setting the paths
#--------------------------------

folder_personal  = "/Users/ewaweychert/Desktop/politics_fertility/"

# BHPS/UKHLS
folder_uk_fertility_1 = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/ukhls/")
folder_bhps_1         = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/bhps/")
folder_part_uk        = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/")
folder_main_uk        = paste0(folder_personal, "United Kingdom/6931/UKDA-6931-stata/stata/stata13_se/")
folder_Pronzato_uk    = paste0(folder_personal, "United Kingdom/UKDA-5629-stata8/stata8/")


requiredPackages = c("dplyr",      # 1.  dplyr - for filtering mutating etc
                     "haven",      # 2.  haven - downloading stata files (end with .dta)
                     "reshape2",   # 3.  reshape2 - for dcast function (from long to wide)
                     "stringr",    # 4.  stringr - for string operations
                     "tidyr",      # 5.  tidyr - for data cleaning 
                     "sjlabelled", # 6.  retrive stata labels
                     "neatRanges", # 6.  expand_dates() Returns a full data frame with expanded sequences in a column, e.g. by day or month
                     "lubridate",  # 8.  lubridate::ceiling_date()
                     "naniar",     # 9.  in shp replace_with_na
                     "easyPSID",   # 10. useful for PSID data
                     "psidR",      # 11. useful for PSID data
                     "openxlsx",   # 12. reading excel for PSID data
                     "sjmisc",     # 13. is_empty () in shp master 
                     "data.table", # 14. merge.date.tabel()
                     "survival",   # 15. KM curve in validation survfit()
                     "survminer",  # 16. plottting KM curve in validation ggsurvplot()
                     "ggplot2",    # 17. graphical analysis in validation 
                     "patchwork",  # 18. graphical analysis in validation ggpubr::get_legend()
                     "readr",      # 19. reading R objects
                     "rlang",      # For !! and sym
                     "collapse",
                     "purrr",
                     "labelled",   # for stata labeles 
                     "DataExplorer",
                     "survival",
                     "sjlabelled",
                     "Hmisc"
)

for(i in requiredPackages){
  for(i in requiredPackages) {if(!require(i,character.only = TRUE)) install.packages(i)}
  for(i in requiredPackages) {if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 
}

for(pkg in requiredPackages) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

rm("requiredPackages", i)

# Object to filter "not in" using dplyr package
`%notin%` <- Negate(`%in%`)