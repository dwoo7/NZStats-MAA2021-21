################################## DRIVER LICENCING TRANSITIONS - SURVIVAL ANALYSIS ###################################################
# This analysis creates a cohort of individuals who turned a specific age in a specific year (as specified by you input values),
# and constructs a survival analysis for that population over the next "x" years to understand their transitions into Learners,
# Restricted, and Full driver licence status.

# Authors: Vinay Benny, Daniel Woo, Joseph Hendry
# Dated: 05 Feb 2024
#######################################################################################################################################


# 1. Set up the R libraries and functions.
setwd("~/Network-Shares/DataLabNas/MAA/MAA2021-21/30_projects/2023_driver_licence_stage_analysis/code/rprogs")
source("r_sql_functions.R")
source("analysis_support_functions.R")
source("rrn.R")
# install.packages("/nas/DataLab/GenData/R_User_Libraries/rapportools_1.1.tar.gz")
# install.packages("/nas/DataLab/GenData/R_User_Libraries/summarytools_1.0.1.tar.gz")
# install.packages("/nas/DataLab/GenData/R_User_Libraries/cmprsk_2.2-11.tar.gz")
library(dplyr)
library(summarytools)
library(ggplot2)
library(survival)
library(cmprsk)
library(ggsurvfit)
library(survminer)


######################## 2. Declare the GLOBAL VARIABLES for the analysis #######################
IDICLEANYYYYMM = 202310   # The year-month suffix to the IDI Clean version you intend to use.
PROJPREFIX = "DL-MAA2021-21" # The IDI Project schema under which the user tables will be written to.
FOLLOWUP_START_AGE = 15 # This is the age from which the cohort is followed.
REF_YEAR = 2015  # This is the year that the cohort population turns 15.
UNIFY_SPELLS_THRESHOLD = 1 # Number of days between 2 successive resident years for individuals, that must be treated as one spell in NZ for an individual
# For instance, if a person is treated as part of resident population (in apc_time_series) for all snapshots from 30 June 2013 to 30 June 2015, goes missing 
# in 30 June 2016 snapshot and returns in 30 June 2017 snapshot, you may choose to ignore that by setting this parameter to > 366 days( as an example).
# Currently we only follow individuals upto the point where they disappear from a snapshot year.
# Do not change UNIFY_SPELLS_THRESHOLD parameter unless you prefer to include individuals who may go missing through time and return into the sample.


######################## 3. Set up the queries based on the global variables above, and run those. #######################
spells_query <- read_content_from_file("../sql/1_define_spells.sql")
spells_query <- replace_parameter_with_value(spells_query, "\\$\\(REF_YEAR\\)", toString(REF_YEAR))
spells_query <- replace_parameter_with_value(spells_query, "\\$\\(FOLLOWUP_START_AGE\\)", toString(FOLLOWUP_START_AGE))
spells_query <- replace_parameter_with_value(spells_query, "\\$\\(UNIFY_SPELLS_THRESHOLD\\)", toString(UNIFY_SPELLS_THRESHOLD))
spells_query <- replace_parameter_with_value(spells_query, "\\$\\(IDICLEANYYYYMM\\)", toString(IDICLEANYYYYMM))
spells_query <- replace_parameter_with_value(spells_query, "\\$\\(PROJPREFIX\\)", toString(PROJPREFIX))
queries <- parse_sqlqueries_from_string(spells_query)

cohort_sql <- read_content_from_file("../sql/2_create_analysis_dataset.sql")

# Connect to the SQL server database and read the analysis data in to R
conn <- create_db_conn(paste0("IDI_Clean_", IDICLEANYYYYMM))
execute_queries(conn, queries)
analysisdata <- dbGetQuery(conn, cohort_sql)
close_db_conn(conn)

######################## 4. Set up a "validation dataset" using random sampling if required #######################
# Not used for now - only set up if required to validate findings on cohort independently using a validation dataset



######################## 5. Create analysis ready datasets #######################
# Look at some summary statistics to see if there are any anomalies
# dfSummary(analysisdata)
# Add some formatting an transformations to the data.
analysisdata <- analysisdata %>%
  mutate(
    event = factor(event, levels = c("CENSOR", "LEARNER", "RESTRICTED", "FULL"))
    ,apc_income_src01_amt = ifelse(is.na(apc_income_src01_amt), 0.0, apc_income_src01_amt)
    ,apc_income_src02_amt = ifelse(is.na(apc_income_src02_amt), 0.0, apc_income_src02_amt)
    ,apc_income_src03_amt = ifelse(is.na(apc_income_src03_amt), 0.0, apc_income_src03_amt)
    ,apc_income_src05_amt = ifelse(is.na(apc_income_src05_amt), 0.0, apc_income_src05_amt)
    ,apc_income_src07_amt = ifelse(is.na(apc_income_src07_amt), 0.0, apc_income_src07_amt)
    ,apc_income_src08_amt = ifelse(is.na(apc_income_src08_amt), 0.0, apc_income_src08_amt)
    ,apc_income_src09_amt = ifelse(is.na(apc_income_src09_amt), 0.0, apc_income_src09_amt)
    ,apc_income_src10_amt = ifelse(is.na(apc_income_src10_amt), 0.0, apc_income_src10_amt)
    ,apc_income_src11_amt = ifelse(is.na(apc_income_src11_amt), 0.0, apc_income_src11_amt)
    ,adj_earnings_total = adj_apc_income_src01_amt + adj_apc_income_src02_amt + adj_apc_income_src03_amt
    ,adj_earnings_total = ifelse(adj_earnings_total < 0.0, 0.0, adj_earnings_total)
    ,is_benefit = ifelse(apc_income_src05_amt > 0 | apc_income_src07_amt > 0 
                         | apc_income_src08_amt > 0 | apc_income_src09_amt > 0 
                         | apc_income_src10_amt > 0 | apc_income_src11_amt > 0, 1, 0)
    ,NZDep2018_1_3 = ifelse(as.integer(NZDep2018) >= 1 & as.integer(NZDep2018) <= 3, 1, 0)
    ,NZDep2018_4_5 = ifelse(as.integer(NZDep2018) >= 4 & as.integer(NZDep2018) <= 5, 1, 0)
    ,NZDep2018_6_7 = ifelse(as.integer(NZDep2018) >= 6 & as.integer(NZDep2018) <= 7, 1, 0)
    ,NZDep2018_8_10 = ifelse(as.integer(NZDep2018) >= 8 & as.integer(NZDep2018) <= 10, 1, 0)
    ,dep_index_ind  = ifelse(as.integer(NZDep2018) >= 1 & as.integer(NZDep2018) <= 3, "1_to_3",
                         ifelse(as.integer(NZDep2018) >= 4 & as.integer(NZDep2018) <= 5, "4_to_5",
                                ifelse(as.integer(NZDep2018) >= 6 & as.integer(NZDep2018) <= 7, "6_to_7",
                                       ifelse(as.integer(NZDep2018) >= 8 & as.integer(NZDep2018) <= 10, "8_to_10",NA)
                                      )
                                )
                        )
    ,dep_index_ind = ifelse(is.na(dep_index_ind), "Unk", dep_index_ind)
    ,dep_index_ind = factor(dep_index_ind ,levels = c("8_to_10", "6_to_7", "4_to_5", "1_to_3", "Unk"))
    ,income_zero = adj_earnings_total == 0
    ,income_1_5000 = adj_earnings_total > 0 & adj_earnings_total < 5001
    ,income_5001_10000 = adj_earnings_total >= 5001 & adj_earnings_total < 10001
    ,income_10001_15000 = adj_earnings_total >= 10001 & adj_earnings_total < 15001
    ,income_15001_20000 = adj_earnings_total >= 15001 & adj_earnings_total < 20001
    ,income_20001_25000 = adj_earnings_total >= 20001 & adj_earnings_total < 25001
    ,income_25001_30000 = adj_earnings_total >= 25001 & adj_earnings_total < 30001
    ,income_30001_35000 = adj_earnings_total >= 30001 & adj_earnings_total < 35001
    ,income_35001_40000 = adj_earnings_total >= 35001 & adj_earnings_total < 40001
    ,income_40001_50000 = adj_earnings_total >= 40001 & adj_earnings_total < 50001
    ,income_50001_60000 = adj_earnings_total >= 50001 & adj_earnings_total < 60001
    ,income_60001_70000 = adj_earnings_total >= 60001 & adj_earnings_total < 70001
    ,income_70001_100000 = adj_earnings_total >= 70001 & adj_earnings_total < 100001
    ,income_100001_150000 = adj_earnings_total >= 100001 & adj_earnings_total < 150001
    ,income_150001_or_more = adj_earnings_total >= 150001
    ,is_study_ind = ifelse(apc_study_prtpcn_code != "N", 1, 0)
    ,is_adult = ifelse(apc_age_in_years_nbr >= 18, 1, 0)
    ,age_category = factor(ifelse(apc_age_in_years_nbr < 18.0, "[<18]",
                          ifelse(apc_age_in_years_nbr >= 18.0 & apc_age_in_years_nbr < 22, "[18-22)",
                                 ifelse(apc_age_in_years_nbr >= 22, "[>=22]", "Unk")
                          )
                    ), levels = c("[<18]", "[18-22)", "[>=22]", "Unk"))
    ,age_15_17_ind = ifelse(apc_age_in_years_nbr < 18.0, 1, 0)
    ,age_18_21_ind = ifelse(apc_age_in_years_nbr >= 18.0 & apc_age_in_years_nbr < 22, 1, 0)
    ,age_22above_ind = ifelse(apc_age_in_years_nbr >= 22, 1, 0)
    ,rural_ind = ifelse(urban_rural_ind != "R" | is.na(urban_rural_ind), 0, 1)
    ,urban_ind = ifelse(urban_rural_ind != "U" | is.na(urban_rural_ind), 0, 1)
    ,addr_transience = factor(addr_transience, levels = c("0_1", "2_3", "3plus"))
    ,region_code_combined = factor(ifelse(region_code == "02", "auckland_reg",
                                   ifelse(region_code == "09", "wellingon_reg",
                                          ifelse(region_code == "13", "canterbury_reg",
                                                 ifelse(region_code == "03", "waikato_reg",
                                                        ifelse(region_code == "04", "bayofplenty_reg", "restofnz_reg")
                                                 ))))
                            ,levels = c("auckland_reg", "wellingon_reg", "canterbury_reg", "waikato_reg", "bayofplenty_reg", "restofnz_reg"))
    ,qual_groups = factor(ifelse(apc_hst_qual_code >= 0 & apc_hst_qual_code <= 3, "0-3",
                          ifelse(apc_hst_qual_code >= 4 & apc_hst_qual_code <= 6, "4-6",
                                 ifelse(apc_hst_qual_code >= 7 & apc_hst_qual_code <= 10, "7-10","Unk"
                          ))), levels = c("0-3", "4-6", "7-10", "Unk"))
    )

to_learners_all <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "NONE") %>% mutate(status = ifelse(event =="LEARNER", 1,0))
to_restricted_all <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "LEARNER") %>% mutate(status = ifelse(event =="RESTRICTED", 1,0))
to_full_all <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "RESTRICTED") %>% mutate(status = ifelse(event =="FULL", 1,0))

to_learners_over18 <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "NONE" & apc_age_in_years_nbr > 18) %>% mutate(status = ifelse(event =="LEARNER", 1,0))
to_restricted_over18 <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "LEARNER" & is_adult == 1) %>% mutate(status = ifelse(event =="RESTRICTED", 1,0))
to_full_over18 <- analysisdata %>% filter(nzta_dlr_licence_stage_text == "RESTRICTED" & is_adult == 1) %>% mutate(status = ifelse(event =="FULL", 1,0))


######################## 6. Run KM survival curves for specific variables & combinations ########################

# By Gender univariate
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ snz_sex_gender_code", title = "None to Learners", legend_title = "Sex/Gender")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ snz_sex_gender_code", title = "Learners to Restricted", legend_title = "Sex/Gender")                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ snz_sex_gender_code", title = "Restricted to Full", legend_title = "Sex/Gender") 

# By Ethnicity univariate
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_maori", title = "None to Learners", legend_title = "Maori", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_maori", title = "Learners to Restricted", legend_title = "Maori", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_maori", title = "Restricted to Full", legend_title = "Maori", legend_labs = c("No", "Yes")) 
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_pasifika", title = "None to Learners", legend_title = "Pasifika", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_pasifika", title = "Learners to Restricted", legend_title = "Pasifika", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_pasifika", title = "Restricted to Full", legend_title = "Pasifika", legend_labs = c("No", "Yes")) 
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_asian", title = "None to Learners", legend_title = "Asian", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_asian", title = "Learners to Restricted", legend_title = "Asian", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_asian", title = "Restricted to Full", legend_title = "Asian", legend_labs = c("No", "Yes")) 
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_european", title = "None to Learners", legend_title = "NZ European", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_european", title = "Learners to Restricted", legend_title = "NZ European", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_european", title = "Restricted to Full", legend_title = "NZ European", legend_labs = c("No", "Yes"))
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_melaa", title = "None to Learners", legend_title = "MELAA", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_melaa", title = "Learners to Restricted", legend_title = "MELAA", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_melaa", title = "Restricted to Full", legend_title = "MELAA", legend_labs = c("No", "Yes"))

# Rural
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ rural_ind", title = "None to Learners", legend_title = "Rural/Urban", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ rural_ind", title = "Learners to Restricted", legend_title = "Rural/Urban", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ rural_ind", title = "Restricted to Full", legend_title = "Rural/Urban", legend_labs = c("No", "Yes"))

# On Benefit
km_func(data = to_learners_over18, formula = "Surv(time_to_event, status) ~ is_benefit", title = "None to Learners", legend_title = "On Benefit", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_over18, formula = "Surv(time_to_event, status) ~ is_benefit", title = "Learners to Restricted", legend_title = "On Benefit", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_over18, formula = "Surv(time_to_event, status) ~ is_benefit", title = "Restricted to Full", legend_title = "On Benefit", legend_labs = c("No", "Yes"))


# By Dep Index
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ dep_index_ind", title = "None to Learners", legend_title = "DepIndex", legend_labs = c( "8_to_10", "6_to_7", "4_to_5", "1_to_3", "Unk"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ dep_index_ind", title = "Learners to Restricted", legend_title = "DepIndex", legend_labs = c( "8_to_10", "6_to_7", "4_to_5", "1_to_3", "Unk"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ dep_index_ind", title = "Restricted to Full", legend_title = "DepIndex", legend_labs = c( "8_to_10", "6_to_7", "4_to_5", "1_to_3", "Unk"))

# By Address transience
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ addr_transience", title = "None to Learners", legend_title = "Addr.Change", legend_labs = c("0_1", "2_3", "3plus"))
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ addr_transience", title = "Lnr to Rest", legend_title = "Addr.Change", legend_labs = c("0_1", "2_3", "3plus"))                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ addr_transience", title = "Rest to Full", legend_title = "Addr.Change", legend_labs = c("0_1", "2_3", "3plus"))

# By Young Mothers
km_func(data = to_learners_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind", title = "None to Learners", legend_title = "Young Mothers", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind", title = "Lnr to Rest", legend_title = "Young Mothers", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind", title = "Rest to Full", legend_title = "Young Mothers", legend_labs = c("No", "Yes"))


# By Study
km_func(data = to_learners_over18, formula = "Surv(time_to_event, status) ~ is_study_ind", title = "None to Learners (Over 18)", legend_title = "Study ind", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_over18, formula = "Surv(time_to_event, status) ~ is_study_ind", title = "Learners to Restricted (Over 18)", legend_title = "Study ind", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_over18, formula = "Surv(time_to_event, status) ~ is_study_ind", title = "Restricted to Full (Over 18)", legend_title = "Study ind", legend_labs = c("No", "Yes"))

# By Employed
km_func(data = to_learners_over18, formula = "Surv(time_to_event, status) ~ apc_employed_ind", title = "None to Learners (Over 18)", legend_title = "Employed", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_over18, formula = "Surv(time_to_event, status) ~ apc_employed_ind", title = "Lnr to Rest(18over)", legend_title = "Employed", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_over18, formula = "Surv(time_to_event, status) ~ apc_employed_ind", title = "Rest to Full(18over)", legend_title = "Employed", legend_labs = c("No", "Yes"))



# By Maori & Gender
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_maori + snz_sex_gender_code", title = "None to Learners",
        legend_title = "Maori peoples interaction with gender")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_maori + snz_sex_gender_code", title = "Learners to Restricted",
        legend_title = "Maori peoples interaction with gender")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_maori + snz_sex_gender_code", title = "Restricted to Full",
        legend_title = "Maori peoples interaction with gender")

# By Maori & Dep Code
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_maori + dep_index_ind", title = "None to Learners",
        legend_title = "Maori peoples interaction with deprivation index")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_maori + dep_index_ind", title = "Learners to Restricted",
        legend_title = "Maori peoples interaction with deprivation index")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_maori + dep_index_ind", title = "Restricted to Full",
        legend_title = "Maori peoples interaction with deprivation index")

# By Maori and Urban/Rural
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_maori + rural_ind", title = "None to Learners",
        legend_title = "Maori peoples interaction with urban indicator")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_maori + rural_ind", title = "Learners to Restricted",
        legend_title = "Maori peoples interaction with urban indicator")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_maori + rural_ind", title = "Restricted to Full",
        legend_title = "Maori peoples interaction with urban indicator")

# By Maori and Employment
km_func(data = to_learners_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_maori + apc_employed_ind", title = "None to Learners",
        legend_title = "Maori adults interaction with employment")
km_func(data = to_restricted_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_maori + apc_employed_ind", title = "Learners to Restricted",
        legend_title = "Maori adults interaction with employment")
km_func(data = to_full_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_maori + apc_employed_ind", title = "Restricted to Full",
        legend_title = "Maori adults interaction with employment")


# By Pasifika & Gender
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + snz_sex_gender_code", title = "None to Learners",
        legend_title = "Pasifika peoples interaction with gender")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + snz_sex_gender_code", title = "Learners to Restricted",
        legend_title = "Pasifika peoples interaction with gender")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + snz_sex_gender_code", title = "Restricted to Full",
        legend_title = "Pasifika peoples interaction with gender")

# By Pasifika & Dep Code
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + dep_index_ind", title = "None to Learners",
        legend_title = "Pasifika peoples interaction with deprivation index")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + dep_index_ind", title = "Learners to Restricted",
        legend_title = "Pasifika peoples interaction with deprivation index")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + dep_index_ind", title = "Restricted to Full",
        legend_title = "Pasifika peoples interaction with deprivation index")

# By Pasifika and Urban/Rural
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + rural_ind", title = "None to Learners",
        legend_title = "Pasifika peoples interaction with urban indicator")
km_func(data = to_restricted_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + rural_ind", title = "Learners to Restricted",
        legend_title = "Pasifika peoples interaction with urban indicator")
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ eth_pasifika + rural_ind", title = "Restricted to Full",
        legend_title = "Pasifika peoples interaction with urban indicator")

# By Pasifika and Employment
km_func(data = to_learners_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_pasifika + apc_employed_ind", title = "None to Learners",
        legend_title = "Pasifika adults interaction with employment")
km_func(data = to_restricted_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_pasifika + apc_employed_ind", title = "Learners to Restricted",
        legend_title = "Pasifika adults interaction with employment")
km_func(data = to_full_all %>% filter(apc_age_in_years_nbr > 18), formula = "Surv(time_to_event, status) ~ eth_pasifika + apc_employed_ind", title = "Restricted to Full",
        legend_title = "Pasifika adults interaction with employment")



# By young mothers and dep index
km_func(data = to_learners_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + NZDep2018_8_10", title = "None to Learners", legend_title = "YMDep1_3")
km_func(data = to_restricted_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + NZDep2018_1_3", title = "Lnr to Rest", legend_title = "YMDep1_3")                                                                                         
km_func(data = to_full_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + NZDep2018_1_3", title = "Rest to Full", legend_title = "YMDep1_3")

# By young mothers and benefit ind
km_func(data = to_learners_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + is_benefit", title = "None to Learners", legend_title = "YMDep_benefit")
km_func(data = to_restricted_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + is_benefit", title = "Lnr to Rest", legend_title = "YMDep_benefit")                                                                                         
km_func(data = to_full_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_ind + is_benefit", title = "Rest to Full", legend_title = "YMDep_benefit")

# By Young Mothers under 19
km_func(data = to_learners_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_before_19_ind", title = "None to Learners", legend_title = "Young Mothers before 19")
km_func(data = to_restricted_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_before_19_ind", title = "Lnr to Rest", legend_title = "Young Mothers before 19")                                                                                         
km_func(data = to_full_all %>% filter(snz_sex_gender_code == 'F'), formula = "Surv(time_to_event, status) ~ young_mother_before_19_ind", title = "Rest to Full", legend_title = "Young Mothers before 19")

# by region
km_func(data = to_learners_all, formula = "Surv(time_to_event, status) ~ region_code_combined", title = "None to Learners", legend_title = "Region")
km_func(data = to_restricted_all , formula = "Surv(time_to_event, status) ~ region_code_combined", title = "Lnr to Rest", legend_title = "Region")                                                                                         
km_func(data = to_full_all, formula = "Surv(time_to_event, status) ~ region_code_combined", title = "Rest to Full", legend_title = "Region")


km_func(data = to_learners_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind", title = "None to Learners (Over 18)", legend_title = "Studied or in study ind", legend_labs = c("No", "Yes"))
km_func(data = to_restricted_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind", title = "Learners to Restricted (Over 18)", legend_title = "Studied or in study ind", legend_labs = c("No", "Yes"))                                                                                         
km_func(data = to_full_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind", title = "Restricted to Full (Over 18)", legend_title = "Studied or in study ind", legend_labs = c("No", "Yes"))

# Benefit and in education
km_func(data = to_learners_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind + is_benefit", title = "None to Learners (Over 18)", legend_title = "Benefit & in Education")
km_func(data = to_restricted_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind + is_benefit", title = "Learners to Restricted (Over 18)", legend_title = "Benefit & in Education")                                                                                         
km_func(data = to_full_over18, formula = "Surv(time_to_event, status) ~ studied_or_study_ind + is_benefit", title = "Restricted to Full (Over 18)", legend_title = "Benefit & in Education")




# All - Learners, Restricted, Full
# km_func(data = to_learners_all, formula = "Surv(cumul_time_to_event, status) ~ 1", title = "None to Learners", legend_title = "All")
# km_func(data = to_restricted_all, formula = "Surv(cumul_time_to_event, status) ~ 1", title = "Lnr to Rest", legend_title = "All")                                                                                         
# km_func(data = to_full_all, formula = "Surv(cumul_time_to_event, status) ~ 1", title = "Rest to Full", legend_title = "All")



######################## 7. Run Cox Hazard models on the 3 transitions ########################

cox_model_to_learners <- coxph(Surv(time_to_event, status)~snz_sex_gender_code
                               + eth_maori
                               + eth_european
                               + eth_pasifika
                               + eth_asian
                               + eth_melaa
                               + eth_others
                               + dep_index_ind
                               + rural_ind
                               + region_code_combined
                               + addr_transience
                               + snz_sex_gender_code_x_young_mother_ind
                               ,data = to_learners_all %>% 
                                 mutate(snz_sex_gender_code_x_young_mother_ind = ifelse(snz_sex_gender_code == "F", young_mother_ind, 0))
                               # ,ties = "breslow"
                               )

# summary(cox_model_to_learners)
# cox.zph(cox_model_to_learners)
# ggcoxzph(cox.zph(cox_model_to_learners))
# ggcoxdiagnostics(cox_model_to_learners, type = "dfbeta", linear.predictions = FALSE)
cox_model_to_restricted <- coxph(Surv(time_to_event, status)~
                                   snz_sex_gender_code
                                 + eth_maori
                                 + eth_european
                                 + eth_pasifika
                                 + eth_asian
                                 + eth_melaa
                                 + eth_others
                                 + dep_index_ind
                                 + studied_or_study_ind
                                 + apc_employed_ind
                                 + rural_ind
                                 + region_code_combined
                                 + addr_transience
                                 + time_spent_in_prevstage
                                 + snz_sex_gender_code_x_young_mother_ind
                                 + is_benefit
                                 ,data = to_restricted_all %>% 
                                   mutate(snz_sex_gender_code_x_young_mother_ind = ifelse(snz_sex_gender_code == "F", young_mother_ind, 0))
                                 ,x=TRUE)
# summary(cox_model_to_restricted)
# cox.zph(cox_model_to_restricted)
# ggcoxzph(cox.zph(cox_model_to_restricted))
# ggcoxdiagnostics(cox_model_to_restricted, type = "dfbeta", linear.predictions = FALSE)
cox_model_to_full <- coxph(Surv(time_to_event, status)~
                             snz_sex_gender_code
                           + eth_maori
                           + eth_european
                           + eth_pasifika
                           + eth_asian
                           + eth_melaa
                           + eth_others
                           + dep_index_ind
                           + studied_or_study_ind
                           + apc_employed_ind
                           + rural_ind
                           + region_code_combined
                           + addr_transience
                           + time_spent_in_prevstage
                           + snz_sex_gender_code_x_young_mother_ind
                           + age_category
                           + is_benefit
                           ,data = to_full_all  %>% 
                             mutate(snz_sex_gender_code_x_young_mother_ind = ifelse(snz_sex_gender_code == "F", young_mother_ind, 0))
                           ,ties = "breslow")

# summary(cox_model_to_full)
# cox.zph(cox_model_to_full)
# ggcoxzph(cox.zph(cox_model_to_full))
# ggcoxdiagnostics(cox_model_to_full, type = "dfbeta", linear.predictions = FALSE)

# The baseline suvival function
# ggsurvplot(survfit(cox_model_to_learners), data = to_learners_all, ggtheme = theme_bw(), conf.int = TRUE, surv.median.line = "hv")


# cox_model_survfit_batch(model = cox_model_to_learners, df = to_learners_all
#                         ,editvalues = list(snz_sex_gender_code =  c("F", "M"))
#                         ,filelabel = "../output/png/expected_survival_gender"
#                         ,title = "None to Learners"
#                         ,legend_title = "Gender", legend_labs = c("F", "M")
#                         )
# cox_model_survfit_batch(model = cox_model_to_restricted, df = to_restricted_all
#                         ,editvalues = list(snz_sex_gender_code =  c("F", "M"))
#                         ,filelabel = "../output/png/expected_survival_gender"
#                         ,title = "Learners to Restricted"
#                         ,legend_title = "Gender", legend_labs = c("F", "M")
# )
# cox_model_survfit_batch(model = cox_model_to_learners, df = to_full_all
#                         ,editvalues = list(snz_sex_gender_code =  c("F", "M"))
#                         ,filelabel = "../output/png/expected_survival_gender"
#                         ,title = "Restricted to Full"
#                         ,legend_title = "Gender", legend_labs = c("F", "M")
# )




cox_output <- rbind(as.data.frame(summary(cox_model_to_learners)$coefficients) %>% mutate(regtype = "cox_model_to_learners"
                                                                      ,rowcount = cox_model_to_learners$n)
      ,as.data.frame(summary(cox_model_to_restricted)$coefficients) %>% mutate(regtype = "cox_model_to_restricted"
                                                                             ,rowcount = cox_model_to_restricted$n)
      ,as.data.frame(summary(cox_model_to_full)$coefficients) %>% mutate(regtype = "cox_model_to_full"
                                                                               ,rowcount = cox_model_to_full$n)
)

write.csv(x = cox_output
          ,file = paste0("../output/csv/cox_regression_output.csv")
          ,row.names = TRUE
)
