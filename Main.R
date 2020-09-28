#### Nigeria LTFU Partner Collection Tool Generation
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 4/29/2019

library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
#library(compareDF)

source("FileRead.R")
source("Archive_Continue_LTFU.R")
source("PartnerToolGenerator.R")

date <- format(Sys.Date(), format =  "%Y_%m_%d")
data_pull_date <- "2020-09-21" # Current Data_Pull Date

######################## 1) Archive/Continue Determination ######################## 
## a) Import New LTFU's & Updated LTFU's and Append
df_ndr <- ndr_wrangle("./UMB/NEWLTFU_09222020.xlsx", # New LTFU File
                        "./UMB/Continue_LTFU_2020_09_22.xlsx", # Updated LTFU File
                        data_pull_date
                        )

# Any dupes
dupe_df <- df_ndr %>% 
  group_by(SITE_PID,FACILITY_UID) %>% 
  filter(n()>1) %>%
  ungroup()

# Same Inactive Dates
dupedate_df <- df_ndr %>% 
  group_by(SITE_PID,FACILITY_UID,INACTIVE_DATE) %>% 
  filter(n()>1)


## b) Archive Updated LTFU's that have RETURN_VALIDATE "Yes" or Died Date or > 6months
# (into Folder Historical_LTFU > Stage2)
## c) Archive Final Dataset for Next Update (Most recent inactive date taken)
# (into Folder Continue_LTFU)
df_cleanNDR <- continue_determine(df_ndr)


## d) Import Partner Current Submissions
# (from Folder Submissions)
df_partner <- import_partnersubmissions()

  
## e) Merge Partner Entry to NDR
df_merged <- merge_ndrdf_partnersub(df_cleanNDR, df_partner)

dupe_df2 <- df_merged %>% 
  group_by(SITE_PID,FACILITY_UID) %>% 
  filter(n()>1)

## In case, partner submission tools are duplicating linelist (df_merged > df_cleanNDR)
df_final <- df_merged %>%
  group_by(SITE_PID, FACILITY_UID) %>%
  arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
  slice(1L) %>%
  ungroup()

######################## 2) Generate Partner Tools ########################
## a) Create Partner Tools
# (into Folder New Tools)
generatetools(df_final)

df_final %>% group_by(IMPLEMENTING_PARTNER,STATE)%>% summarise(n=n())
