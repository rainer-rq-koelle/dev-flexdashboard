# AIRPORT FLEXDASHBOARD RENDER SCRIPT
# This scrips reads in the data files (i.e. monthly files currently stored on
# ansperformance.eu) and additional data summaries. These input files are
# currently saved to the project folder for the script to read.


# To-Do
# 1. processing pipeline read/extract data from PRISME to remove by hand download
# 1a. download data from blog portal repo: 
#    https://github.com/euctrl-pru/portal/tree/master/static/download/xls
# 2. reduce data load by cleaning the input data tables
#    note: as the dashboard is under development this will be a clean-up task
#    once it is known what the "minimal needed payload data" is
# 3. post-processing: rendered boards are stored as html-pages in the boards
#    sub-folder

# load required packages
library("dplyr")
library("lubridate")
library("purrr")
library("readxl")

library(formattable)
library(sparkline)

# REMOVE WHEN DEPLOYED -------------------------------------------------
# TO LIMIT TEST LOAD SUBSET FOR "TEST" AIRPORTS
# To-Do: deployed version should use airport list, etc or be derived from ids
# the list is used as an iterator for the render loop below
apts   <- c("EGLL", "EDDF", "EIDW")
# version - counter increased to 05 (including Sara's comments) & discussion with
# Enrico
version<- "05"
## ------------------------------------------------ REMOVE WHEN DEPLOYED


## ------------ READ IN DATA TABLES FROM DOWNLOAD POINT ------------------

# traffic counts and ids
tfc_df <- readxl::read_excel("./data/Airport_Traffic.xlsx", sheet = "DATA"
                             # readxl style to force all chr # , col_types = "text" #
) %>%
  mutate(FLT_DATE = lubridate::date(FLT_DATE))

ids_df <- tfc_df %>% select(apt = APT_ICAO, name = APT_NAME, state = STATE_NAME)

thru_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_THRU.csv") %>% rename(APT_ICAO = APT)

atfm_df <- readxl::read_excel("./data/Airport_Arrival_ATFM_Delay.xlsx", sheet = "DATA") %>%
  mutate(FLT_DATE = lubridate::date(FLT_DATE)) %>%
  mutate_at(vars(starts_with("DLY_APT_ARR_")), tidyr::replace_na, 0) %>%
  mutate( AD_DISRUPTION = DLY_APT_ARR_A_1 + DLY_APT_ARR_E_1 + DLY_APT_ARR_N_1 +
            DLY_APT_ARR_O_1 + DLY_APT_ARR_NA_1
          ,AD_CAPACITY   = DLY_APT_ARR_G_1 + DLY_APT_ARR_M_1 + DLY_APT_ARR_R_1 +
            DLY_APT_ARR_V_1
          ,AD_WEATHER    = DLY_APT_ARR_D_1 + DLY_APT_ARR_W_1
          ,AD_DISRUPTION_ATC = DLY_APT_ARR_I_1 + DLY_APT_ARR_T_1
          ,AD_CAPACITY_ATC   = DLY_APT_ARR_C_1
          ,AD_STAFFING_ATC   = DLY_APT_ARR_S_1
          ,AD_EVENTS     = DLY_APT_ARR_P_1
  )

# idea was to read out the meta data to map causes to groups
#atfm_reg <- readxl::read_excel("./data-test/Airport_Arrival_ATFM_Delay.xlsx", sheet = "META"
#                               , skip = 8  # skip first rows
#                               ) %>%
#  select(1, 4) %>%
#  filter(!is.na(`Reason Group`))

slot_df <- readxl::read_excel("./data/ATFM_Slot_Adherence.xlsx", sheet = "DATA")

asma_df <- readxl::read_excel("./data/ASMA_Additional_Time.xlsx", sheet = "DATA")

txot_df <- readxl::read_excel("./data-test/Taxi-Out_Additional_Time.xlsx", sheet = "DATA")

txit_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TXIT.csv") %>% rename(APT_ICAO = APT)

pddly_df<- readxl::read_excel("./data-test/STAT_AIRPORT_DATA.xlsx", sheet = "DATA") %>%
  select(APT_ICAO, APT_IATA, YEAR, MONTH_NUM, TOTAL_DLY_89, TOTAL_DLY_999, TOTAL_DLY_ZZZ
         ,TOTAL_DLY_OTHER, TOTAL_UN_RPTED_DLY, TOTAL_OV_RPTED_DLY)

turn_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TURN.csv") %>% rename(APT_ICAO = APT)

punc_df <-  readr::read_csv("./data-test/STAT_AIRPORT_DATA_PUNC.csv") %>% rename(APT_ICAO = APT)


## ------------ UTILITY FUNCTIONS -------------------------------------

filter_df_by_apt <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt)
}

pick_apt_name <- function(.df, .apt){
  name <- .df %>% filter(APT_ICAO == .apt)
  name <- name$APT_NAME[1]
}

pick_state_name <- function(.df, .apt){
  state <- .df %>% filter(APT_ICAO == .apt)
  state <- state$STATE_NAME[1]
}

pick_apt_iata <- function(.df, .apt){
  iata <- .df %>% filter(APT_ICAO == .apt)
  iata <- iata$APT_IATA[1]
}

## ------------ RENDER DASHBOARDS -------------------------------------


apts %>%
  purrr::walk(
    .f=~rmarkdown::render(
      input  = "apt_dashboard_04.Rmd"   # master flexdashboard Rmd
      , params = list( #------ start params -------------------------
                       icao = .
                       ,iata = pick_apt_iata( pddly_df,  .apt = .)   # merge iata code with other source
                       ,name = pick_apt_name(   tfc_df,  .apt = .)
                       ,state= pick_state_name( tfc_df,  .apt = .)
                       ,tfc  = filter_df_by_apt(tfc_df,  .apt = .)
                       ,thru = filter_df_by_apt(thru_df, .apt = .)
                       ,atfm = filter_df_by_apt(atfm_df, .apt = .)
                       ,slot = filter_df_by_apt(slot_df, .apt = .)
                       ,asma = filter_df_by_apt(asma_df, .apt = .)
                       ,txot = filter_df_by_apt(txot_df, .apt = .)
                       ,txit = filter_df_by_apt(txit_df, .apt = .)
                       ,pddly= filter_df_by_apt(pddly_df,.apt = .)
                       ,turn = filter_df_by_apt(turn_df, .apt = .)
                       ,punc = filter_df_by_apt(punc_df, .apt = .)
      ) #----------------- end params ---------------------------
      # output_dir DEACTIVATED and included in output_file name
      # brittle as reported in stackoverflow
      #  , output_dir = "./boards"
      , output_file = paste0("./boards/", .,"-", version,".html")
    )
  )
