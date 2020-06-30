# AIRPORT FLEXDASHBOARD RENDER SCRIPT
# This scrips reads in the data files (i.e. monthly files currently stored on
# ansperformance.eu) and additional data summaries. These input files are
# currently saved to the project folder for the script to read.
#
#
# To-Do
# 1. processing pipeline read/extract data from PRISME to remove by hand download
# 1a. download data from blog portal repo:
#    https://github.com/euctrl-pru/portal/tree/master/static/download/xls
# 2. reduce data load by cleaning the input data tables
#    note: as the dashboard is under development this will be a clean-up task
#    once it is known what the "minimal needed payload data" is
# 3. post-processing: rendered boards are stored as html-pages in the boards
#    sub-folder
#
# load required packages
library("dplyr")
library("lubridate")
library("purrr")
library("readxl")
library(formattable)
library(sparkline)

min_year <- 2016 # by definition 5 years, i.e. 2016-2020

# REMOVE WHEN DEPLOYED -------------------------------------------------
# TO LIMIT TEST LOAD SUBSET FOR "TEST" AIRPORTS
# To-Do: deployed version should use airport list, etc or be derived from ids
# the list is used as an iterator for the render loop below
#apts   <- c(, "EDDF", "EIDW")
apts <- c("EGLL","EBBR", "LEMD")
# version - counter increased to 05 (including Sara's comments) & discussion with
# Enrico --> deactivated as we move towards deployment. version<- "05"
## ------------------------------------------------ REMOVE WHEN DEPLOYED
#
#
## ------------ READ IN DATA TABLES FROM DOWNLOAD POINT ------------------
#
# Thierry's dashboard table
db_df <- readr::read_csv2("./data/STAT_AIRPORT_MONTHLY.csv")

db_conf <- readr::read_csv2("./data/STAT_AIRPORT_CONFIGURATION.csv")

# traffic counts and ids
tfc_df <- readxl::read_excel("./data/Airport_Traffic.xlsx", sheet = "DATA"
                             # readxl style to force all chr # , col_types = "text" #
) %>%
  mutate(FLT_DATE = lubridate::date(FLT_DATE))

ids_df <- tfc_df %>% select(apt = APT_ICAO, name = APT_NAME, state = STATE_NAME)

config_df <- db_conf %>%
  na.omit() %>%    # currently only 2019 data, rest NA
  select(APT_ICAO, YEAR, CONFIGURATION, SHARE_PCT) %>%
  arrange(desc(SHARE_PCT))

thru_df <- readr::read_csv2("./data/STAT_AIRPORT_THROUGHPUT.csv") %>% rename(APT_ICAO = AIRPORT)

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
  ) %>%
  # trim payload
  select( APT_ICAO, YEAR, MONTH_NUM, FLT_DATE, FLT_ARR_1, DLY_APT_ARR_1
         ,starts_with("AD_"), FLT_ARR_1_DLY, FLT_ARR_1_DLY_15)

# idea was to read out the meta data to map causes to groups
#atfm_reg <- readxl::read_excel("./data-test/Airport_Arrival_ATFM_Delay.xlsx", sheet = "META"
#                               , skip = 8  # skip first rows
#                               ) %>%
#  select(1, 4) %>%
#  filter(!is.na(`Reason Group`))
#
slot_df <- readxl::read_excel("./data/ATFM_Slot_Adherence.xlsx", sheet = "DATA")

asma_df <- readxl::read_excel("./data/ASMA_Additional_Time.xlsx", sheet = "DATA")

txot_df <- readxl::read_excel("./data/Taxi-Out_Additional_Time.xlsx", sheet = "DATA")

#txit_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TXIT.csv") %>% rename(APT_ICAO = APT)
txit_df <- db_df %>%
  select( APT_ICAO, YEAR, MONTH_NUM
         ,N_SMPL  = NB_TAXI_IN_FL
         ,TOT_REF = TAXI_IN_REF_TIME_MIN
         ,TOT_ADD_TXIT = ADD_TAXI_IN_TIME_MIN)

#pddly_df<- readxl::read_excel("./data-test/STAT_AIRPORT_DATA.xlsx", sheet = "DATA") %>%
#  select(APT_ICAO, APT_IATA, YEAR, MONTH_NUM, APT_FLT_DEP
#         ,TOTAL_DLY_89, TOTAL_DLY_999, TOTAL_DLY_ZZZ
#         ,TOTAL_DLY_OTHER, TOTAL_UN_RPTED_DLY, TOTAL_OV_RPTED_DLY)
pddly_df <- db_df %>%
  select(APT_ICAO, YEAR, MONTH_NUM, APT_FLT_DEP = NB_DLY_DEP_FL
         ,TOTAL_DLY_89 = DLY_89_MIN, TOTAL_DLY_999 = DLY_999_MIN
         ,TOTAL_DLY_ZZZ= DLY_ZZZ_MIN
         ,TOTAL_DLY_OTHER = DLY_OTHER_MIN
         ,TOTAL_UN_RPTED_DLY = UN_RPTED_DLY_MIN
         ,TOTAL_OV_RPTED_DLY = OV_RPTED_DLY_MIN
         )

#turn_df <- readr::read_csv("./data-test/STAT_AIRPORT_DATA_TURN.csv") %>% rename(APT_ICAO = APT)
turn_df <- readr::read_csv2("./data/STAT_AIRPORT_TURN_AROUND.csv") %>%
  select(APT_ICAO, YEAR, MONTH_NUM, AC_CLASS
         ,N = NB_TURN_ARROUND
         ,TOT_SDTT_MIN, TOT_ACTT_MIN, TOT_ADTT_MIN) %>%
  mutate(AVG_ATTT = TOT_ACTT_MIN / N, AVG_STTT = TOT_SDTT_MIN / N) %>%
  select(APT_ICAO, YEAR, MONTH = MONTH_NUM, AC_CLASS, N, AVG_ATTT, AVG_STTT)

punc_df <-  readr::read_csv("./data-test/STAT_AIRPORT_DATA_PUNC.csv") %>% rename(APT_ICAO = APT)

covid_df <- readr::read_csv("./data/COVID_AIRPORT.csv")

## ------------ UTILITY FUNCTIONS -------------------------------------
#
filter_df_by_apt <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt) %>%
    filter(YEAR >= min_year)  # ensure only 5 years of data
}
#
pick_apt_name <- function(.df, .apt){
  name <- .df %>% filter(APT_ICAO == .apt)
  name <- name$APT_NAME[1]
}
#
pick_state_name <- function(.df, .apt){
  state <- .df %>% filter(APT_ICAO == .apt)
  state <- state$STATE_NAME[1]
}
#
pick_apt_iata <- function(.df, .apt){
  iata <- .df %>% filter(APT_ICAO == .apt)
  iata <- iata$APT_IATA[1]
}
#
landing_page_indicators <- function(.df=db_df, .atfm=atfm_df, .apt){
  inds <- .df %>% filter(APT_ICAO == .apt)

  ind_tfc_2019 <- inds %>%
    select(APT_ICAO, YEAR, NB_NM_TOT) %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise(NB_NM_TOT = sum(NB_NM_TOT, na.rm = TRUE)) %>% ungroup()

  ind_txot_2019 <- inds %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise( ADD_TAXI_OUT_TIME_MIN = sum(ADD_TAXI_OUT_TIME_MIN, na.rm = TRUE)
              ,NB_TAXI_OUT_FL        = sum(NB_TAXI_OUT_FL,        na.rm = TRUE)
              )%>% ungroup() %>%
    mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2) ) %>%
    select(AVG_ADD_TXOT)

  ind_asma_2019 <- inds %>% filter(YEAR == 2019) %>%
    group_by(APT_ICAO, YEAR) %>%
    summarise( ADD_ASMA_TIME_MIN = sum(ADD_ASMA_TIME_MIN, na.rm = TRUE)
               ,NB_ASMA_FL       = sum(NB_ASMA_FL,        na.rm = TRUE)
    )%>% ungroup() %>%
    mutate(AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL,2) ) %>%
    select(AVG_ADD_ASMA)

  ind_atfm_2019 <- .atfm %>% filter(APT_ICAO == .apt, YEAR == 2019) %>%
    select(FLT_ARR_1, DLY_APT_ARR_1) %>%
    summarise( FLT_ARR_1     = sum(FLT_ARR_1,     na.rm = TRUE)
              ,DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
    mutate(AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1,2) ) %>%
    select(AVG_ARR_ATFM)

  out <- ind_tfc_2019 %>%
    bind_cols(ind_txot_2019, ind_asma_2019, ind_atfm_2019)
}
#
latest_month_indicators <- function(.df=db_df, .atfm=atfm_df, .apt){
  inds <- .df %>% filter(APT_ICAO == .apt)

  mth_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  ind_tfc_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, NB_NM_TOT) %>% na.omit() %>%
    filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate( MONTH = mth_name[MONTH_NUM]
           ,  TFC = paste0(NB_NM_TOT," (", MONTH, " ", YEAR,")")
           ) %>%
    select(APT_ICAO, TFC)

  ind_txot_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, ADD_TAXI_OUT_TIME_MIN, NB_TAXI_OUT_FL) %>%
    na.omit() %>% filter(YEAR == max(YEAR) ) %>%
    filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate(AVG_ADD_TXOT = round(ADD_TAXI_OUT_TIME_MIN / NB_TAXI_OUT_FL,2)
           ,AVG_ADD_TXOT= paste0(AVG_ADD_TXOT," (", mth_name[MONTH_NUM], " ", YEAR,")")
           ) %>%
    select(AVG_ADD_TXOT)

  ind_asma_lm <- inds %>%
    select(APT_ICAO, YEAR, MONTH_NUM, ADD_ASMA_TIME_MIN, NB_ASMA_FL) %>%
    na.omit() %>% filter(YEAR == max(YEAR) ) %>%
    filter(MONTH_NUM == max(MONTH_NUM)) %>%
    mutate( AVG_ADD_ASMA = round(ADD_ASMA_TIME_MIN / NB_ASMA_FL, 2)
           ,AVG_ADD_ASMA = paste0(AVG_ADD_ASMA," (", mth_name[MONTH_NUM], " ", YEAR,")")
           ) %>%
    select(AVG_ADD_ASMA)

  ind_atfm_lm <- .atfm %>% filter(APT_ICAO == .apt) %>%
    select(YEAR, MONTH_NUM, FLT_ARR_1, DLY_APT_ARR_1) %>%
    filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    na.omit() %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise( FLT_ARR_1     = sum(FLT_ARR_1, na.rm = TRUE)
              ,DLY_APT_ARR_1 = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>%
    mutate( AVG_ARR_ATFM = round(DLY_APT_ARR_1 / FLT_ARR_1, 2)
           ,AVG_ARR_ATFM = paste0(AVG_ARR_ATFM," (", mth_name[MONTH_NUM], " ", YEAR,")")) %>%
    select(AVG_ARR_ATFM)

  inds_lm <- ind_tfc_lm %>%
    bind_cols(ind_txot_lm, ind_asma_lm, ind_atfm_lm)

}
#
trim_covid <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt) %>%
    select(Day, FLTS_2020, FLTS_2019, MOV_AVG_WK)
}
#
pack_thru <- function(.df, .apt){
  df <- .df %>% filter(APT_ICAO == .apt) %>%
    mutate(DATE = lubridate::dmy(DAY, tz="UTC")
           ,YEAR = year(DATE), MONTH_NUM = month(DATE)
           , WEEKDAY = lubridate::wday(DATE, label=TRUE)) %>%
    filter(YEAR == max(YEAR)) %>% filter(MONTH_NUM == max(MONTH_NUM)) %>%
    select(APT_ICAO, YEAR, MONTH_NUM, DATE, WEEKDAY, TIME, ROLLING_HOUR_MVT, PHASE) %>%
    group_by(YEAR, MONTH_NUM, TIME, PHASE) %>% summarise(ROLLING_HOUR_MVT = mean(ROLLING_HOUR_MVT)) %>%
    ungroup()
}
#
## ------------ RENDER DASHBOARDS -------------------------------------


apts %>%
  purrr::walk(
    .f=~rmarkdown::render(
      input  = "apt_dashboard_05.Rmd"   # master flexdashboard Rmd
      , params = list( #------ start params -------------------------
                        icao  = .
                       ,iata  = pick_apt_iata(   db_df ,   .apt = .)   # merge iata code with other source
                       ,name  = pick_apt_name(   tfc_df,   .apt = .)
                       ,state = pick_state_name( tfc_df,   .apt = .)
                       ,config= filter_df_by_apt(config_df,.apt = .)
                       ,ldgsum= landing_page_indicators(db_df, atfm_df, .apt = .)
                       ,latest= latest_month_indicators(db_df, atfm_df, .apt = .)
                       ,covid = trim_covid(      covid_df, .apt = .)
                       ,tfc   = filter_df_by_apt(tfc_df,   .apt = .)
                       ,thru  = pack_thru(       thru_df,  .apt = .)
                       ,atfm  = filter_df_by_apt(atfm_df,  .apt = .)
                       ,slot  = filter_df_by_apt(slot_df,  .apt = .)
                       ,asma  = filter_df_by_apt(asma_df,  .apt = .)
                       ,txot  = filter_df_by_apt(txot_df,  .apt = .)
                       ,txit  = filter_df_by_apt(txit_df,  .apt = .)
                       ,pddly = filter_df_by_apt(pddly_df, .apt = .)
                       ,turn  = filter_df_by_apt(turn_df,  .apt = .)
                       ,punc  = filter_df_by_apt(punc_df,  .apt = .)
      ) #----------------- end params ---------------------------
      # output_dir DEACTIVATED and included in output_file name
      # brittle as reported in stackoverflow
      #  , output_dir = "./boards"
      #, output_file = paste0("./boards/", ., ".html")
      , output_file = paste0("./docs/", ., ".html")
    )
  )


