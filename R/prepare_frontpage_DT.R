#' utility function to preprocess the data for the front page DT
#' ##############################################################

prepare_front_page_DT <- function(.icao, .tfc, .asma, .txot, .atfm){
  library(tidyverse)
  library(DT)
  library(formattable)
  library(sparkline)
 
  icao <- .icao 
  tfc  <- .tfc
  asma <- .asma
  txot <- .txot
  atfm <- .atfm
  
  # utility function to extract last 12 month for overview table
  extract_last_months <- function(
    df
    ,n_months = 24
    ,now_year = lubridate::year( Sys.Date()) 
    ,now_month= lubridate::month(Sys.Date()) -2
  ){
    df <- df %>% 
      filter(MOF <= paste0(now_year, "-", now_month)) %>% 
      arrange(desc(MOF)) %>%
      head(n_months)
    
    if(!is.integer(df$YEAR))     {df$YEAR      <- as.integer(df$YEAR)}
    if(!is.integer(df$MONTH_NUM)){df$MONTH_NUM <- as.integer(df$MONTH_NUM)}
    return(df)
  }
  
  tfc2 <- tfc %>% filter(APT_ICAO == icao) %>%
    select(YEAR, MONTH_NUM, FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2) %>% 
    group_by(YEAR, MONTH_NUM) %>% 
    summarise(FLT_TOT = sum(FLT_TOT_IFR_2, na.rm = TRUE)) %>% 
    mutate(MOF = paste0(YEAR,"-", sprintf("%02s", MONTH_NUM) )) %>%
    extract_last_months()
  
  asma2<- asma %>% filter(APT_ICAO == icao) %>% 
    select(YEAR, MONTH_NUM, ADD_ASMA = TIME_ASMA_ADD_2, N_ASMA = FLT_ASMA_UNIMP_2) %>%
    mutate( MOF = paste0(YEAR,"-", sprintf("%02d", MONTH_NUM)) ) %>%
    extract_last_months()
  
  txot2<- txot %>% filter(APT_ICAO == icao) %>%
    select(YEAR, MONTH_NUM, ADD_TXOT = TIME_TXO_ADD_2, N_TXOT = FLT_TXO_UNIMP_2) %>%
    mutate( MOF = paste0(YEAR,"-", sprintf("%02d", MONTH_NUM)) ) %>%
    extract_last_months()
  
  atfm2<- atfm %>% filter(APT_ICAO == icao) %>%
    select(YEAR, MONTH_NUM, ATFM_ADLY = DLY_APT_ARR_1, N_ATFM_A = FLT_ARR_1) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(ATFM_ADLY = sum(ATFM_ADLY, na.rm = TRUE), N_ATFM_A = sum(N_ATFM_A, na.rm = TRUE)) %>%
    mutate( MOF = paste0(YEAR,"-", sprintf("%02d", MONTH_NUM)) ) %>%
    extract_last_months()
  
  #message(paste0("Results for "
  #               , now_month =lubridate::month(Sys.Date()) -2, "-"
  #               , now_year  =lubridate::year( Sys.Date())
  #               )
  #        )
  
  perf <- tfc2 %>% 
    left_join(asma2, by=c("MOF","YEAR","MONTH_NUM")) %>% 
    left_join(txot2, by=c("MOF","YEAR","MONTH_NUM")) %>%
    left_join(atfm2, by=c("MOF","YEAR","MONTH_NUM")) %>%
    ungroup
  
  calc_current_month <- function(.perf){
    df <- .perf %>% 
      mutate(
        DIFF_FLT      = FLT_TOT - lead(FLT_TOT)
        ,DIFF_FLT_P   = DIFF_FLT / lead(FLT_TOT)
        ,AVG_ADD_ASMA = ADD_ASMA / N_ASMA
        ,DIFF_ASMA    = AVG_ADD_ASMA - lead(AVG_ADD_ASMA)
        ,DIFF_ASMA_P  = DIFF_ASMA / lead(AVG_ADD_ASMA)
        ,AVG_ADD_TXOT = ADD_TXOT / N_TXOT
        ,DIFF_TXOT    = AVG_ADD_TXOT - lead(AVG_ADD_TXOT)
        ,DIFF_TXOT_P  = DIFF_TXOT / lead(AVG_ADD_TXOT)
        ,AVG_ATFM_ADLY= ATFM_ADLY / N_ATFM_A
        ,DIFF_ATFM_A  = AVG_ATFM_ADLY - lead(AVG_ATFM_ADLY)
        ,DIFF_ATFM_A_P= DIFF_ATFM_A / lead(AVG_ATFM_ADLY)
      ) %>%
      select(YEAR, MONTH_NUM, MOF
             , FLT_TOT, DIFF_FLT, DIFF_FLT_P
             , AVG_ADD_ASMA, DIFF_ASMA , DIFF_ASMA_P
             , AVG_ADD_TXOT, DIFF_TXOT , DIFF_TXOT_P
             , AVG_ATFM_ADLY,DIFF_ATFM_A,DIFF_ATFM_A_P
      ) %>%
      mutate_if(is.double, round, 3)     # round all doubles to n digits
    
    df$CHECK <- NA
    df$CHECK[1] <- "NOW"
    df$CHECK[13]<- "AGO"
    return(df)
  }
  
  perf2 <- perf %>% calc_current_month()
  
  # extract sparklines, reverse order, i.e. rev(), to have timeline correct
  tfc_spk   <- sparkline::spk_chr(rev(perf2$FLT_TOT[1:13]))
  asma_spk  <- sparkline::spk_chr(rev(perf2$AVG_ADD_ASMA[1:13]), type="bar")
  txot_spk  <- sparkline::spk_chr(rev(perf2$AVG_ADD_TXOT[1:13]), type="bar")
  atfm_spk  <- sparkline::spk_chr(rev(perf2$AVG_ATFM_ADLY[1:13]), type="line")
  
  spark_tbl <- tibble(
    IND = c("TFC"  ,"ASMA"   ,"TXOT"   ,"ATFM_A")
    ,SPK = c(tfc_spk, asma_spk, txot_spk, atfm_spk)
  )
  
  r_tfc <- perf2 %>% filter(CHECK == "NOW") %>% mutate(IND = "TFC") %>%
    select(IND, CURRENT = FLT_TOT, CHANGE_M2M_P = DIFF_FLT_P)
  
  r_asma<- perf2 %>% filter(CHECK == "NOW") %>% mutate(IND = "ASMA") %>%
    select(IND, CURRENT = AVG_ADD_ASMA, CHANGE_M2M_P = DIFF_ASMA_P)
  
  r_txot<- perf2 %>% filter(CHECK == "NOW") %>% mutate(IND = "TXOT") %>%
    select(IND, CURRENT = AVG_ADD_TXOT, CHANGE_M2M_P = DIFF_TXOT_P)
  
  r_atfm<- perf2 %>% filter(CHECK == "NOW") %>% mutate(IND = "ATFM_A") %>%
    select(IND, CURRENT = AVG_ATFM_ADLY, CHANGE_M2M_P = DIFF_ATFM_A_P)
  
  IND <- c("TFC", "ASMA","TXOT","ATFM_A")
  NAM <- c("Monthly traffic", "Average additional ASMA time [min/arr]"
           ,"Average additional taxi-out time [min/dep]"
           ,"Average arrival ATFM delay [min/arr]"
  )
  nam <- tibble(IND = IND, NAM = NAM)
  
  out <- bind_rows(r_tfc, r_asma, r_txot, r_atfm)
  out <- out %>% left_join(spark_tbl, by="IND")
  out <- out %>% left_join(nam, by="IND")
  out <- out %>% select(NAM, CURRENT, CHANGE_M2M_P, SPK)
  
  return(out)
}