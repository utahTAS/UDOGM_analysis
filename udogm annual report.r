# 
# 
# 
#           
#           data pull and report out for annual emissions.
#           
#           this is meant to pull the current data from UDOGM and then generate 
#           a report for a annual time period starting 1 year and 6 months in 
#           the past and going till 6 months in the past
# 
#           
#####
## see how long this process takes
system.time({

#####
## requried libraries

library(tidyverse)
library(lubridate)
library(writexl)

#####
## function for pulling tables from the UDOGM webpage

udogm_data_pull <- function(url, file){    
    temp <- tempfile()
    download.file(url, temp)
    data <- read.csv(unz(temp, file),
                     stringsAsFactors = F,
                     na.strings = 'NULL',
                     colClasses = 'character')
    unlink(temp)
    as.tibble(data)
}

#####
##  relevant tables from UDOGM

wells <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Wells.zip',
                         "Wells.csv")

production <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Production2015To2020.zip',
                'Production2015To2020.csv')

#####
## set date range of interest based on the date the report was pulled from UDOGM
## these dates will go back to the beginning of the month in the case of the 
## start date and the end of the month for the end date: one full year.

date_report_ran <- Sys.Date()

# begin report period 1 year and 6 months before the present (lag due to DOGM report delays)
#start_report_window <- (date_report_ran %m-% months(18)) - day(date_report_ran) +1
# analysis period == 1 year (12 mo)
#end_report_window <- date_report_ran %m-% months(6) - day(date_report_ran)

# report period for 2017 annual report
start_report_window <- as.Date("2017-01-01")
end_report_window <- as.Date("2017-12-01")

#####
##  join tables and first round mutate
#attempt to make more specific tag (unique ID)

report_0 <- production %>%
  left_join(., wells) %>%    
  mutate(ReportPeriod = mdy(ReportPeriod),
         Received = mdy(Received),
         DaysProd = as.numeric(DaysProd),
         Oil = as.numeric(Oil),
         Gas = as.numeric(Gas),
         Water = as.numeric(Water),
         Ground_Elev = as.numeric(Ground_Elev),
         Kelly_Elev = as.numeric(Kelly_Elev),
         Floor_Elev = as.numeric(Floor_Elev),
         ConfRelDate = ymd_hms(ConfRelDate),
         TotCum_Oil = as.numeric(TotCum_Oil),
         TotCum_Gas = as.numeric(TotCum_Gas),
         TotCum_Water = as.numeric(TotCum_Water),
         #tag = paste(API, WellBore, FormationName, sep = '_')
         tag = paste(API, WellBore, FormationName, FieldName, Operator, sep = '_')
         )

#####
##  filter down to the period/type of interest.
report_1 <- report_0 %>%
    filter(ReportPeriod >= start_report_window & ReportPeriod <= end_report_window,
           WellType == 'GW' | WellType == 'OW',
           WellStatus == 'S' | WellStatus == 'P')

#####
## checking if there are more than 12 months of production
## establishing well type by first well type and then
## break ties: wells showing 6 mo. prod as "OW" and 6 mo. prod as "GW" will be 
## categorized by which had more oil or gas production. gas production was
## converted to bbl equivilant. 

t1 <- report_1 %>%
    group_by(tag) %>%
    summarize(months_production = n(),
              gw = sum(WellType=='GW'),
              ow = sum(WellType=='OW'),
              gas_bbl = sum(Gas*0.1781076, na.rm = T),
              oil_bbl = sum(Oil, na.rm = T),
              well_type = ifelse(gw > ow, 'Gas Well',
                                 ifelse(ow > gw, 'Oil Well',
                                        ifelse(ow==gw & gas_bbl > oil_bbl,
                                               'Gas Well',
                                               ifelse(ow==gw & oil_bbl > gas_bbl,
                                                      'Oil Well', 
                                                      'Weird')
                                               )
                                        )
                                 )
              ) %>%
    select(tag, months_production, well_type)

#added CountyName to final report 
report_2 <- left_join(report_1, t1) %>%
    mutate(Condensate = ifelse(well_type == 'Gas Well', Oil, NA),
           Oil = ifelse(well_type == 'Oil Well', Oil, NA)) %>%
    group_by(tag, API, WellBore, FormationName, well_type, Operator, WellName, 
             CountyName, Latitude, Longitude) %>%
    summarize(annual_average_water = mean(Water, na.rm = T),
              annual_average_oil = mean(Oil, na.rm = T),
              annual_average_gas = mean(Gas, na.rm = T),
              annual_average_condensate = mean(Condensate, na.rm = T),
              total_days_production = sum(DaysProd, na.rm = T))

QA <- data.frame(`Date report was run` = as.character(date_report_ran),
                 `Report window` = paste(start_report_window, 'to',
                                         end_report_window, sep = ' '),
                 `Any facility over 12 months` = any(t1$months_production>12)) %>%
    gather()


sheets <- list(`Date Ran and QA` = QA,
               `Annual Report` = report_2) 

write_xlsx(sheets, path = paste('UDOGM Annual Average Report from', 
                                start_report_window, 'to', end_report_window, 
                                '.xlsx',
                                sep = ' '))

})























