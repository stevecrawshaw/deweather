#de - weathering bristol pollution data
#load libraries and install if needed
wants <- c("tidyverse", "data.table", "openair", "here", "lubridate", "deweather")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)
#------------------------SET VARIABLES---------------------------
Sys.setenv(TZ = "Etc/GMT-0")
dateFrom_dw <- "2020-01-01" #start of deweather analysis period
dateTo = "2020-05-31" #final day of lockdown period (or latest date for monitoring data)
enddate <- as.Date(dateTo)
startDate_dw <- as.Date(dateFrom_dw)
valid_hours <- as.integer((enddate - startDate_dw) * 24)

#--------------------import AQ data for selected period-----------------
# source(here::here("importODS.r"))
# aq_data_DT <- importODSAQ(
#   siteid = "all",
#   pollutant = c("pm10", "no2", "nox", "no", "o3", "pm25"),
#   dateFrom = dateFrom_dw,
#   dateTo = dateTo
# ) %>% setDT()
# 
# #----------------------------AQ MONITORS META DATA----------------------------
# #get site classifications for current sites
# 
# keepcols <- c("siteid", "locationclass", "location")
# aq_monitors <- fread("https://opendata.bristol.gov.uk/explore/dataset/air-quality-monitoring-sites/download/?format=csv&disjunctive.pollutants=true&refine.current=True&refine.pollutants=NOX&refine.pollutants=PM10&refine.pollutants=PM2.5&refine.pollutants=O3&timezone=Europe/London&lang=en&use_labels_for_header=false&csv_separator=%3B", select = keepcols) %>% setkey(siteid)
# 
# #-------------------IMPORT MET DATA-----------------------------
# colnames <- c("date_time", "ws", "wd", "temp")
# met_data_DT <- fread(paste0("https://opendata.bristol.gov.uk/explore/dataset/met-data-bristol-lulsgate/download/?format=csv&q=date_time:%5B", dateFrom_dw, "T00:00:00Z+TO+", dateTo, "T22:59:59Z%5D&timezone=UTC&lang=en&use_labels_for_header=false&csv_separator=%3B"), select = colnames)
# 
# #time average to hourly data so it can be joined to AQ data
# met_data_DT_hr <- met_data_DT %>% 
#   mutate(date = as.POSIXct(date_time, format = "%Y-%m-%dT%H:%M"),
#          year = as.factor(str_sub(date, 1, 4))) %>% 
#   timeAverage(avg.time = "hour") %>% setDT()

#load the necessary air quality and met data frames
#these data are those retrieved by the commented code above

load(here("data", "deweather_data.RData"))


#-----------------------DE WEATHER WRANGLING------------------------------
#hour average met data, join to AQ data , join to aq_monitors
#add variables for modelling
#strip NA values for no2 and pm10
#filter the sites for data capture rates < 0.9

#---------QUESTION - are the prepData parameters optimal? should I adjust lag?

aq_met_dw_hourly <- aq_data_DT[met_data_DT_hr, on = "date"][aq_monitors, on = "siteid"] %>% 
  prepData(add = c("hour", "hour.local", "weekday", "trend",
                   "week", "jday", "month"), local.tz = "Europe/London", lag = NULL) %>% 
  .[, site := as.factor(location)] %>% 
  .[date >= startDate_dw] %>% #only 2020 data
  filter(!is.na(no2) | !is.na(pm10) | !is.na(nox)) %>% #select pollutants
  add_count(site, name = "count") %>% 
  filter(count / valid_hours > 0.9) %>% #remove sites with low data capture
  mutate(site = droplevels(site), 
         locationclass = as.factor(locationclass),
         dw_group = case_when(
           siteid == 501 ~ "City Centre",
           siteid != 501 & locationclass == "Urban Traffic" ~ "Urban Traffic",
           locationclass == "Urban Background" ~ "Urban Background"
         )) #classify sites into 3 classes for intelligible plotting

#aggregate by date and dw_group to give mean hourly values for all pollutants
dw_hourly_agg <- aq_met_dw_hourly %>% 
  group_by(date, dw_group, weekday, month) %>% 
  summarise_if(is.numeric, mean, na.rm = T)

#set variables for the models
vars = c("trend", "ws", "wd", "hour", "weekday", "temp", "week")
metVars = c("ws", "wd", "hour", "weekday", "temp", "week")

#pivot to enable grouping by pollutant
#keep only pm10 and no2 as model fails with low numbers from pm2.5 and ozone
#group by pollutant and site type, nesting other columns into a list column, data
#apply the testMod, buildMod, and metSim models to each data frame by group
#also create a ggplot object for each de - weathered output and a filename so that they can be iteratively plotted

#-----run the deweather model for grouped data---------------------------------
  models_plots <- dw_hourly_agg %>%
    pivot_longer(cols = pm10:pm2.5, names_to = "pollutant", values_to = "concentration") %>%
    filter(pollutant == "pm10" | pollutant == "no2" | pollutant == "nox") %>%
    group_by(dw_group, pollutant) %>%
    nest() %>%
    mutate(testMod = map(data, ~ testMod(., vars = vars, pollutant = "concentration")),
           buildMod = map(data, ~ buildMod(., vars = vars, pollutant = "concentration", n.core = 6, B = 100)),
           demet = map(buildMod, ~ metSim(., metVars = metVars)),
           plot = map(demet, ~ggplot(timeAverage(., "day"), aes(date, pred)) + geom_line(col = "dodgerblue", size = 1) +
                        ylab(quickText("ug/m3")) +
                        labs(title = quickText(paste0("De - weathered concentrations of ", pollutant, ": ", dw_group)))),
           filename = paste0(dw_group, "_", pollutant, "_", ".png"))
#------------------------run models by site--------------------------
  
  models_plots_sites <- aq_met_dw_hourly %>%
    pivot_longer(cols = pm10:pm2.5, names_to = "pollutant", values_to = "concentration") %>%
    filter(pollutant == "no2" | pollutant == "nox") %>%
    group_by(site, pollutant) %>%
    nest() %>% #View()
    mutate(testMod = map(data, ~ testMod(., vars = vars, pollutant = "concentration")),
           buildMod = map(data, ~ buildMod(., vars = vars, pollutant = "concentration", n.core = 6, B = 100)),
           demet = map(buildMod, ~ metSim(., metVars = metVars)),
           plot = map(demet, ~ggplot(timeAverage(., "day"), aes(date, pred)) + geom_line(col = "dodgerblue", size = 1) +
                        ylab(quickText("ug/m3")) +
                        labs(title = quickText(paste0("De - weathered concentrations of ", pollutant, ": ", site)))),
           filename = paste0(site, "_", pollutant, "_", ".png"))

save(aq_data_DT, aq_monitors, met_data_DT_hr, file = here("data", "deweather_data.RData"))

