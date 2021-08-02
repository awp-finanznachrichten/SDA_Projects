library(cronR)
library('miniUI')
library('shiny')
library('shinyFiles')


# creating the schedule to run the scripts
# need to be updated in march and october, with time change
# it's link to the time system, so it may differ in accordance to different server
# You may need to adapt the path for the files


# For coronavirus (du lundi au vendredi "1-5")
cmd <- cron_rscript("/home/rstudio/ofsp-automation.R")
cron_add(command = cmd, frequency = 'daily', at = '10:30', id = 'ofsp', days_of_week = "1-5")


#lotro euromillions (tuesday and friday)
cmdEuro<-cron_rscript("/home/rstudio/R/lotro.R")
cron_add(command = cmdEuro, frequency = 'daily', at = '19:40', id = 'euro', days_of_week = "2,5")

#lotro swissloto, two dif cronjob (wednesday and saturday)
cmdSwiss<-cron_rscript("/home/rstudio/R/lotroSwiss.R")
cron_add(command = cmdSwiss, frequency = 'daily', at = '16:05', id = 'swiss', days_of_week = "6")
cron_add(command = cmdSwiss, frequency = 'daily', at = '18:05', id = 'swissMercredi', days_of_week = "3")

#show all the current cronjob
cron_ls()

#remove selected cronjob
cron_rm(id = "euro")
#remove  all of them
cron_clear(ask = TRUE, user = "") #to clear it


Sys.time()
