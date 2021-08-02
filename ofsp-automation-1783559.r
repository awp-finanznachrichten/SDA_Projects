library(rvest)
library(tidyverse)
library(stringr)
library(RCurl)
library(readxl)
library("rio")
library(gsubfn)
library(lubridate)
library(naptime)
library(scales)
library(xml2)
library(git2r)


gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}


######tool box

# if you need to specify a path for saving the folder of the file
# not in use here because of dropbox 
pathSu<-''
pathIn<-''
pathIt<-''



#putting the ' where it belongs in numbers higher than 10'000, variable is the number
bigNum<- function(bNum) {
  bNum<-ifelse(bNum >= 10000, format(bNum, big.mark="'", scientific=FALSE),bNum)
}

#Writing small numbers, second argument language= 'it', 'de', or 'fr' (if missing, it goes with french)
smallNum<- function(sNum,lan){
  if (missing(lan)){lan='fr'}
  chiffre<-c('un','deux','trois','quatre','cinq','six','sept','huit','neuf','dix')
  chiffreD<-c('ein','zwei', 'drei', 'vier', 'fünf', 'sechs', 'sieben', 'acht', 'neun', 'zehn')
  chiffreI<-c('uno', 'due', 'tre', 'quattro', 'cinque', 'sei', 'sette', 'otto', 'nove', 'dieci')
  if(lan=='de'){
    sNum<-ifelse(sNum <= 10, chiffreD[sNum],sNum)
  } else if (lan=='it'){
    sNum<-ifelse(sNum <= 10, chiffreI[sNum],sNum)
  } else if (lan=='fr') {
    sNum<-ifelse(sNum <= 10, chiffre[sNum],sNum)
  } else {
    sNum
  }
  
}


#days of the week in 3 different languages second argument lan= 'it', 'de', or 'fr' (if missing, it goes with french)
dayM<- function(date, lan) {
  if (missing(lan)){lan='fr'}
  num<-wday(date)
  dayE<-c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
  dayD<-c('Sonntag', 'Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag')
  dayI<-c('domenica', 'lunedì', 'martedì', 'mercoledì', 'giovedì','venerdì', 'sabato')
  dayF<-c('dimanche', 'lundi','mardi','mercredi','jeudi','vendredi','samedi')
  wday(date)
  if(lan=='de'){
    day<-dayD[num]
  } else if (lan=='it'){
    day<-dayI[num]
  } else if (lan=='fr') {
    day<-dayF[num]
  } else {
    day<-dayF[num]
  }
  return(day)
}


# triple options selector,
# enter the number and 3 possible texts (one,many,none)
# return the  corresponding paragraphe
# used for hospitalization or death in the lead
selectText<- function(num,varOne,varMult,varNone){
  if (num==1){
    para<-varOne
  } else if (num>1){
    para<-varMult
  } else if (num<=0) {
    para<-varNone
  }
  return(para)
}


# Create file name adding the date
# fname=given by the user "exemple.mrs"
# pathDesk= if you want to put it in a different directory
createFileName<- function(fname,pathDesk) {
  library(filenamer)
  if (missing(pathDesk)) {pathDesk=''}
  f <- as.filename(fname)
  txt <- tag(f, dateFormat)
  txt<-tag(pathDesk,txt) # add path in the arg somethings among those line to save in the correct path
}


#Different functions to create the file

#create the final file function, needs to be updated in case of longer text and in case of saving
#the it file still use the first one, it's the shortest of the 3 languages
createFile<- function(fsource, fname,lead,par1,par2,par3,titleShort,titleLong) {
  tx  <- readLines(fsource)
  tx2  <- gsub(pattern = "Leadtxt", replace = lead, x = tx)
  tx2  <- gsub(pattern = "Paragraphe1", replace = par1, x = tx2)
  tx2  <- gsub(pattern = "Paragraphe2", replace = par2, x = tx2)
  tx2  <- gsub(pattern = "Paragraphe3", replace = par3, x = tx2)
  tx2  <- gsub(pattern = "titreCourt", replace = titleShort, x = tx2)
  tx2  <- gsub(pattern = "titreLong", replace = titleLong, x = tx2)
  writeLines(tx2, con=fname)
}

# not in use anymore
createFile4<- function(fsource, fname,lead,par1,par2,par3,par4,titleShort,titleLong) {
  tx  <- readLines(fsource)
  tx2  <- gsub(pattern = "Leadtxt", replace = lead, x = tx)
  tx2  <- gsub(pattern = "Paragraphe1", replace = par1, x = tx2)
  tx2  <- gsub(pattern = "Paragraphe2", replace = par2, x = tx2)
  tx2  <- gsub(pattern = "Paragraphe3", replace = par3, x = tx2)
  tx2  <- gsub(pattern = "Paragraphe4", replace = par4, x = tx2)
  tx2  <- gsub(pattern = "titreCourt", replace = titleShort, x = tx2)
  tx2  <- gsub(pattern = "titreLong", replace = titleLong, x = tx2)
  writeLines(tx2, con=fname)
}

# This is the function for the DE and FR file, it uses a list for the paragraphs
# create the final file function up to 9 paragraphes (lead not included) careful
# fsource= the document to open and change (.mrs)
# fname= the name of the final file (made by createFileName)
# numPar= the number of paragraphs that we need (the excess is deleted, so be precise)
# titleShort,titleLong,lead,par, the different element to change, par is a list of string
createFile9<- function(fsource, fname, numPar, titleShort,titleLong,lead,par) {
  tx  <- readLines(fsource)
  tx2  <- gsub(pattern = "titreCourt", replace = titleShort, x = tx)
  tx2  <- gsub(pattern = "titreLong", replace = titleLong, x = tx2)
  tx2  <- gsub(pattern = "Leadtxt", replace = lead, x = tx2)
  for (i in 1:9){
    pat<-sprintf("Paragraphe%s",i)
    if (numPar>=i){
      tx2  <- gsub(pattern = pat, replace = as.character(par[i]), x = tx2)
    } else if (numPar<i){
      tx2  <- gsub(pattern = pat, replace ='', x = tx2)
    }
  }
  writeLines(tx2, con=fname)
}

#######



#Getting on the webpage
url <- "https://www.covid19.admin.ch/fr/overview?ovTime=total"
h <- read_html(url)

#finding what today is
today<-wday(Sys.time())

#not very relevant anymore, but if you need to know what day was yesterday, here you have it
yesterday<-today-1


# The table option
# I ended up not using tbls in this version, but you can use it to shorten the script
tbls <- html_nodes(h, "table")

# Extract the date 
date<-h %>% 
  html_nodes(xpath = "/html/body/app-root/bag-dashboard/main/bag-overview/div/div[1]/div[3]/bag-card-overview-case/bag-overview-card/div/div/p")
date<-as.character(date)
date

# Transform it in a date format and extract the day
dateFormat<- strapply(date, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")[[1]][1]
date<-as.Date(dateFormat, format="%d.%m.%Y", optional = FALSE, tz="GMT")
day<-wday(date)
date
day

# Finding total number of cases as yesterday
# Look for the node table, they are currently 10 tables on the website
# Access them by index (here .[1])
newNumber<-h %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

# From the table, isolate the column that we are interested in
cas<-newNumber[[1]][2]
cas<-cas[[1]]
cas

# Extract the number and make it into a numeric format
totCasesYesterday<-as.numeric(gsub("\\D+", "",cas[2]))
totCasesYesterday


#####Loop
# setting the variable before the loop to make sure it runs properly
day<-10
totCases<-totCasesYesterday
n<-0

# Two conditions are look upon, as the website was not always updated properly
# It checks if we are today, and if the number of cases has changed
while (today != day && totCasesYesterday == totCases) {
  url <- "https://www.covid19.admin.ch/fr/overview?ovTime=total"
  h <- read_html(url)
  #the number of cases option
  tbls <- html_nodes(h, "card")
  #finding the day
  date<-h %>% 
    html_nodes(xpath = "/html/body/app-root/bag-dashboard/main/bag-overview/div/div[1]/div[3]/bag-card-overview-case/bag-overview-card/div/div/p")
  date<-as.character(date)
  date
  dateFormat<- strapply(date, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")[[1]][1]
  date<-as.Date(dateFormat, format="%d.%m.%Y", optional = FALSE, tz="GMT")
  day<-wday(date)
  #finding the number of cases
  newNumber<-h %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  cas<-newNumber[[1]][2]
  cas<-cas[[1]]
  cas
  totCases<-as.numeric(gsub("\\D+", "",cas[2]))
  # just in case it start to run on a weekend, will stop it
  if(today==7 | today==1) {
    day <- today
  }
  # An exit condition, to avoid an infinite loop
  # Stop running after 3h45min, in case something wrong
  n<-n+1
  if(n > 4500) {day <- today} 
  # it does a little nap for 3 second before next loop
  # Used to limit the computing power, if it's not a problem, this is not needed
  naptime(3)
}

# a few variables to look upon for the log in case of trouble
n
day
today
totCases
totCasesYesterday

#End of loop


#### Extracting the data
#Different number of hours if monday or the rest of the week
# This has to be change either here or manually for holidays, when the website doesn't update
hours<-ifelse(today==2,72,24)
# I keep this alternative version for holiday time
#hours<-ifelse(today==3,96,24)
hours
print(today)
date<-today


#getting the different tables from the website
url <- "https://www.covid19.admin.ch/fr/overview"
h <- read_html(url)

# this line is actually a bit useless now
tbls <- html_nodes(h, "table")

# The extraction system remains the same for all table
# Extracting the data from the first (.[1]) table
# If the tables are move (when they add a new one), changing this number will fix the problem
newNumber<-h %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

#getting number from the second column
cas<-newNumber[[1]][2]
cas<-cas[[1]]
cas

# Cleaning the number, some can be use as they are
newCases<-as.numeric(gsub("\\D+", "",cas[1]))
fourteenDays<-as.numeric(gsub("\\D+", "",cas[2]))
perCapita<-cas[3]



#info from the hospitalization table
hosp<-h %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

#getting number from the second column
hosp<-hosp[[1]][2]
hosp<-hosp[[1]]
hosp

newHosp<-as.numeric(gsub("\\D+", "",hosp[1]))
hospfourteenDays<-as.numeric(gsub("\\D+", "",hosp[2]))
hospperCapita<-hosp[3]


#info from the death table
dead<-h %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

#getting number from the second column
dead<-dead[[1]][2]
dead<-dead[[1]]
dead

newDeath<-as.numeric(gsub("\\D+", "",dead[1]))
deadfourteenDays<-as.numeric(gsub("\\D+", "",dead[2]))
deadperCapita<-dead[3]
newDeath

#info test card
test<-h %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table(fill = TRUE)

#getting number from test column
test<-test[[1]][2]
test<-test[[1]]
test

newTest<-as.numeric(gsub("\\D+", "",test[1]))
testfourteenDays<-as.numeric(gsub("\\D+", "",test[2]))
testPCR14Days<-as.numeric(gsub("\\D+", "",test[3]))
testRapide14Days<-as.numeric(gsub("\\D+", "",test[4]))

#extracting positivity rate and computing the total for 14 days
PCRPos<-test[6]
testPCRPos<-str_replace(PCRPos, ",", ".")
testPCRPos<-parse_number(testPCRPos)
PCRPos
testPCRPos

# this was use for a time in the DE version, to have the positivty rate during the last 14 days
# not in use anymore
RapidePos<-test[7]
testRapidePos<-str_replace(RapidePos, ",", ".")
testRapidePos<-parse_number(testRapidePos)
RapidePos
testRapidePos

positivityfourteenDays<-((testPCRPos/100)*testPCR14Days+(testRapidePos/100)*testRapide14Days)/testfourteenDays

positivityfourteenDays<-label_percent(accuracy = 0.1, decimal.mark = ",")(positivityfourteenDays)
positivityfourteenDays<-gsub("%", "", positivityfourteenDays)

positivityfourteenDays

#Getting reproduction rate
Reprod<-h %>%
  html_nodes("table") %>%
  .[5] %>%
  html_table(fill = TRUE)

txReprod<-Reprod[[1]][2]
txReprod<-txReprod[[1]]

txReprod<-txReprod[1]
txReprod


# Getting the variant as percentage 
# An alternative method, with numbers, is further down in the script
variPerc<-h %>%
  html_nodes("table") %>%
  .[6] %>%
  html_table(fill = TRUE)

variPerc<-variPerc[[1]][2]
variPerc<-variPerc[[1]]

variPercBeta<-variPerc[1]
variPercGamma<-variPerc[2]
variPercDelta<-variPerc[3]
variPercDelta


#Getting vaccine
vax<-h %>%
  html_nodes("table") %>%
  .[7] %>%
  html_table(fill = TRUE)

vax1<-vax[[1]][2]
vax1<-vax1[[1]]
vax1[6]

vaxReceived<-as.numeric(gsub("\\D+", "",vax1[1]))
vaxDelivery<-as.numeric(gsub("\\D+", "",vax1[2]))
vaxDone<-as.numeric(gsub("\\D+", "",vax1[3]))
vaxOne<-vax1[5]
vaxDouble<-vax1[6]
vaxParti<-vax1[7]

vaxReceived
vaxDelivery
vaxOne
vaxDone
vaxDouble
vaxParti



# certificat
certifi<-h %>%
  html_nodes("table") %>%
  .[8] %>%
  html_table(fill = TRUE)

certi<-certifi[[1]][2]
certi<-certi[[1]]
certi

# certification vaccine
certifVaxNew<-as.numeric(gsub("\\D+", "",certi[1]))
certifVaxTot<-as.numeric(gsub("\\D+", "",certi[2]))

# certification cured
certifCuredNew<-as.numeric(gsub("\\D+", "",certi[5]))
certifCuredTot<-as.numeric(gsub("\\D+", "",certi[6]))

# certification test
certifTestNew<-as.numeric(gsub("\\D+", "",certi[9]))
certifTestTot<-as.numeric(gsub("\\D+", "",certi[10]))


#hospitals capacity
hospCapa<-h %>%
  html_nodes("table") %>%
  .[9] %>%
  html_table(fill = TRUE)

#extracting the numbers
capacity<-hospCapa[[1]][2]
capacity<-capacity[[1]]
capacity

occupationTotal<-capacity[1]
occupationTotalCovid<-capacity[2]
iuOccupation<-capacity[4]
iuOccupationCovid<-capacity[5]
bedIUCovid15days<-capacity[6]



# version without % (Percent in the text) for the DE version
iuOccupationDE<-str_replace(iuOccupation, ",", ".")
iuOccupationDE<-parse_number(iuOccupationDE)
iuOccupationDE<-format(iuOccupationDE, digits=4, decimal.mark=",", scientific=FALSE)
iuOccupationDE

iuOccupationCovidDE<-str_replace(iuOccupationCovid, ",", ".")
iuOccupationCovidDE<-parse_number(iuOccupationCovidDE)
iuOccupationCovidDE<-format(iuOccupationCovidDE, digits=4, decimal.mark=",", scientific=FALSE)
iuOccupationCovidDE

#test to see if everything is there
occupationTotal
occupationTotalCovid
iuOccupation
iuOccupationCovid
bedIUCovid15days


#Getting quarantine
isolQuar<-h %>%
  html_nodes("table") %>%
  .[10] %>%
  html_table(fill = TRUE)

#Isolation tab, extracting the numbers
isol<-isolQuar[[1]][2]
isol<-isol[[1]]
isol

isolation<-as.numeric(gsub("\\D+", "",isol[1]))
quarantaine<-as.numeric(gsub("\\D+", "",isol[2]))
quarantaineArrival<-as.numeric(gsub("\\D+", "",isol[3]))





# Block for the total, we need to switch to a different url
# The system remains the same
# extracting info from the page with the number since the beginning of the pandemic
url <- "https://www.covid19.admin.ch/fr/overview?ovTime=total"
h <- read_html(url)

# still there, still no use in this version 
tbls2 <- html_nodes(h, "table")

# getting the table for cases
newNumber<-h %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

#getting number 
cas<-newNumber[[1]][2]
cas<-cas[[1]]
cas

totCases<-as.numeric(gsub("\\D+", "",cas[2]))


#info from the second table
hosp<-h %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

#getting numbers
hosp<-hosp[[1]][2]
hosp<-hosp[[1]]
hosp

totHosp<-as.numeric(gsub("\\D+", "",hosp[2]))
totHosp


#info third table
dead<-h %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

#getting number from first column
dead<-dead[[1]][2]
dead<-dead[[1]]
dead

totDeath<-as.numeric(gsub("\\D+", "",dead[2]))
totDeath

#info 4th table
test<-h %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table(fill = TRUE)

#getting number from first column
test<-test[[1]][2]
test<-test[[1]]
test

totTest<-as.numeric(gsub("\\D+", "",test[2]))
totTest

#block variants
# the variants are in a different page, not sure if it's necessary to report them
# on the main page variants are also reported, we kind of let it go for now, but it can always be added back
url3<-'https://www.covid19.admin.ch/fr/epidemiologic/virus-variants'
h3<-read_html(url3)
tabs<- html_nodes(h3, "table")

variants<-h3 %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

#getting numbers
variants<-variants[[1]][2]
variants<-variants[[1]]


variantsBrit<-as.numeric(gsub("\\D+", "",variants[5]))
variantsAfr<-as.numeric(gsub("\\D+", "",variants[1]))
variantsBres<-as.numeric(gsub("\\D+", "",variants[2]))
variantsIndien<-as.numeric(gsub("\\D+", "",variants[3]))
variantsIndien

# Not in use currently, but may be useful
# Getting the data from the zip file, it can be extended and improve

# temp <- tempfile()
# download.file("https://www.covid19.admin.ch/api/data/20201105-iq6fzdhl/downloads/sources-csv.zip",temp)
# dataCases <- read.csv(unz(temp, "data/COVID19Cases_geoRegion.csv"))
# 
# totCases<-max(dataCases$sumTotal)
# 
# dataDeath <- read.csv(unz(temp, "data/COVID19Death_geoRegion.csv"))
# 
# totDeath<-max(dataDeath$sumTotal)
# totDeath
# 
# dataHosp <- read.csv(unz(temp, "data/COVID19Hosp_geoRegion.csv"))
# 
# totHosp<-max(dataHosp$sumTotal)
# totHosp
# 
# dataTest <- read.csv(unz(temp, "data/COVID19Test_geoRegion_all.csv"))
# tst<- arrange(dataTest, desc(dataTest$sumTotal))
# tst<-head(tst,1)
# totTest<-tst$sumTotal
# 
# unlink(temp)

# Compute positivity rate and turn it in %
pos<-newCases/newTest
positivity<-label_percent(accuracy = 0.01, decimal.mark = ",")(pos)
positivity



##Control variables for test

# newCases
# fourteenDays
# perCapita
# newHosp
# hospfourteenDays
# hospperCapita
# newDeath
# deadfourteenDays
# deadperCapita
# newTest
# testfourteenDays
# positivityfourteenDays
# isolation
# quarantaine
# quarantaineArrival
# totCases
# totHosp
# totDeath
# totTest




### Writing

#Makes the title part, the surtitre is not in use, because it never change, but still an option

#titles FR
surtitre<-"Coronavirus"
titleShort<- sprintf("%s nouveaux cas de coronavirus", bigNum(newCases))
titleLong<- sprintf("La Suisse compte %s nouveaux cas de coronavirus en %s heures", bigNum(newCases),hours)

#titles D
surtitreD<-"Coronavirus - Schweiz"
titleShortD<- sprintf("%s Neuinfektionen in der Schweiz", bigNum(newCases))
titleLongD<- sprintf("BAG meldet %s neue Coronavirus-Fälle innerhalb von %s Stunden", bigNum(newCases), hours)

#titles I
surtitreI<-"Epidemia"
titleShortI<- sprintf("%s nuovi casi di coronavirus", bigNum(newCases))
titleLongI<- sprintf("In Svizzera %s nuovi casi di coronavirus", bigNum(newCases))



# Possible lead  
# the hospitalization and death parts slightly change according to the numbers
# First sentences of the lead doesn't change 
lead1<-sprintf("La Suisse compte %s %s cas supplémentaires de coronavirus en %s heures, selon les chiffres de l'Office fédéral de la santé publique (OFSP).", dayM(date,'fr'), bigNum(newCases),hours)

# Three options for the number of death
# Lead FR
leadDeath<-sprintf("Un décès de plus a été enregistré")
leadNoDeath<-sprintf("Aucun décès de plus n'a été enregistré")
LeadMultipleDeath<-sprintf("On déplore %s décès supplémentaires", smallNum(newDeath,'fr'), totDeath)

# Hospitalization part of the lead, if no, multiple or one
hospMult<-sprintf("et %s malades ont été hospitalisés.", smallNum(newHosp,'fr'))
hospOne <- sprintf("et il y a une hospitalisation de plus par rapport à la veille.")
hospNone<- sprintf("et il n'y a pas de nouvelle hospitalisation.")

#lead D
lead1D<-sprintf("In der Schweiz und in Liechtenstein sind dem Bundesamt für Gesundheit (BAG) am %s innerhalb von %s Stunden %s neue Coronavirus-Ansteckungen gemeldet worden.", dayM(date,'de'), hours, bigNum(newCases))
leadDeathD<-sprintf("Zudem wurde ein neuer Todesfall verzeichnet.")
leadNoDeathD<-sprintf("Neue Todesfälle gab es keine.")
LeadMultipleDeathD<-sprintf("Gleichzeitig registrierte das BAG %s neue Todesfälle",smallNum(newDeath,'de'))

hospMultD<-sprintf("und %s Spitaleinweisungen.", smallNum(newHosp,'de'))
hospOneD <- sprintf("Ein Erkrankter mussten hospitalisiert werden.")
hospNoneD<- sprintf("Hospitalisiert werden musste niemand.")

#Lead it
lead1I<-sprintf("In Svizzera, nelle ultime %s ore, si sono registrati %s nuovi casi di coronavirus, secondo le cifre pubblicate dall’Ufficio federale di sanità pubblica (UFSP).", hours, bigNum(newCases))
leadDeathI<-sprintf("Si segnala un nuovo decesso.")
leadNoDeathI<-sprintf("Nessun decesso è stato segnalato.")
LeadMultipleDeathI<-sprintf("%s nuovi decessi sono stati segnalati.", str_to_sentence(smallNum(newDeath,'it')))

hospMultI<-sprintf("%s persone sono state ricoverate in ospedale.", smallNum(newHosp,'it'))
hospOneI <- sprintf("In Svizzera si registra una nuova ospedalizzazione.")
hospNoneI<- sprintf("Non si registrano nuove ospedalizzazioni.")




# Creating lead by pasting it together 
# selectText does choose the right part to add in
lead<-paste(lead1,selectText(newDeath,leadDeath,LeadMultipleDeath,leadNoDeath),selectText(newHosp, hospOne,hospMult,hospNone))
leadD<-paste(lead1D,selectText(newDeath,leadDeathD,LeadMultipleDeathD,leadNoDeathD),selectText(newHosp, hospOneD,hospMultD,hospNoneD))
leadI<-paste(lead1I,selectText(newDeath,leadDeathI,LeadMultipleDeathI,leadNoDeathI),selectText(newHosp, hospOneI,hospMultI,hospNoneI))

cat(lead)

#the variant part is not in use right now, but can be simply put back, it's par4, need to rechange numbers of par and remove par6

#text F
par1<-sprintf("Durant les dernières %s heures, les résultats de %s tests ont été transmis, indique l'OFSP. Le taux de positivité s'élève à %s.",hours, bigNum(newTest), positivity) 
par2<-sprintf("Sur les quatorze derniers jours, le nombre total d'infections est de %s, soit %s nouvelles infections pour 100'000 habitants. Le taux de reproduction, qui a un délai d'une dizaine de jours, est lui de %s. Les patients Covid-19 occupent %s des places disponibles en soins intensifs, dont le taux d'occupation est de %s.", bigNum(fourteenDays), perCapita, txReprod,iuOccupationCovid, iuOccupation)
par3<-sprintf("Au total %s doses de vaccin ont été administrées et %s des personnes ont déjà reçu deux doses. La Suisse a reçu jusqu'à présent %s doses de vaccins. Des certificats ont déjà été émis pour %s personnes vaccinées.", bigNum(vaxDone), bigNum(vaxDouble),bigNum(vaxReceived), bigNum(certifVaxTot))
#par4<-sprintf("Quant aux variants du coronavirus, %s cas ont été attribués au variant Alpha (B.1.1.7), %s au variant Beta (B.1.351), %s au variant Gamma (P.1) et %s au variant Delta (B.1.617.2).", bigNum(variantsBrit),variantsAfr, variantsBres, variantsIndien)
par4<-sprintf("Depuis le début de la pandémie, %s cas de contamination au Covid-19 ont été confirmés en laboratoire sur un total de %s tests effectués en Suisse et au Liechtenstein. Le total des décès s'élève à %s et le nombre de personnes hospitalisées atteint %s.", bigNum(totCases),bigNum(totTest), bigNum(totDeath),bigNum(totHosp))
par5<-sprintf("Le pays dénombre par ailleurs %s personnes en isolement et %s individus faisant partie de leurs contacts ont été mis en quarantaine. S'y ajoutent %s autres personnes revenant de voyage d'un pays à risque et qui ont dû aussi passer par la case de la quarantaine.",bigNum(isolation),bigNum(quarantaine),bigNum(quarantaineArrival))

# Note part with a few open line available, the end user has to cut-paste the note in the right section 
par6<-sprintf("")
par7<-sprintf("")
par8<-sprintf("NOTE: Cette dépêche a été générée automatiquement sur la base des données de l'OFSP et relue avant diffusion.")
# 
# par9<-spintf("")

#create a list with all the paragraph
parF<-list(par1,par2,par3,par4,par5,par6,par7,par8)

#text D
par1D<-sprintf("Auf 100'000 Einwohnerinnen und Einwohner wurden in den vergangenen zwei Wochen %s laborbestätigte Coronavirus-Infektionen gemeldet. Die Reproduktionszahl R, die angibt, wie viele Personen eine infizierte Person im Durchschnitt ansteckt, lag vor rund zehn Tagen bei %s.", perCapita, txReprod)
par2D<-sprintf("Die Auslastung der Intensivstationen in den Spitälern beträgt zur Zeit %s Prozent. %s Prozent der verfügbaren Betten werden von Covid-19-Patienten besetzt.", iuOccupationDE, iuOccupationCovidDE)
par3D<-sprintf("Insgesamt wurden bis vorgestern Abend %s Impfdosen an die Kantone und Liechtenstein ausgeliefert. Damit wurden %s Dosen verabreicht. %s Personen sind bereits vollständig geimpft. Bislang wurden %s Zertifikate für vollständig Geimpfte ausgestellt.", bigNum(vaxDelivery),bigNum(vaxDone), bigNum(vaxDouble), bigNum(certifVaxTot))
par4D<-sprintf("In den vergangenen %s Stunden wurden dem BAG %s neue Corona-Tests gemeldet. Seit Beginn der Pandemie wurden in der Schweiz und in Liechtenstein %s Tests auf Sars-CoV-2 durchgeführt, den Erreger der Atemwegserkrankung Covid-19, wie das BAG weiter mitteilte. Insgesamt gab es %s laborbestätigte Fälle von Ansteckungen mit dem Coronavirus.", hours, bigNum(newTest), bigNum(totTest), bigNum(totCases))
#par5D<-sprintf("Von den mutierten Varianten des Coronavirus betrafen %s die Alpha-Variante (B.1.1.7), %s die Delta-Variante (B.1.617.2), %s die Beta-Variante (B.1.351) sowie %s die Gamma-Variante (P.1).", bigNum(variantsBrit),variantsIndien, variantsAfr, variantsBres)
par5D<-sprintf("%s Personen mussten bisher wegen einer Covid-19-Erkrankung im Spital behandelt werden. Die Zahl der Todesfälle im Zusammenhang mit einer Covid-19-Erkrankung belief sich auf %s.",bigNum(totHosp), bigNum(totDeath))
par6D<-sprintf("Aufgrund der Kontakt-Rückverfolgung befanden sich laut Angaben des BAG %s Menschen in Isolation und %s Menschen in Quarantäne. Zusätzlich befanden sich %s Personen in Quarantäne, die aus einem Risikoland heimgekehrt waren.", bigNum(isolation),bigNum(quarantaine),bigNum(quarantaineArrival))


par7D<-sprintf("")
par8D<-sprintf("")
par9D<-sprintf("NOTE: Diese Meldung wurde automatisch auf der Grundlage der BAG-Daten erstellt. Sie wurde vor der Publikation überprüft.")

paraD<-list(par1D,par2D,par3D,par4D,par5D,par6D,par7D,par8D,par9D)


#Text it, still on an older function, no need to make a list of paragraphs
par1I<-sprintf("Nel corso delle ultime %s ore sono stati trasmessi i risultati di %s test, indica l’UFSP. Il tasso di positività è del %s. Sull’arco di due settimane, il numero totale di infezioni è %s. I casi per 100'000 abitanti negli ultimi 14 giorni sono %s.  Il tasso di riproduzione, che ha un ritardo di una decina di giorni sugli altri dati, si attesta a %s.",hours, bigNum(newTest), positivity, bigNum(fourteenDays), perCapita, txReprod)
par2I<-sprintf("Complessivamente %s dosi di vaccino sono state consegnate ai cantoni, di cui %s sono state somministrate, e %s persone hanno già ricevuto una seconda iniezione. Dall’inizio della pandemia, %s casi di Covid-19 sono stati confermati in laboratorio su un totale di %s test effettuati in Svizzera e nel Liechtenstein. In totale si contano %s decessi e il numero di persone ospedalizzate si attesta a %s.", bigNum(vaxDelivery),bigNum(vaxDone), bigNum(vaxDouble), bigNum(totCases),bigNum(totTest),bigNum(totDeath), bigNum(totHosp))
par3I<-sprintf("In Svizzera si contano attualmente %s persone in isolamento e %s entrate in contatto con loro e messe in quarantena. A questi se ne aggiungono %s di ritorno da un Paese a rischio e posti in quarantena.",bigNum(isolation),bigNum(quarantaine),bigNum(quarantaineArrival))



#Last addition, but untested: Warning system if tables are added, need to be modified after the alert is fixed
# warning<-"Attention, l'ofsp a modifié ses données, cette dépêche peut contenir des erreurs"
# if (length(tbls)>10) {titleShort=warning}
# titleShort


# Create the file name
# the path variable is not in use, but it's an option if the file has to be save in a different folder
txt<-createFileName("ofsp.mrs",pathSu)
txtd<-createFileName("bag.mrs",pathIn)
txti<-createFileName("ufsp.mrs",pathIt)


# Make the actual .mrs file, for italian, still with the older system
createFile9('ofsp9.mrs', txt, 8, titleShort,titleLong,lead,parF)
createFile9('bag9par.mrs', txtd, 9, titleShortD,titleLongD,leadD,paraD)
createFile("ufsp.mrs",txti,leadI,par1I,par2I,par3I,titleShortI,titleLongI)


# Final step, sending everything to dropbox
library(httpuv)
library(rdrop2)

# Use the token.rds file with the data to access dropbox, you need to create your own
token <- readRDS("token.rds")

# path bring it to the right folder in dropbox
drop_upload(txt, path='ofsp', dtoken = token)
drop_upload(txtd, path='ofsp', dtoken = token)
drop_upload(txti, path='ofsp', dtoken = token)
