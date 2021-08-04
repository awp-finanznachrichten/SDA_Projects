library(rvest)
library(tidyverse)
library(readr)
library(stringr)
library(RCurl)
library(readxl)
library("rio")
library(gsubfn)
library(lubridate)
library(naptime)
library(XML)
library(xml2)
library(git2r)

setwd("C:/Automatisierungen/SDA_Projects")

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


###### tool box


#putting the ' where it belongs
bigNum<- function(bNum) {
  bNum<-ifelse(bNum >= 10000, format(bNum, big.mark="'", scientific=FALSE),bNum)
}

#Writing small numbers, second argument lan= 'it', 'de', or 'fr'
smallNum<- function(sNum,lan){
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


#days of the week in 3 different languages second argument lan= 'it', 'de', or 'fr'
dayM<- function(date, lan) {
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



#triple options selector, enter the number and 3 possible texts (one,many,none)
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



#Create file name adding the date
createFileName<- function(fname) {
  library(filenamer)
  f <- as.filename(fname)
  txt <- tag(f, dateFormat)
}



#create the final file function, needs to be updated in case of longer text and in case of saving
createFile2<- function(fsource, fname,lead,par1,titleShort,titleLong) {
  tx  <- readLines(fsource)
  tx2  <- gsub(pattern = "Leadtxt", replace = lead, x = tx)
  tx2  <- gsub(pattern = "Paragraphe1", replace = par1, x = tx2)
  tx2  <- gsub(pattern = "titreCourt", replace = titleShort, x = tx2)
  tx2  <- gsub(pattern = "titreLong", replace = titleLong, x = tx2)
  writeLines(tx2, con=fname)
}

# the updated version (see the ofsp script for more detail)
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

#######


### setting for the loop 

url <- "https://jeux.loro.ch/games/euromillions/results"
h <- read_html(url)

# a variable with the date on it
loopy<- h%>% html_nodes(xpath ='/html/body/main/section[2]/section/div[4]/div/div/div[1]/h3')%>%
  xml_text()%>%tolower()
loopy

# a bit of an older system, it does use the day as a word
today<-dayM(wday(Sys.time()-100000),"fr")

str_detect(loopy, today, negate = FALSE)

#### The loop
# Setting the variables
n<-0
testing<-FALSE

# the condition is testing, just check if we are today on the website
while (testing == FALSE) {
  url <- "https://jeux.loro.ch/games/euromillions/results#"
  h <- read_html(url)
  loopy<- h%>% html_nodes(xpath ='/html/body/main/section[2]/section/div[4]/div/div/div[1]/h3')%>%
    xml_text()%>%tolower()
  print(loopy)
  testing <- str_detect(loopy, today, negate = FALSE)
  print(testing)
  ###test features
  #print(totCases)
  # if(today==7 | today==1) {
  #   day <- today
  # }
  # Exit loop in case of trouble
  n<-n+1
  if(n > 180) {testing <- TRUE} #stop running after 1h30, in case something wrong
  # it does take a long 30 seconds nap, time is not of the essence here
  naptime(30)
}


#####

#retrieving the result page and the main page with the sum of the next prize
url <- "https://jeux.loro.ch/games/euromillions/results"
url2<-"https://jeux.loro.ch/games/euromillions"
h <- read_html(url)
h2<-read_html(url2)


# Extract the date
# We use the xpath to retrive data, possible to go with a table option, but this works fine
date<- h%>%html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[1]')%>%xml_text()
date<-ifelse(str_detect(date,"Mardi"), "mardi","vendredi")

# Extract the 5 numbers
result <- h %>% html_nodes(xpath ="/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[1]/ul")%>%html_nodes('li')%>%
  xml_text()%>%as.numeric()

# Extract the 2 stars
star1<- h%>% html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[1]/span[2]')%>%
  xml_text()%>%as.numeric()

star2<- h%>% html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[1]/span[3]')%>%
  xml_text()%>%as.numeric()

# Extract number of winners with 5 and 2, and gains
gdGagnant<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[1]/td[2]')%>%
  xml_text()%>%as.numeric()

# Extract their winnings
gains<-h%>%html_nodes(xpath='/html/body/main/section[2]/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[1]/td[3]')%>%
  xml_text() %>%parse_number(locale=locale(grouping_mark="'", decimal_mark="."))     #gsub("'","") #%>%as.numeric()
gains<-as.numeric(gains)/10^6
gains<-format(gains, digits=4, decimal.mark=",", scientific=FALSE)



#winners with 5 and 1 stars, and gains
ptGagnant<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[2]/td[2]')%>%
  xml_text()%>%as.numeric()

ptGains<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[2]/td[3]')%>%
  xml_text()%>%parse_number(locale=locale(grouping_mark="'", decimal_mark="."))
ptGains<-ptGains/10^6
ptGains<-format(ptGains, digits=2, decimal.mark=",", scientific=FALSE)
ptGains

# This is a plural options
s<-ifelse(ptGains>=2, "s","")




#Possible lead, write all of them, and then it will choose the right one
winner<-sprintf("Un joueur a trouvé la combinaison gagnante du tirage de l'Euro Millions de %s et a empoché %s millions de francs.", date,gains)

dblWinner<-sprintf("L'Euro Millions a fait %s heureux gagnants %s qui ont chacun empoché %s millions de francs.", smallNum(gdGagnant,"fr"), date,gains)

ptWinner<-sprintf("Personne n'a trouvé la bonne combinaison à l'Euro Millions %s. Un joueur a tout de même trouvé cinq bons numéros et une étoile et a empoché %s million%s de francs.", date,ptGains,s)

dblptWinner<-sprintf("Personne n'a trouvé la bonne combinaison à l'Euro Millions %s. %s joueurs ont tout de même trouvé cinq bons numéros et une étoile et empoché %s million%s de francs.", date, str_to_sentence(smallNum(ptGagnant,'fr')), ptGains,s)

noWinner<-sprintf("Personne n'a trouvé la combinaison gagnante à l'Euro Millions %s soir.", date)

bonNum<-sprintf("Pour gagner, il fallait jouer les numéros %s, %s, %s, %s et %s, ainsi que les étoiles %s et %s.", result[1],result[2],result[3],result[4],result[5], star1,star2)


#possible title
titWinner<- sprintf("Un joueur décroche le jackpot de l'Euro Millions")
titdblWinner<- sprintf("L'Euro Millions fait %s nouveaux millionnaires", smallNum(gdGagnant,"fr"))
titptWinner<-sprintf("Un nouveau millionnaire au tirage de l'Euro million")
titnoWinner<-sprintf("Aucun joueur ne devine la bonne combinaison de l'Euro Millions")
titdblptWinner<- sprintf("L'Euro Millions fait %s nouveaux millionnaires", smallNum(ptGagnant,"fr"))

#possible short title
titWinnerCo<- sprintf("Un chanceux remporte %s millions de francs", gains)
titdblWinnerCo<- sprintf("L'Euro Millions fait %s millionnaires", smallNum(gdGagnant,"fr"))
titptWinnerCo<-sprintf("Un nouveau millionnaire à l'Euro million")
titnoWinnerCo<-sprintf("Pas de gagnant à l'Euro Millions")
titdblptWinnerCo<- sprintf("L'Euro Millions fait %s millionnaires", smallNum(ptGagnant,"fr"))


#get the next cashprize
nextPrice <- h2 %>% html_nodes(xpath="/html/body/main/section/section/div[2]/div/div[1]/h2/span[1]")
nextPrice<-as.numeric(xml_text(nextPrice))
nextPrice<-format(nextPrice, decimal.mark=",", scientific=FALSE)
nextPrice

#get the next day
nextDate<-ifelse(date=='vendredi', 'mardi','vendredi')

# the only paragraph after the lead, just inform on the next cashprize
cash<-sprintf("Lors du prochain tirage %s, %s millions de francs seront en jeu, indique la Loterie Romande.", nextDate,nextPrice)

#selecting the strings for the lead
if (gdGagnant==1){
  win<-winner
  tit<-titWinner
  titco<-titWinnerCo
} else if (gdGagnant>1){
  win<-dblWinner
  tit<-titdblWinner
  titco<-titdblWinnerCo
  
} else if (gdGagnant==0 & ptGains<1){
  win<-noWinner
  tit<-titnoWinner
  titco<-titnoWinnerCo
  
} else if (ptGagnant>=2 & ptGains>=1){
  win<-dblptWinner
  tit<-titdblptWinner
  titco<-titdblptWinnerCo
  
} else if (ptGains>=1){
  win<-ptWinner
  tit<-titptWinner
  titco<-titptWinnerCo
}


#printing for test
cat(win, bonNum,cash)
titco
tit
cat(bonNum)

# Create the lead
leadEuro<-paste(win,bonNum)
leadEuro

# Add the note 
par2<-sprintf("")
par3<-sprintf("NOTE: Cette dépêche a été produite avec le soutien de la Loterie Romande. Elle a été générée automatiquement et relue avant diffusion.")


dateFormat<-format(Sys.time(), "%d%b")
dateFormat

# Create the file name and the final file
txtEuro<-createFileName("loto.mrs")
createFile("lotoN.mrs",paste0("Output/Lotto/",txtEuro),leadEuro,cash,par2,par3,titco,tit)


#Make Commit
token <- read.csv("C:/Automatisierungen/Github_Token/token.txt",header=FALSE)[1,1]

#git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
invisible(git2r::cred_token(token))
gitadd()
gitcommit()
gitpush()


# Sending the file to dropbox
#library(httpuv)
#library(rdrop2)

#token <- readRDS("token.rds")
#drop_upload(paste0("Output/",txtEuro), path='Loto', dtoken = token)
