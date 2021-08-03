library(rvest)
library(tidyverse)
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


###### Tool box


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


### add the loop here

url <- "https://jeux.loro.ch/games/swissloto/results"
h <- read_html(url)

loopy<- h%>% html_nodes(xpath ='/html/body/main/section[2]/section/div[4]/div/div/div[1]/h3')%>%
  xml_text()%>%tolower()
loopy

today<-dayM(wday(Sys.time()),"fr")


str_detect(loopy, today, negate = FALSE)

#was there to prevent some problem
# I don't think it's necessary anymore, but as everything works well, I rather not remove stuff
jokerGainsNum<-0
ptGainsNum<-0


#### Loop 
# Setting the variables for the loop
n<-0
testing<-FALSE

# This was a tricky loop, as the website is sometimes updated in 3 different times
# The loop checks for the day, the number of row in the winning result and in the joker result
while (testing == FALSE) {
  url <- "https://jeux.loro.ch/games/swissloto/results"
  h <- read_html(url)
  #number of lines winner
  nligne<-h %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  nligne1<-nligne[[1]][2] 
  testrow<-as.numeric(nrow(nligne1))
  testrow
  #number of lines Joker
  nligneJ<-h %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  nligne2<-nligneJ[[1]][2] 
  testrow2<-as.numeric(nrow(nligne2))
  testrow2
  #finding the day
  datetest<- h%>%html_nodes(xpath ='//*[@id="result-date"]')%>%xml_text()
  daytest<-as.character(tolower(datetest[1]))
  daytest<-str_detect(daytest, today, negate = FALSE)
  # if the 3 elements are new, we got our testing = True
  if(daytest==TRUE && testrow>7 && testrow2<8 ) {
    testing<-TRUE
  }
  # The exit option
  n<-n+1
  if(n > 180) {testing <- TRUE} #stop running after 1h30min, in case something wrong
  # Still takes a little nap
  if(testing==FALSE) {
    naptime(30)
  }
}

daytest
testrow
testrow2
testing
#####


#retrieving the result page and the main page with the sum of the next prize
url <- "https://jeux.loro.ch/games/swissloto/results"
url2<-"https://jeux.loro.ch/games/swissloto"
h <- read_html(url)
h2<-read_html(url2)


#extract the date
date<- h%>%html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[1]')%>%xml_text()
date<-ifelse(str_detect(date,"Mercredi"), "mercredi","samedi")

#extract the 6 numbers
result <- h %>% html_nodes(xpath ="/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[1]/ul")%>%html_nodes('li')%>%
  xml_text()%>%as.numeric()

#extract the lucky number, replay, and jocker
luckNum<- h%>% html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[1]/span[2]')%>%
  xml_text()%>%as.numeric()

replay<- h%>% html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[1]/div[2]/span')%>%
  xml_text()%>%as.numeric()

#the tricky joker, with some editing
joker<- h%>% html_nodes(xpath ='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/div[1]/div/span')%>%
  xml_text()%>%str_replace_all(fixed(" "), "")%>%str_replace_all("[\r\n]" , "")%>%str_trim("both")


print(joker)

#winner with 6 numbers, and winnings
gdGagnant<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[1]/td[2]')%>%
  xml_text()%>%as.numeric()

gains<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[1]/td[3]')%>%
  xml_text()%>%parse_number(locale=locale(grouping_mark="'", decimal_mark="."))
gains<-gains/10^6
gains<-format(gains, digits=4, decimal.mark=",", scientific=FALSE)


#winners with 5 numbers, and winnings
ptGagnant<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[2]/td[2]')%>%
  xml_text()%>%as.numeric()

ptGains<-h%>%html_nodes(xpath='/html/body/main/section/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/table[1]/tbody/tr[2]/td[3]')%>%
  xml_text()%>%parse_number(locale=locale(grouping_mark="'", decimal_mark="."))
# We keep a numeric version for the selection system, and a string one for the text
ptGainsNum<-ptGains/10^6
ptGains<-format(ptGainsNum, digits=2, decimal.mark=",", scientific=FALSE)


# joker winner with winnings
jokerGagnant<- h%>%html_nodes(xpath='/html/body/main/section[2]/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/div[2]/table/tbody/tr[1]/td[2]')%>%
  xml_text()%>%as.numeric()


jokerGains<-h%>%html_nodes(xpath='/html/body/main/section[2]/div/div[1]/section/div/div[2]/article[1]/div[2]/div/div[2]/div[2]/table/tbody/tr[1]/td[3]')%>%
  xml_text()%>%parse_number(locale=locale(grouping_mark="'", decimal_mark="."))
# We keep a numeric version for the selection system, and a string one for the text
jokerGainsNum<-jokerGains/10^6
jokerGains<-format(jokerGainsNum, digits=2, decimal.mark=",", scientific=FALSE)

# Had a bunch of problem with the joker. It's updated afer the other element
# Now it works fine, but this is just for the log
if (is.null(jokerGainsNum)==TRUE){print("Joker is null")}


#numbers in words for the text, I didn't updated this one with smallNum, as it's only in french
chiffre<-c('un','deux','trois','quatre','cinq','six','sept','huit','neuf')

#possible title
surtitre<-"Sim Loterie"
titWinner<- sprintf("Un joueur décroche le jackpot au Swiss Loto")
titdblWinner<- sprintf("Le Swiss Loto fait %s nouveaux millionnaires", chiffre[gdGagnant])
titdblWinnerSMBig<-sprintf("Le Swiss Loto fait deux nouveaux millionnaires")
titptWinner<-sprintf("Un nouveau millionnaire au tirage du Swiss Loto")
titnoWinner<-sprintf("Pas de nouveau millionnaire au tirage du Swiss Loto")
titdblptWinner<-sprintf("Deux nouveaux millionnaires au tirage du Swiss Loto")

#short title
titWinnerCo<- sprintf("Un joueur décroche le jackpot au Swiss Loto")
titdblWinnerCo<- sprintf("Le Swiss Loto fait %s millionnaires", chiffre[gdGagnant])
titdblWinnerSMBigCo<- sprintf("Le Swiss Loto fait deux millionnaires")
titptWinnerCo<-sprintf("Un nouveau millionnaire au Swiss Loto")
titnoWinnerCo<-sprintf("Pas de millionnaire au tirage du Swiss Loto")
titdblptWinnerCo<-sprintf("Deux chanceux au tirage du Swiss Loto")



#
#Possible lead
winner<-sprintf("Le Swiss Loto a fait un heureux gagnant %s qui a empoché %s millions de francs.", date,gains)
dblWinner<-sprintf("Le Swiss Loto a fait %s heureux gagnants %s qui ont chacun empoché %s millions de francs.",chiffre[gdGagnant], date,gains)
dblWinnerSMBig<-sprintf("Le Swiss Loto a fait %s heureux gagnants %s. L'un a empoché %s millions de francs et l'autre %s million.",chiffre[gdGagnant+ptGagnant], date,gains,ptGains)
ptWinner<-sprintf("Le Swiss Loto a fait un nouveau millionnaire %s. Un joueur a coché les six bons numéros et empoche %s million de francs.", date,ptGains)
noWinner<-sprintf("Personne n'a trouvé la combinaison gagnante du Swiss Loto %s soir.", date)
bonNum<-sprintf("Pour empocher le gros lot, il fallait cocher le %s, %s, %s, %s, %s et %s. Le numéro chance était le %s, le rePLAY le %s et le Joker le %s.", result[1],result[2],result[3],result[4],result[5], result[6], luckNum, replay, joker)

# 6 num et joker
dblptitWinner<- sprintf("Le Swiss Loto a fait deux millionnaires %s. L'un empoche %s avec les six bons numéros et l'autre %s million grâce au Joker.", date, ptGains, jokerGains)
jokerWinner<- sprintf("La Suisse compte un nouveau millionnaire. Un joueur a empoché %s million grâce au Joker au tirage du Swiss Loto  %s.", jokerGains, date)


#get the next cashprize
nextPrice <- h2 %>% html_nodes(xpath="/html/body/main/section/section/div[2]/div/div[1]/h2/span[1]")
nextPrice<-as.numeric(xml_text(nextPrice))
nextPrice<-format(nextPrice, decimal.mark=",", scientific=FALSE)
nextPrice

#get the next loto day
nextDate<-ifelse(date=='samedi', 'mercredi prochain','samedi')
cash<-sprintf("Lors du prochain tirage %s, %s millions de francs seront en jeu, indique la Loterie Romande.", nextDate,nextPrice)



# Selecting the strings, careful to use only numeric values
# I tried to get all the scenario, but it's possible to add more
if (gdGagnant>1){
  win<-dblwinner
  tit<-titdblWinner
  titco<-titdblWinnerCo
  
} else if (gdGagnant==1 && ptGainsNum>=1){
  win<-dblWinnerSMBig
  tit<-titdblWinnerSMBig
  titco<-titdblWinnerSMBigCo
  
} else if (gdGagnant==1){
  win<-winner
  tit<-titWinner
  titco<-titWinnerCo
  
} else if (jokerGainsNum>=1 && ptGainsNum>=1){
  win<-dblptitWinner
  tit<-titdblptWinner
  titco<-titdblptWinnerCo

} else if (jokerGainsNum>=1){
  win<-jokerWinner
  tit<-titptWinner
  titco<-titptWinnerCo

} else if (ptGainsNum>=1){
  win<-ptWinner
  tit<-titptWinner
  titco<-titptWinnerCo

} else if (gdGagnant==0 && ptGainsNum<1){
  win<-noWinner
  tit<-titnoWinner
  titco<-titnoWinnerCo
  
} 


#printing for test
cat(tit)
cat(win, bonNum,cash)

#Create the lead
leadSwiss<-paste(win,bonNum)
leadSwiss

par2<-sprintf("")
par3<-sprintf("NOTE: Cette dépêche a été produite avec le soutien de la Loterie Romande. Elle a été générée automatiquement et relue avant diffusion.")




dateFormat<-format(Sys.time(), "%d%b")
dateFormat

#Create the file name and then the file
txtSwiss<-createFileName("lotoSwiss.mrs")

createFile("lotoN.mrs",paste0("Output/",txtSwiss),leadSwiss,cash,par2, par3, titco, tit)

#Make Commit
token <- read.csv("C:/Automatisierungen/Github_Token/token.txt",header=FALSE)[1,1]

git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
try(git2r::cred_token(token))
gitadd()
gitcommit()
gitpush()

#Send everything to dropbox
#library(httpuv)
#library(rdrop2)

#token <- readRDS("token.rds")

#drop_upload(txtSwiss, path='Loto', dtoken = token)
