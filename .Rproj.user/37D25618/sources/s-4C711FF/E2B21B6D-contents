#Create_Text
impfdaten_dw$Text_d <- paste0("Im Kanton ",impfdaten_dw$Kanton_d," wurden bislang pro 100 Einwohner <b>",
                              impfdaten_dw$Verimpft_pro_Person,"</b> Impfungen durchgeführt.",
                              " Das entspricht <b>",impfdaten_dw$Verimpft,"</b> Impfungen.",
                              " <b>",impfdaten_dw$Geimpfte_Personen, "</b> Personen oder <b>",
                              impfdaten_dw$Anteil_Bevoelkerung," Prozent</b> der Bevölkerung sind bereits vollständig geimpft.<br><br>",
                              " In der vergangenen Woche wurden pro Tag durchschnittlich <b>",impfdaten_dw$Verimpft_pro_Tag,
                              "</b> Personen geimpft.",
                              " Im Vergleich zur Vorwoche entspricht dies einer Veränderung von <b>",impfdaten_dw$Veraenderung," Prozent</b>.")

#Create_Text
impfdaten_dw$Text_f <- paste0("Dans le canton de ",impfdaten_dw$Kanton_f,", <b>",
                              impfdaten_dw$Verimpft_pro_Person,"</b> injections pour 100 habitants ont été réalisées jusqu’ici.",
                              " Cela représente en tout <b>",impfdaten_dw$Verimpft,"</b> vaccinations.",
                              " <b>",impfdaten_dw$Geimpfte_Personen, "</b> personnes (soit <b>",
                              impfdaten_dw$Anteil_Bevoelkerung," pour cent</b> de la population) ont été vaccinées complètement.<br><br>",
                              " La semaine dernière, <b>",impfdaten_dw$Verimpft_pro_Tag,
                              "</b> personnes ont été vaccinées chaque jour en moyenne.",
                              " Cela représente une variation de <b>",impfdaten_dw$Veraenderung,"%</b> par rapport à la semaine précédente.")

impfdaten_dw$Text_f <- gsub("de <b>mehr als ","<b>plus de ",impfdaten_dw$Text_f)
impfdaten_dw$Text_f <- gsub("de <b>weniger als ","<b>moins de ",impfdaten_dw$Text_f)


impfdaten_dw$Text_i <- paste0("Nel canton ",impfdaten_dw$Kanton_i," fino a questo momento sono state effettuate <b>",
                              impfdaten_dw$Verimpft_pro_Person,"</b> iniezioni ogni 100 abitanti.",
                              " In cifre assolute, si tratta di <b>",impfdaten_dw$Verimpft,"</b> vaccinazioni.",
                              " <b>",impfdaten_dw$Geimpfte_Personen, "</b> persone o il <b>",
                              impfdaten_dw$Anteil_Bevoelkerung," percento</b> della popolazione sono completamente vaccinati.<br><br>",
                              " La scorsa settimana, in media <b>",impfdaten_dw$Verimpft_pro_Tag,
                              "</b> persone sono state vaccinate giornalmente ",
                              " (variazione rispetto alla settimana precedente: <b>",impfdaten_dw$Veraenderung,"%)</b>.")


impfdaten_dw$Text_i <- gsub("<b>mehr als ","<b>più del ",impfdaten_dw$Text_i)
impfdaten_dw$Text_i <- gsub("<b>weniger als ","<b>meno del ",impfdaten_dw$Text_i)

impfdaten_dw <- excuse_my_french(impfdaten_dw)

#Write File
write.csv(impfdaten_dw,"Output/impfdaten.csv", row.names=FALSE, fileEncoding = "UTF-8")

#Make Commit
token <- read.csv("C:/Automatisierungen/Github_Token/token.txt",header=FALSE)[1,1]

git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
try(git2r::cred_token(token))
gitadd()
gitcommit()
gitpush()

#Change Title of Datawrapper-Chart and publish Charts
datawrapper_auth("BMcG33cGBCp2FpqF1BSN5lHhKrw2W8Ait4AYbDEjkjVgCiWe07iqoX5pwHXdW36g")
dw_edit_chart("8nBMe",intro=paste0("In der Schweiz wurden bislang pro 100 Einwohner <b>",impfungen_anteil_ch,"</b> Impfdosen verabreicht. Das entspricht <b>",impfungen_ch,"</b> Impfungen. <b>",
                                   format(impfungen_complete,big.mark = "'"),"</b> Personen sind bereits vollständig geimpft, das heisst <b>",anteil_bevoelkerung,
                                   " Prozent</b> der Bevölkerung haben bereits zwei Impfdosen erhalten."), annotate=paste0("Stand: ",impfdaten_dw$Datum[1]))
dw_publish_chart("8nBMe")

dw_edit_chart("Ty61K",intro=paste0("En Suisse, <b>",impfungen_anteil_ch,"</b> injections pour 100 habitants ont été réalisées jusqu'ici. Cela représente en tout <b>",impfungen_ch,"</b> vaccinations.",
                                   " Jusqu'ici, <b>",format(impfungen_complete,big.mark = "'"),
                                   "</b> personnes ont été vaccinées complètement. Cela signifie que <b>",
                                   anteil_bevoelkerung,
                                   " pour cent</b> de la population a déjà obtenu deux doses de vaccin."), annotate=paste0("Etat: ",impfdaten_dw$Datum[1]))
dw_publish_chart("Ty61K")

dw_edit_chart("OmzDG",intro=paste0("In Svizzera fino a questo momento sono state effettuate <b>",impfungen_anteil_ch,"</b> iniezioni ogni 100 abitanti. In cifre assolute, si tratta di <b>",impfungen_ch,"</b> vaccinazioni.",
                                   " Fino ad ora <b>",format(impfungen_complete,big.mark = "'"),
                          "</b> persone sono state completamente vaccinate, questo significa che il <b>",
                          anteil_bevoelkerung,
                          " percento</b> della popolazione ha ricevuto due iniezioni."), annotate=paste0("Stato: ",impfdaten_dw$Datum[1]))
dw_publish_chart("OmzDG")

#Tabellen SDA
dw_edit_chart("6thMk",intro=paste0("Stand: ",impfdaten_dw$Datum[1]))
dw_publish_chart("6thMk")

dw_edit_chart("ILXgH",intro=paste0("Etat: ",impfdaten_dw$Datum[1]))
dw_publish_chart("ILXgH")

dw_edit_chart("mfQwr",intro=paste0("Stato: ",impfdaten_dw$Datum[1]))
dw_publish_chart("mfQwr")


#Karten AWP
dw_edit_chart("d7vmx",intro=paste0("In der Schweiz wurden bislang pro 100 Einwohner <b>",impfungen_anteil_ch,"</b> Impfdosen verabreicht. Das entspricht <b>",impfungen_ch,"</b> Impfungen. <b>",
                                   format(impfungen_complete,big.mark = "'"),"</b> Personen sind bereits vollständig geimpft, das heisst <b>",round(anteil_bevoelkerung,1),
                                   " Prozent</b> der Bevölkerung haben bereits zwei Impfdosen erhalten."), annotate=paste0("Stand: ",impfdaten_dw$Datum[1]))
dw_publish_chart("d7vmx")

dw_edit_chart("TeJmy",intro=paste0("En Suisse, <b>",impfungen_anteil_ch,"</b> injections pour 100 habitants ont été réalisées jusqu'ici. Cela représente en tout <b>",impfungen_ch,"</b> vaccinations. <b>",
                                   format(impfungen_complete,big.mark = "'"),"</b> personnes sont déjà entièrement vaccinées. Pour garantir une protection optimale, deux doses de vaccin sont nécessaires."), annotate=paste0("Etat: ",impfdaten_dw$Datum[1]))
dw_publish_chart("TeJmy")

#Impfentwicklung Chart d & fr
#dw_publish_chart("bnIJk")
#dw_publish_chart("XjYhi")

#Chart SDA
dw_edit_chart("pyMze",intro=paste0("In der Schweiz wurden bislang pro 100 Einwohner <b>",impfungen_anteil_ch,"</b> Impfdosen verabreicht. Das entspricht <b>",impfungen_ch,"</b> Impfungen."), annotate=paste0("Stand: ",impfdaten_dw$Datum[1]))
dw_publish_chart("pyMze")
