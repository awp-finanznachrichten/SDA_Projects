#Funktionen

#Funktion für Retry beim Upload
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}



dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

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

excuse_my_french <- function(dta) {
  
  for (i in 1:nrow(dta)) {
    
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de A","d'A") 
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de E","d'E")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de I","d'I") 
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de O","d'O") 
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de U","d'U")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Yv","d'Yv")
    
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Les","des")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Le","du")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"à Les","aux")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"A Les","Aux")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"à Le","au")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"A Le","Au")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"du Vaud","de Le Vaud")
    
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Henniez","d'Henniez")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Hermance","d'Hermance")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"de Hermenches","d'Hermenches")
    
    
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Jura","canton du Jura")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Tessin","canton du Tessin")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Valais","canton du Valais")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"du canton de Valais","en Valais")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Argovie","canton d'Argovie")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Grisons","canton des Grisons")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Obwald","canton d'Obwald")
    dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"canton de Uri","canton d'Uri")
    
  }  
  
  
  return(dta)   
  
} 
