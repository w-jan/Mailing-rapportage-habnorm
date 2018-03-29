library (mailR)
library (dplyr) #voor de piping %>% 

# inlezen data (in ASCI-US-formaat, om accenten te vermijden)
mdata <- readr::read_csv2("mailing_habnorm_2018_Kruistabel.txt")

# verbeteren veldnamen (mbt kleine letters, veldnamen met enkel een cijfer )
mdata <- mdata %>% janitor::clean_names()
maxpdf <-ncol(mdata)-6
pdfs <- as.vector(paste("pdf",1:maxpdf, sep=""))
colnames(mdata)[7:(7+maxpdf-1)] <- pdfs

library(knitr)  # voor het renderen (= omzetten van rmd-formaat naar een ander formaat hier een html). NB rmarkdown::render geeft een                   gedocumenteerde fout bij het aanmaken van een mail)
library(memisc) # gebruik van functie cases (alternatief voor geneste if else )

for (i in 91:nrow(mdata)){
# for (i in 106:106){
  
  knit2html("mailmerge_Habnorm_nieuw.Rmd", encoding = "iso-8859-1")
  # rmarkdown::render("mailmerge_Habnorm_nieuw.Rmd") werkt niet !

  # bijlagen selecteren, alle in het formaat 'persoonsnaam_naam gebied_organisatie.pdf'
  bijlagen <-""
  for (j in maxpdf:1){
    veldnaam <- paste0("pdf",j)
    if (!is.na(mdata[i,veldnaam])) {
      gebiedsnaam <-substring(mdata[i,veldnaam] ,16 )
      bijlagen <- c(bijlagen, paste0(mdata$naam[i], "_", gebiedsnaam, ".pdf"))
    }
  }
  bijlagen <- bijlagen [2:length(bijlagen)]
  
  # opstellen van een (gepersonaliseerde) lijst van gebieden, gepuurd uit de naam van de pdf
  lijstgebiedsnamen <-""
  for (j in maxpdf:1){
    veldnaam <- paste0("pdf",j)
    if (!is.na(mdata[i,veldnaam])) {
      gebiedsnaam <-substring(mdata[i,veldnaam] ,16, regexpr("_",mdata[i,veldnaam])-1 )
      lijstgebiedsnamen <- c(lijstgebiedsnamen, gebiedsnaam)
    }
  }
  lijstgebiedsnamen <- lijstgebiedsnamen [2:length(lijstgebiedsnamen)]
  
  # opstellen van een merge-veld voor de ontwerp-titel van de mail
  groepgebiedsnaam <-    cases(
    aantal_gebieden > 3 -> "meerdere gebieden in uw beheerregio",
    aantal_gebieden == 3 -> paste0(lijstgebiedsnamen[1], ", ", lijstgebiedsnamen[2], " en ", lijstgebiedsnamen[3]),
    aantal_gebieden == 2 -> paste0(lijstgebiedsnamen[1], " en ", lijstgebiedsnamen[2]),
    aantal_gebieden == 1 -> lijstgebiedsnamen[1]
  )  

  # het versturen van de mail, omdat de bijlagen geen padnaam hebben, moeten ze in de projectmap zitten
  send.mail(from = "jan.wouters@inbo.be",
            to = c(mdata$email[i]),
            # to = "naams@inbo.be",

            subject = paste("Onderzoek milieucondities habitattypen - rapportage van ruwe data voor", groepgebiedsnaam),
            body = "mailmerge_Habnorm_nieuw.html",
            encoding = "utf-8",
            html = T,
            inline = T,
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "naam@inbo.be", passwd = "in te vullen", ssl = TRUE),
            attach.files = as.character(bijlagen),
            authenticate = TRUE,
            send = TRUE)
  # controle toevoegen naar wie het laatst een mail verstuurd werd
  write.csv2(c(i,mdata$email[i]), "logmails.csv")
}


