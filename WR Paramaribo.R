library("data.table")
rm(list=ls())

setwd("C:/Users/Rick/Downloads")

WR <- fread("/home/tuinschepje/Surfdrive/CLARIAH/Thunnis/WR28-47 20240606 dataverse.csv", encoding="UTF-8")
WR$gender <- ifelse(tolower(WR$sex)=="m", "schema:Male",
                    ifelse(tolower(WR$sex)=="v" | WR$weduwe=="weduwe", "schema:Female", NA))
#WR <- WR[WR$`Kaart Id`=="1838a222",]
WR$Achternaam2 <- trimws(ifelse(is.na(WR$Tussenvoegsel), WR$Achternaam, paste(WR$Tussenvoegsel, WR$Achternaam)))
WR$Partnernaam <- trimws(ifelse(is.na(WR$Fn_Dec_Spouse), WR$Achternaam2, paste(WR$Fn_Dec_Spouse, WR$Achternaam2)))
WR$Archieftoegang[is.na(WR$Archieftoegang)] <- ""
WR$Straatnaam[is.na(WR$Straatnaam)] <- ""
WR$Huisnummer[is.na(WR$Huisnummer)] <- ""
WR$'Aantal Slaafgemaakten'[is.na(WR$'Aantal Slaafgemaakten')] <- 0
WR$'Aantal Slaafgemaakten' <- ifelse(is.na(WR$Enslaved), WR$'Aantal Slaafgemaakten', WR$'Aantal Slaafgemaakten'-1)
WR$'Aantal Slaafgemaakten'[WR$Id==124812] <- 2
WR$imp_enslaved <- ifelse(WR$'Aantal Slaafgemaakten'>0, paste0("hdsc:", WR$Id, "e", 1), NA)
x <- 1
repeat{
  x <- x + 1
  WR$imp_enslaved <- ifelse(WR$'Aantal Slaafgemaakten'>=x, paste(WR$imp_enslaved, paste0("hdsc:", WR$Id, "e", x),sep=","), WR$imp_enslaved)
  if(x==max(WR$'Aantal Slaafgemaakten',na.rm=T)){break}
}
rm(x)
WR$Xma2 <- as.numeric(ifelse(is.na(WR$Xma) | WR$Xma==" ", 0, WR$Xma))
WR$Xfa2 <- as.numeric(ifelse(is.na(WR$Xfa) | WR$Xfa==" ", 0, WR$Xfa)); WR$Xfa2 <- WR$Xma2+WR$Xfa2
WR$Xmc2 <- as.numeric(ifelse(is.na(WR$Xmc) | WR$Xmc==" ", 0, WR$Xmc)); WR$Xmc2 <- WR$Xfa2+WR$Xmc2
WR$Xfc2 <- as.numeric(ifelse(is.na(WR$Xfc) | WR$Xfc==" ", 0, WR$Xfc)); WR$Xfc2 <- WR$Xmc2+WR$Xfc2
WR$Bma2 <- as.numeric(ifelse(is.na(WR$Bma) | WR$Bma==" ", 0, WR$Bma)); WR$Bma2 <- WR$Xfc2+WR$Bma2
WR$Bfa2 <- as.numeric(ifelse(is.na(WR$Bfa) | WR$Bfa==" ", 0, WR$Bfa)); WR$Bfa2 <- WR$Bma2+WR$Bfa2
WR$Bmc2 <- as.numeric(ifelse(is.na(WR$Bmc) | WR$Bmc==" ", 0, WR$Bmc)); WR$Bmc2 <- WR$Bfa2+WR$Bmc2
WR$Bfc2 <- as.numeric(ifelse(is.na(WR$Bfc) | WR$Bfc==" ", 0, WR$Bfc)); WR$Bfc2 <- WR$Bmc2+WR$Bfc2
WR$Unk2 <- as.numeric(ifelse(is.na(WR$Unk) | WR$Unk==" ", 0, WR$Unk)); WR$Unk2 <- WR$Bfc2+WR$Unk2

#
card <- WR[!duplicated(WR$'Kaart Id'),]
partner <- WR[which(WR$Weduwe=="weduwe"),]
free <- WR[which(is.na(WR$Enslaved) & WR$Voornaam!="" | is.na(WR$Enslaved) & WR$Achternaam!=""),]
exp_enslaved <- WR[which(WR$Enslaved==1),]
enslaved <- WR[which(WR$'Aantal Slaafgemaakten'>0),]

#namespaces
prefix <- "@prefix enslaved:        <http://www.enslaved.org/1.0> .
@prefix hdsc:            <https://www.ru.nl/hdsc/example/> .
@prefix pico:			 <https://personsincontext.org/ontology/> .
@prefix picot:			 <https://terms.personsincontext.org/ThesaurusHistorischePersoonsgegevens/> .
@prefix pnv:			 <https://w3id.org/pnv#> .
@prefix prov:			 <http://www.w3.org/ns/prov#> .
@prefix schema:			 <https://www.schema.org/> .
@prefix xsd:			 <http://www.w3.org/2000/10/XMLSchema#> .
"

#source
source_ttl <- paste0("hdsc:", card$'Kaart Id', " 
    a schema:ArchiveComponent ; 
    schema:name \"", card$Register, " ", card$Archieftoegang, ", ", card$Wijk, ", ", card$Jaar, ", ", card$Straatnaam, " ", card$Huisnummer, "\" ; 
    schema:locationCreated \"", "Paramaribo", "\" ; 
    schema:dateCreated \"", card$Jaar, "\"^^xsd:gYear ; 
    schema:image <", card$Scan, "> ; 
    schema:holdingArchive <https://www.nationaalarchief.nl> .
")
source_ttl <- gsub("    schema:image NA ; 
", "", source_ttl)

#address
card$Adress_Remarks[card$Adress_Remarks==""] <- NA
card1 <- card[which(is.na(card$Adress_Remarks)),]
  address_ttl1 <- paste0("hdsc:", card1$'Kaart Id', "_address 
    a schema:PostalAddress ; 
    schema:streetAddress \"", card1$Straatnaam, " ", card1$Huisnummer, "\" ; 
    hdsc:wijk \"", card1$Wijk, "\" ; 
    hdsc:streetname \"", card1$Straatnaam, "\" ; 
    hdsc:houseNumber \"", card1$Huisnummer, "\" ; 
    prov:hadPrimarySource hdsc:", card1$'Kaart Id', " .
")
card2 <- card[which(!is.na(card$Adress_Remarks)),]
  address_ttl2 <- paste0("hdsc:", card2$'Kaart Id', "_address  
    a schema:PostalAddress ; 
    schema:streetAddress \"", card2$Straatnaam, " ", card2$Huisnummer, "\" ; 
    hdsc:wijk \"", card2$Wijk, "\" ; 
    hdsc:streetname \"", card2$Straatnaam, "\" ; 
    hdsc:houseNumber \"", card2$Huisnummer, "\" ; 
    hdsc:locationRemark \"", card2$Adress_Remarks, "\" ; 
    prov:hadPrimarySource hdsc:", card2$'Kaart Id', " .
")
address_ttl <- c(address_ttl1, address_ttl2)
rm(address_ttl1, address_ttl2)
rm(card, card1, card2)
address_ttl <- gsub("    schema:streetAddress \" \" ; 
", "", address_ttl)
address_ttl <- gsub("    hdsc:streetname \"\" ; 
", "", address_ttl)
address_ttl <- gsub("    hdsc:houseNumber \"\" ; 
", "", address_ttl)
address_ttl <- gsub("\"\"a\"\"", "a", address_ttl)

#explicit persons
free$Weduwe[free$Weduwe==""] <- NA
free1 <- free[which(is.na(free$Weduwe)),] #geen weduwe
  free_ttl1 <- paste0("hdsc:", free1$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", free1$'Kaart Id', " ; 
    schema:name \"", free1$Voornaam, " ", free1$Achternaam2, "\" ; 
    schema:familyName \"", free1$Achternaam2, "\" ; 
    schema:givenName \"", free1$Voornaam, "\" ; 
    schema:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", free1$Voornaam, " ", free1$Achternaam2, "\" ; 
        pnv:baseSurname \"", free1$Achternaam, "\" ; 
        pnv:surnamePrefix \"", free1$Tussenvoegsel, "\" ; ] ; 
    schema:address hdsc:", free1$'Kaart Id', "_address ; 
    schema:gender ", free1$gender, " ; 
    schema:hasOccupation \"", free1$Beroep, "\" ; 
    pico:hasAge \"", free1$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", free1$Religie, "\" ; 
    pico:hasRole ", "picot:490", " ; 
    hdsc:hasRacialCategory \"", free1$Etniciteit, "\" ; 
    hdsc:isEnslaverOf ", free1$imp_enslaved, " .
")
free2 <- free[which(!is.na(free$Weduwe)),] #wel weduwe, geen enslaved
  free_ttl2 <- paste0("hdsc:", free2$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", free2$'Kaart Id', " ; 
    schema:name \"", free2$Voornaam, " ", free2$Meisjesnaam, ", weduwe ", free2$Partnernaam, "\" ; 
    schema:familyName \"", free2$Meisjesnaam, "\" ; 
    schema:givenName \"", free2$Voornaam, "\" ; 
    schema:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", free2$Voornaam, " ", free2$Meisjesnaam, ", weduwe ", free2$Partnernaam, "\" ; 
        pnv:baseSurname \"", free2$Meisjesnaam, "\" ; 
        pnv:surnamePrefix \"", ifelse(grepl(" ", free2$Meisjesnaam), gsub(" .*", "", free2$Meisjesnaam), NA), "\" ; ] ; 
    schema:address hdsc:", free2$'Kaart Id', "_address ; 
    schema:gender ", free2$gender, " ; 
    schema:hasOccupation \"", free2$Beroep, "\" ; 
    pico:hasAge \"", free2$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", free2$Religie, "\" ; 
    pico:hasRole ", "picot:490", " ; 
    hdsc:hasRacialCategory \"", free2$Etniciteit, "\" ; 
    schema:spouse hdsc:", free2$Id, "p ; 
    hdsc:isEnslaverOf ", free2$imp_enslaved, " .
")
free_ttl <- c(free_ttl1, free_ttl2)
rm(free_ttl1, free_ttl2)
rm(free, free1, free2)
free_ttl <- gsub("    schema:gender NA ; 
", "", free_ttl)
free_ttl <- gsub("    schema:gender  ; 
", "", free_ttl)
free_ttl <- gsub("    schema:hasOccupation NA ; 
", "", free_ttl)
free_ttl <- gsub("    schema:hasOccupation \"\" ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasAge \"NA\"^^xsd:decimal ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasAge \"\"..xsd:decimal ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasReligion NA ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasReligion \"\" ; 
", "", free_ttl)
free_ttl <- gsub(" ; 
    hdsc:hasRacialCategory \"NA\"", "", free_ttl)
free_ttl <- gsub(" ; 
    hdsc:hasRacialCategory \"\"", "", free_ttl)
free_ttl <- gsub("; 
    schema:givenName \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
    schema:givenName \"\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:surnamePrefix \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:surnamePrefix \"\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:baseSurname \"\" ", "", free_ttl)
free_ttl <- gsub("; 
    schema:familyName \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
    schema:familyName \"\" ", "", free_ttl)
free_ttl <- gsub("; 
    hdsc:isEnslaverOf NA", "", free_ttl)
free_ttl <- gsub("\" , weduwe", "\"weduwe", free_ttl)
free_ttl <- gsub("\"\"Slaven\"\"", "Slaven", free_ttl)


#implicit spouses
partner_ttl <- paste0("hdsc:", partner$Id, "p 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", partner$'Kaart Id', " ; 
    schema:name \"", partner$Partnernaam, "\" ; 
    schema:familyName \"", partner$Achternaam2, "\" ; 
    schema:givenName \"", partner$Fn_Dec_Spouse, "\" ; 
    schema:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", partner$Partnernaam, "\" ; 
        pnv:baseSurname \"", partner$Achternaam, "\" ; 
        pnv:surnamePrefix \"", partner$Tussenvoegsel, "\" ; ] ; 
    schema:gender ", ifelse(partner$gender=="schema:Female", "schema:Male", 
                            ifelse(partner$gender=="schema:Male", "schema:Female", NA)), " ; 
    pico:deceased true ; 
    schema:spouse hdsc:", partner$Id, " .
")
rm(partner)
partner_ttl <- gsub("    schema:gender NA ; 
", "", partner_ttl)
partner_ttl <- gsub("; 
    schema:givenName \"\" ", "", partner_ttl)
partner_ttl <- gsub("; 
    schema:familyName \"\" ", "", partner_ttl)
partner_ttl <- gsub("; 
        pnv:surnamePrefix \"NA\" ", "", partner_ttl)
partner_ttl <- gsub("; 
        pnv:surnamePrefix \"\" ", "", partner_ttl)
partner_ttl <- gsub("; 
        pnv:baseSurname \"\" ", "", partner_ttl)


#explicit enslaved
exp_enslaved$Voornaam[exp_enslaved$Voornaam==""] <- NA
exp_enslaved$Achternaam2[exp_enslaved$Achternaam2==""] <- NA
exp_enslaved$Naam <- ifelse(is.na(exp_enslaved$Voornaam) & is.na(exp_enslaved$Achternaam2), NA,
                            ifelse(!is.na(exp_enslaved$Voornaam) & is.na(exp_enslaved$Achternaam2), exp_enslaved$Voornaam, paste(exp_enslaved$Voornaam, exp_enslaved$Achternaam2)))

exp_enslaved_ttl <- paste0("hdsc:", exp_enslaved$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", exp_enslaved$'Kaart Id', " ; 
    schema:name \"", exp_enslaved$Naam, "\" ; 
    schema:familyName \"", exp_enslaved$Achternaam2, "\" ; 
    schema:givenName \"", exp_enslaved$Voornaam, "\" ; 
    schema:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", exp_enslaved$Naam, "\" ; 
        pnv:baseSurname \"", exp_enslaved$Achternaam, "\" ; 
        pnv:surnamePrefix \"", exp_enslaved$Tussenvoegsel, "\" ; ] ; 
    schema:address hdsc:", exp_enslaved$'Kaart Id', "_address ; 
    schema:gender ", exp_enslaved$gender, " ; 
    schema:hasOccupation \"", exp_enslaved$Beroep, "\" ; 
    pico:hasAge \"", exp_enslaved$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", exp_enslaved$Religie, "\" ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory \"", exp_enslaved$Etniciteit, "\" .
")
rm(exp_enslaved)
exp_enslaved_ttl <- gsub("    schema:gender NA ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    schema:hasOccupation NA ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    schema:hasOccupation \"\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasAge \"NA\"^^xsd:decimal ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasAge \"\"..xsd:decimal ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasReligion \"NA\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasReligion \"\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub(" ; 
    hdsc:hasRacialCategory \"NA\"", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub(" ; 
    hdsc:hasRacialCategory \"\"", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("; 
        pnv:surnamePrefix \"NA\" ", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("; 
        pnv:surnamePrefix \"\" ", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("; 
        pnv:baseSurname \"\" ", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("\"\"Slaven\"\"", "Slaven", exp_enslaved_ttl)

#implicit enslaved
enslaved$imp_enslaved <- ""
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xma2>=x, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Male", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q425 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:kleurling", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xma2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xfa2>=x & x>enslaved$Xma2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Female", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "address ; 
    hdsc:hasAgeCategory enslaved:Q425 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:kleurling", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xfa2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xmc2>=x & x>enslaved$Xfa2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Male", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q427 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:kleurling", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xmc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xfc2>=x & x>enslaved$Xmc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Female", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q427 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:kleurling", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xfc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bma2>=x & x>enslaved$Xfc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Male", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q425 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:zwart", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bma2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bfa2>=x & x>enslaved$Bma2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Female", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q425 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:zwart", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bfa2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bmc2>=x & x>enslaved$Bfa2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Male", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q427 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:zwart", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bmc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bfc2>=x & x>enslaved$Bmc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:gender ", "schema:Female", " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    hdsc:hasAgeCategory enslaved:Q427 ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:hasRacialCategory ", "hdsc:zwart", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bfc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Unk2>=x & x>enslaved$Bfc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:", enslaved$'Kaart Id', " ; 
    schema:address hdsc:", enslaved$'Kaart Id', "_address ; 
    pico:hasRole ", "picot:490", " ; 
    pico:hasRole ", "enslaved:Q109", " ; 
    hdsc:isEnslavedBy ", "hdsc:", enslaved$Id, " .
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Unk2)){break}
}
rm(x)


#bind
ttl <- c(prefix, source_ttl, address_ttl, free_ttl, partner_ttl, exp_enslaved_ttl, enslaved$imp_enslaved)
ttl <- gsub(" ,", ",", ttl)


#write ttl
write.table(ttl,
            paste0("WR Paramaribo ", Sys.Date(), ".ttl"),
            quote=F,
            sep = "\t", 
            col.names=F,
            row.names=F,
            fileEncoding="UTF-8")
