library("data.table")
rm(list=ls())

setwd("/home/tuinschepje/Surfdrive/CLARIAH/Thunnis")

WR <- fread("WR28-47 20240606 dataverse.csv", encoding="UTF-8")
WR$gender <- ifelse(tolower(WR$sex)=="m", "sdo:Male",
                    ifelse(tolower(WR$sex)=="v" | WR$weduwe=="weduwe", "sdo:Female", NA))
#WR <- WR[WR$`Kaart Id`=="1838a222",]
WR$Achternaam2 <- trimws(ifelse(is.na(WR$Tussenvoegsel), WR$Achternaam, paste(WR$Tussenvoegsel, WR$Achternaam)))
WR$Partnernaam <- trimws(ifelse(is.na(WR$Fn_Dec_Spouse), WR$Achternaam2, paste(WR$Fn_Dec_Spouse, WR$Achternaam2)))
WR$Archieftoegang[is.na(WR$Archieftoegang)] <- ""
WR$Straatnaam[is.na(WR$Straatnaam)] <- ""
WR$Huisnummer[is.na(WR$Huisnummer)] <- ""
WR$Leeftijd <- as.numeric(gsub(",", ".", WR$Leeftijd))
WR$'Aantal Slaafgemaakten'[is.na(WR$'Aantal Slaafgemaakten')] <- 0
WR$'Aantal Slaafgemaakten' <- ifelse(is.na(WR$Enslaved), WR$'Aantal Slaafgemaakten', WR$'Aantal Slaafgemaakten'-1)
WR$'Aantal Slaafgemaakten'[WR$Id==124812] <- 2
WR$imp_enslaved <- ifelse(WR$'Aantal Slaafgemaakten'>0, paste0("hdsc:WR-Suriname\\/observation\\/", WR$Id, "e", 1), NA)
x <- 1
repeat{
  x <- x + 1
  WR$imp_enslaved <- ifelse(WR$'Aantal Slaafgemaakten'>=x, paste(WR$imp_enslaved, paste0("hdsc:WR-Suriname\\/observation\\/", WR$Id, "e", x),sep=","), WR$imp_enslaved)
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
prefix <- "@prefix ed:    <http://lod.enslaved.org/entity/> .
@prefix hdsc:  <https://www.ru.nl/hdsc/example/> .
@prefix pico:  <https://personsincontext.org/model#> .
@prefix picot: <https://terms.personsincontext.org/> .
@prefix pnv:   <https://w3id.org/pnv#> .
@prefix prov:  <http://www.w3.org/ns/prov#> .
@prefix sdo:   <https://www.schema.org/> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
"

#source
source_ttl <- paste0("hdsc:WR-Suriname\\/source\\/", card$'Kaart Id', " 
    a sdo:ArchiveComponent ; 
    sdo:name \"", card$Register, " ", card$Archieftoegang, ", ", card$Wijk, ", ", card$Jaar, ", ", card$Straatnaam, " ", card$Huisnummer, "\" ; 
    sdo:locationCreated \"", "Paramaribo", "\" ; 
    sdo:dateCreated \"", card$Jaar, "\"^^xsd:gYear ; 
    sdo:image <", card$Scan, "> ; 
    sdo:holdingArchive <https://www.nationaalarchief.nl> .
")
source_ttl <- gsub("    sdo:image NA ; 
", "", source_ttl)

#address
card$Adress_Remarks[card$Adress_Remarks==""] <- NA
card1 <- card[which(is.na(card$Adress_Remarks)),]
  address_ttl1 <- paste0("hdsc:WR-Suriname\\/location\\/", card1$'Kaart Id', "_address 
    a sdo:PostalAddress ; 
    sdo:streetAddress \"", card1$Straatnaam, " ", card1$Huisnummer, "\" ; 
    hdsc:wijk \"", card1$Wijk, "\" ; 
    hdsc:streetname \"", card1$Straatnaam, "\" ; 
    hdsc:houseNumber \"", card1$Huisnummer, "\" ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", card1$'Kaart Id', " .
")
card2 <- card[which(!is.na(card$Adress_Remarks)),]
  address_ttl2 <- paste0("hdsc:WR-Suriname\\/location\\/", card2$'Kaart Id', "_address  
    a sdo:PostalAddress ; 
    sdo:streetAddress \"", card2$Straatnaam, " ", card2$Huisnummer, "\" ; 
    hdsc:wijk \"", card2$Wijk, "\" ; 
    hdsc:streetname \"", card2$Straatnaam, "\" ; 
    hdsc:houseNumber \"", card2$Huisnummer, "\" ; 
    sdo:description \"", card2$Adress_Remarks, "\" ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", card2$'Kaart Id', " .
")
address_ttl <- c(address_ttl1, address_ttl2)
rm(address_ttl1, address_ttl2)
rm(card, card1, card2)
address_ttl <- gsub("    sdo:streetAddress \" \" ; 
", "", address_ttl)
address_ttl <- gsub("    hdsc:streetname \"\" ; 
", "", address_ttl)
address_ttl <- gsub("    hdsc:houseNumber \"\" ; 
", "", address_ttl)
address_ttl <- gsub("\"\"a\"\"", "a", address_ttl)

#explicit persons
free$Weduwe[free$Weduwe==""] <- NA
free1 <- free[which(is.na(free$Weduwe)),] #geen weduwe
  free_ttl1 <- paste0("hdsc:WR-Suriname\\/observation\\/", free1$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", free1$'Kaart Id', " ; 
    sdo:name \"", free1$Voornaam, " ", free1$Achternaam2, "\" ; 
    sdo:familyName \"", free1$Achternaam2, "\" ; 
    sdo:givenName \"", free1$Voornaam, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", free1$Voornaam, " ", free1$Achternaam2, "\" ; 
        pnv:baseSurname \"", free1$Achternaam, "\" ; 
        pnv:surnamePrefix \"", free1$Tussenvoegsel, "\" ; ] ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", free1$'Kaart Id', "_address ; 
    sdo:gender ", free1$gender, " ; 
    sdo:hasOccupation \"", free1$Beroep, "\" ; 
    pico:hasAge \"", free1$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", free1$Religie, "\" ; 
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P32 \"", free1$Etniciteit, "\" ;  #hasRaceOrColor
    hdsc:isEnslaverOf ", free1$imp_enslaved, " . 
")
free2 <- free[which(!is.na(free$Weduwe)),] #wel weduwe, geen enslaved
  free_ttl2 <- paste0("hdsc:WR-Suriname\\/observation\\/", free2$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", free2$'Kaart Id', " ; 
    sdo:name \"", free2$Voornaam, " ", free2$Meisjesnaam, ", weduwe ", free2$Partnernaam, "\" ; 
    sdo:familyName \"", free2$Meisjesnaam, "\" ; 
    sdo:givenName \"", free2$Voornaam, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", free2$Voornaam, " ", free2$Meisjesnaam, ", weduwe ", free2$Partnernaam, "\" ; 
        pnv:baseSurname \"", free2$Meisjesnaam, "\" ; 
        pnv:surnamePrefix \"", ifelse(grepl(" ", free2$Meisjesnaam), gsub(" .*", "", free2$Meisjesnaam), NA), "\" ; ] ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", free2$'Kaart Id', "_address ; 
    sdo:gender ", free2$gender, " ; 
    sdo:hasOccupation \"", free2$Beroep, "\" ; 
    pico:hasAge \"", free2$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", free2$Religie, "\" ; 
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P32 \"", free2$Etniciteit, "\" ;  #hasRaceOrColor
    sdo:spouse hdsc:WR-Suriname\\/observation\\/", free2$Id, "p ; 
    hdsc:isEnslaverOf ", free2$imp_enslaved, " . 
")
free_ttl <- c(free_ttl1, free_ttl2)
rm(free_ttl1, free_ttl2)
rm(free, free1, free2)
free_ttl <- gsub("    sdo:gender NA ; 
", "", free_ttl)
free_ttl <- gsub("    sdo:gender  ; 
", "", free_ttl)
free_ttl <- gsub("    sdo:hasOccupation NA ; 
", "", free_ttl)
free_ttl <- gsub("    sdo:hasOccupation \"\" ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasAge \"NA\"..xsd:decimal ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasAge \"\"..xsd:decimal ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasReligion NA ; 
", "", free_ttl)
free_ttl <- gsub("    pico:hasReligion \"\" ; 
", "", free_ttl)
free_ttl <- gsub(" ;  #bewoner
    ed:P32 \"NA\"", "", free_ttl)
free_ttl <- gsub(" ;  #bewoner
    ed:P32 \"\"", "", free_ttl)
free_ttl <- gsub("; 
    sdo:givenName \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
    sdo:givenName \"\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:surnamePrefix \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:surnamePrefix \"\" ", "", free_ttl)
free_ttl <- gsub("; 
        pnv:baseSurname \"\" ", "", free_ttl)
free_ttl <- gsub("; 
    sdo:familyName \"NA\" ", "", free_ttl)
free_ttl <- gsub("; 
    sdo:familyName \"\" ", "", free_ttl)
free_ttl <- gsub(";  #hasRaceOrColor
    hdsc:isEnslaverOf NA", "", free_ttl)
free_ttl <- gsub("; 
    hdsc:isEnslaverOf NA", "", free_ttl)
free_ttl <- gsub(';  #bewoner
    ed:P32 ""', "", free_ttl)
free_ttl <- gsub("\" , weduwe", "\"weduwe", free_ttl)
free_ttl <- gsub("\"\"Slaven\"\"", "Slaven", free_ttl)


#implicit spouses
partner_ttl <- paste0("hdsc:WR-Suriname\\/observation\\/", partner$Id, "p 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", partner$'Kaart Id', " ; 
    sdo:name \"", partner$Partnernaam, "\" ; 
    sdo:familyName \"", partner$Achternaam2, "\" ; 
    sdo:givenName \"", partner$Fn_Dec_Spouse, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", partner$Partnernaam, "\" ; 
        pnv:baseSurname \"", partner$Achternaam, "\" ; 
        pnv:surnamePrefix \"", partner$Tussenvoegsel, "\" ; ] ; 
    sdo:gender ", ifelse(partner$gender=="sdo:Female", "sdo:Male", 
                            ifelse(partner$gender=="sdo:Male", "sdo:Female", NA)), " ; 
    pico:deceased true ; 
    sdo:spouse hdsc:WR-Suriname\\/observation\\/", partner$Id, " .
")
rm(partner)
partner_ttl <- gsub("    sdo:gender NA ; 
", "", partner_ttl)
partner_ttl <- gsub("; 
    sdo:givenName \"\" ", "", partner_ttl)
partner_ttl <- gsub("; 
    sdo:familyName \"\" ", "", partner_ttl)
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

exp_enslaved_ttl <- paste0("hdsc:WR-Suriname\\/observation\\/", exp_enslaved$Id, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", exp_enslaved$'Kaart Id', " ; 
    sdo:name \"", exp_enslaved$Naam, "\" ; 
    sdo:givenName \"", exp_enslaved$Voornaam, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", exp_enslaved$Naam, "\" ; ] ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", exp_enslaved$'Kaart Id', "_address ; 
    sdo:gender ", exp_enslaved$gender, " ; 
    sdo:hasOccupation \"", exp_enslaved$Beroep, "\" ; 
    pico:hasAge \"", exp_enslaved$Leeftijd, "\"^^xsd:decimal ; 
    pico:hasReligion \"", exp_enslaved$Religie, "\" ; 
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 \"", exp_enslaved$Etniciteit, "\" .  #hasRaceOrColor
")
rm(exp_enslaved)
exp_enslaved_ttl <- gsub("    sdo:gender NA ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    sdo:hasOccupation NA ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    sdo:hasOccupation \"\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasAge \"NA\"..xsd:decimal ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasAge \"\"..xsd:decimal ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasReligion \"NA\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub("    pico:hasReligion \"\" ; 
", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub(" ;  #hasPersonStatus #enslaved person
    ed:P32 \"NA\"", "", exp_enslaved_ttl)
exp_enslaved_ttl <- gsub(" ;  #hasPersonStatus #enslaved person
    ed:P32 \"\"", "", exp_enslaved_ttl)
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
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Male", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q425 ;  #hasAgeCategory  adult age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:kleurling", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xma2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xfa2>=x & x>enslaved$Xma2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Female", " ; 
    sdo:address hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', "address ; 
    ed:P4 ed:Q425 ;  #hasAgeCategory  adult age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:kleurling", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xfa2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xmc2>=x & x>enslaved$Xfa2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Male", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q427 ;  #hasAgeCategory  child age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:kleurling", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xmc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Xfc2>=x & x>enslaved$Xmc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Female", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q427 ;  #hasAgeCategory  child age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:kleurling", " ; 
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Xfc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bma2>=x & x>enslaved$Xfc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Male", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q425 ;  #hasAgeCategory  adult age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:zwart", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bma2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bfa2>=x & x>enslaved$Bma2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Female", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q425 ;  #hasAgeCategory  adult age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:zwart", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bfa2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bmc2>=x & x>enslaved$Bfa2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Male", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q427 ;  #hasAgeCategory  child age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:zwart", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bmc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Bfc2>=x & x>enslaved$Bmc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:gender ", "sdo:Female", " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    ed:P4 ed:Q427 ;  #hasAgeCategory  child age group
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    ed:P32 ", "hdsc:zwart", " ;  #hasRaceOrColor
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
")), 
                                  enslaved$imp_enslaved)
  if(x==max(enslaved$Bfc2)){break}
}
x <- 0
repeat{
  x <- x + 1
  enslaved$imp_enslaved <- ifelse(enslaved$Unk2>=x & x>enslaved$Bfc2, paste0(enslaved$imp_enslaved, 
                                                           paste0("hdsc:WR-Suriname\\/observation\\/", enslaved$Id, "e", x, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:WR-Suriname\\/source\\/", enslaved$'Kaart Id', " ; 
    sdo:address hdsc:WR-Suriname\\/location\\/", enslaved$'Kaart Id', "_address ; 
    pico:hasRole ", "picot:roles\\/490", " ;  #bewoner
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    hdsc:isEnslavedBy ", "hdsc:WR-Suriname\\/observation\\/", enslaved$Id, " . 
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
