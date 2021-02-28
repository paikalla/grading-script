# install.packages("readxl")
# install.packages("dplyr")
library("readxl")
library("dplyr")

#########
#ALKUVALMISTELUT

# Käy nämä 4 kohtaa läpi.
# 1) Oletko tekemässä 1 vai 2 osan listaa? Muokkaa koodi.
# 2) Minkä kuun listaa olet tekemässä? Kirjoita kuun nimi pienellä alkukirjaimella.
# 3) Kirjoita etu- ja sukunimesi
# 4) Aja koko koodi ja hyppää 4-kohtaan.

# Koodin pitäisi toimia sellaisenaan, kunhan
# *) Sekä mooc-data että Annen data on working directoryssa
# *) Anne on nimennyt listan samoin kuin aiemmin.

#1)
#osa = 1
#osa = 2
print(paste("Osa", osa, "valittu."))

#2)
#kuukausi = "tammikuu"
print(paste(kuukausi, "valittu."))

#3)
# Kirjoita etu ja sukunimesi.
#nimi = 
print(nimi)

# Alkuvalmistelut loppuu. Tämän jälkeen koodin pitäisi toimia sellaisenaan. Käsin joutuu kuitenkin edelleen lisäämäään
#opiskelijat, joita ei löydy sähköpostin tai opiskelijanumeron perusteella. Koodi ei myöskään varmista, onko
# moocin datassa mukana tapauksia, joissa sama opiskelija mahdollisesti mukana kaksi kertaa ja saisi paremman
# arvosanan jos valitaan opiskelijamumeron vs sähköpostin perusteella.

#########


#Muodostetaan nimi Annen listalle. Huom. tässä tulee myöhemmässä kohtaa virhe,
#jos Anne on nimennyt listan eri tavoin kuin yleensä.
# OSalle 1 tulee koodi 103 ja osalle 2 koodi 104.
koodi = ifelse(osa ==1, "103", "104" )
annen_lista_nimi = paste0("AYVALT-", koodi, "_", kuukausi, "2021.xlsx")


#### Funktiot ####

laske_pisteet <- function(data, sarakkeet_nro) {
  rowSums(data[c(sarakkeet_nro)])
}

laske_arvosana <- function(data, sarake_nimi, pisteraja_5, pisteraja_4, pisteraja_3, pisteraja_2, pisteraja_1) {
  ifelse(test = data[sarake_nimi] >=pisteraja_5, yes = 5,
         ifelse(test = data[sarake_nimi] >=pisteraja_4, yes = 4,
                ifelse(test = data[sarake_nimi] >=pisteraja_3, yes = 3,
                       ifelse(test = data[sarake_nimi] >=pisteraja_2, yes = 2,
                              ifelse(test = data[sarake_nimi] >=pisteraja_1, yes = 1, 
                                     no = 0)))))
}

get_opiskelijanumero_by_tunnistenumero <- function(tunnistenumero) {
  osat <- unlist(strsplit(tunnistenumero, ":"))
  opiskelijanumero <- osat[length(osat)]
  eka_merkki <- substr(opiskelijanumero, start = 1, stop = 1) 
  normalisoitu_opiskelijanumero <- ifelse(
    test = eka_merkki == "0",
    yes = substr(opiskelijanumero, start = 2, stop = nchar(opiskelijanumero)),
    no = opiskelijanumero
  )
  
  return(normalisoitu_opiskelijanumero)
}

#### Aineistot ####

# Moocista tuotu data
mooc_data <- read_excel("#tilastoMOOC Arvioinnit.xlsx")
mooc_data$Opiskelijanumero <- sapply(mooc_data$Tunnistenumero, get_opiskelijanumero_by_tunnistenumero)

# Avoimen Annelta saatu suorituslista; nimetään sarakkeet
#suorituslista <- read_excel("AYVALT-103_joulukuu2020.xlsx", skip = 0)
suorituslista <- read_excel(annen_lista_nimi, skip = 0)
names(suorituslista)[2] <- "Opiskelijanumero"; names(suorituslista)[3] <- "Sähköpostiosoite"

# yhdistä aineistot ja SUODATA mooc data BY Annen suoristuslistan sähköpostiosoite
yhdistetty_lista  <- left_join(mooc_data, suorituslista, by = "Sähköpostiosoite")

suodatettu_lista_by_sahkoposti <- semi_join(yhdistetty_lista, suorituslista, by = "Sähköpostiosoite")

# OSA 1 tenttipiste-sararakkeet [8:17], osa 2 [18:27]
# valitsee tenttipistesarakkeet osan mukaan.

if (osa == 1){
  cols <- c(8:17)
}else if (osa == 2){
  cols = c(18:27)
}

# tyhjät solut "-" -> 0
suodatettu_lista_by_sahkoposti[cols] <- lapply(suodatettu_lista_by_sahkoposti[cols], gsub, pattern = "-", replacement = 0)
suodatettu_lista_by_sahkoposti[cols] <- lapply(suodatettu_lista_by_sahkoposti[cols], as.numeric)


#######
## 1 ##
#######  Tarkista pisterajat osa1/osa2
suodatettu_lista_by_sahkoposti$Yhteispisteet <- laske_pisteet(suodatettu_lista_by_sahkoposti, cols)
if (osa == 1){
  suodatettu_lista_by_sahkoposti$Arvosana <- laske_arvosana(suodatettu_lista_by_sahkoposti, "Yhteispisteet", pisteraja_5 = 180, pisteraja_4 = 160, pisteraja_3 = 140, pisteraja_2 = 120, pisteraja_1 = 100)
} else if (osa == 2){
  suodatettu_lista_by_sahkoposti$Arvosana <- laske_arvosana(suodatettu_lista_by_sahkoposti, "Yhteispisteet", pisteraja_5 = 260, pisteraja_4 = 220, pisteraja_3 = 180, pisteraja_2 = 140, pisteraja_1 = 100)
}

# NOT found BY sahkoposti -> found only BY opiskelijanumero
not_found_by_sahkoposti_list <- anti_join(suorituslista, mooc_data, by = "Sähköpostiosoite")

found_only_by_opiskelijanumero_list <- semi_join(mooc_data, not_found_by_sahkoposti_list, by = "Opiskelijanumero")

#Muokkasin tämän. complete cases. Muuten ottaa NA mukaan.
found_only_by_opiskelijanumero_list = found_only_by_opiskelijanumero_list[complete.cases(found_only_by_opiskelijanumero_list$Opiskelijanumero),]


found_only_by_opiskelijanumero_list[cols] <- lapply(found_only_by_opiskelijanumero_list[cols], gsub, pattern = "-", replacement = 0)
found_only_by_opiskelijanumero_list[cols] <- lapply(found_only_by_opiskelijanumero_list[cols], as.numeric)


#######
## 2 ##
#######  Tarkista pisterajat osa1/osa2

found_only_by_opiskelijanumero_list$Yhteispisteet <- laske_pisteet(found_only_by_opiskelijanumero_list, cols)

if (osa == 1){
  found_only_by_opiskelijanumero_list$Arvosana <- laske_arvosana(found_only_by_opiskelijanumero_list, "Yhteispisteet", pisteraja_5 = 180, pisteraja_4 = 160, pisteraja_3 = 140, pisteraja_2 = 120, pisteraja_1 = 100)
} else if (osa == 2){
  found_only_by_opiskelijanumero_list$Arvosana <- laske_arvosana(found_only_by_opiskelijanumero_list, "Yhteispisteet", pisteraja_5 = 260, pisteraja_4 = 220, pisteraja_3 = 180, pisteraja_2 = 140, pisteraja_1 = 100)
}

#### Sis. vain tarvittavat sarakkeet ####

suodatettu_lista_valmis <- suodatettu_lista_by_sahkoposti %>% 
  select(Etunimi, Sukunimi, Opiskelijanumero.y, Arvosana)

suodatettu_lista_valmis <- rename(suodatettu_lista_valmis, Opiskelijanumero = Opiskelijanumero.y)

found_only_by_opiskelijanumero_list_valmis <- found_only_by_opiskelijanumero_list %>% 
  select(Etunimi, Sukunimi, Opiskelijanumero, Arvosana)

valmis_paketti <- bind_rows(suodatettu_lista_valmis, found_only_by_opiskelijanumero_list_valmis) 

#######
## 3 ## 
####### Tallenna nimelläsi 

valmis_paketti[names(suorituslista)[1]] <- " " # kurssi-info Annen suorituslistasta
# tästä
valmis_paketti[paste("Vastuuopettaja: Kimmo Vehkalahti & listan tekijä:", nimi)] <- " "
outputfilename = paste0(kuukausi, "_Arvosanat_AYVALT-", koodi, ".xlsx")
write.xlsx(valmis_paketti, file = outputfilename)


#######
## 4 ##
####### KIRJAA KÄSIN: näillä ei tunnistetietoja!

not_found_by_email_or_opiskelijanro_list <- anti_join(not_found_by_sahkoposti_list, mooc_data, by = "Opiskelijanumero")

# Pieni tarkistus
if (nrow(valmis_paketti) + nrow(not_found_by_email_or_opiskelijanro_list) == nrow(suorituslista)-1){
  print(paste0("Muista lisätä vielä käsinkirjattavat opiskelijat (4-kohta), muuttoin Kaikki mukana, hyvää työtä ", nimi, "! :)"))
} else{
  print("Jokin saattaa olla pielessä, rivien määrät eivät täsmää.")
}


