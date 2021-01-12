# install.packages("readxl")
# install.packages("dplyr")
library("readxl")
library("dplyr")

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
mooc_data <- read_excel("#tilastoMOOC Arvioinnit.xlsx")  %>% replace(is.na(.), 0)
mooc_data$Opiskelijanumero <- sapply(mooc_data$Tunnistenumero, get_opiskelijanumero_by_tunnistenumero)

# Avoimen Annelta saatu suorituslista; nimetään sarakkeet
suorituslista <- read_excel("AYVALT-103_joulukuu2020.xlsx", skip = 0)
names(suorituslista)[2] <- "Opiskelijanumero"; names(suorituslista)[3] <- "Sähköpostiosoite"

# yhdistä aineistot ja SUODATA mooc data BY Annen suoristuslistan sähköpostiosoite
yhdistetty_lista  <- left_join(mooc_data, suorituslista, by = "Sähköpostiosoite")
suodatettu_lista_by_sahkoposti <- semi_join(yhdistetty_lista, suorituslista, by = "Sähköpostiosoite")

# OSA 1 tenttipiste-sararakkeet [8:17]
cols <- c(8:17) 

# tyhjät solut "-" -> 0
suodatettu_lista_by_sahkoposti[cols] <- lapply(suodatettu_lista_by_sahkoposti[cols], gsub, pattern = "-", replacement = 0)
suodatettu_lista_by_sahkoposti[cols] <- lapply(suodatettu_lista_by_sahkoposti[cols], as.numeric)


#######
## 1 ##
#######  Tarkista pisterajat osa1/osa2?

suodatettu_lista_by_sahkoposti$Yhteispisteet <- laske_pisteet(suodatettu_lista_by_sahkoposti, cols)
suodatettu_lista_by_sahkoposti$Arvosana <- laske_arvosana(suodatettu_lista_by_sahkoposti, "Yhteispisteet", pisteraja_5 = 180, pisteraja_4 = 160, pisteraja_3 = 140, pisteraja_2 = 120, pisteraja_1 = 100)


### EIVÄT löydy sähköpostiosoitteella mutta löytyvät tunnistenumerolla ###
##########################################################################
not_found_by_sahkoposti_list <- anti_join(suorituslista, mooc_data, by = "Sähköpostiosoite")
found_only_by_opiskelijanumero_list <- semi_join(mooc_data, not_found_by_sahkoposti_list, by = "Opiskelijanumero")

cols <- c(8:17) 

found_only_by_opiskelijanumero_list[cols] <- lapply(found_only_by_opiskelijanumero_list[cols], gsub, pattern = "-", replacement = 0)
found_only_by_opiskelijanumero_list[cols] <- lapply(found_only_by_opiskelijanumero_list[cols], as.numeric)
##########################################################################


#######
## 2 ##
#######  Tarkista pisterajat osa1/osa2?

found_only_by_opiskelijanumero_list$Yhteispisteet <- laske_pisteet(found_only_by_opiskelijanumero_list, cols)
found_only_by_opiskelijanumero_list$Arvosana <- laske_arvosana(found_only_by_opiskelijanumero_list, "Yhteispisteet", pisteraja_5 = 180, pisteraja_4 = 160, pisteraja_3 = 140, pisteraja_2 = 120, pisteraja_1 = 100)


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
valmis_paketti["Vastuuopettaja: Kimmo Vehkalahti & listan tekijä: --NIMESI-- "] <- " "
write.csv(valmis_paketti, file = "Arvosanat TILASTOMOOC.csv")

#######
## 4 ##
####### KIRJAA KÄSIN: näillä ei tunnistetietoja!

not_found_by_email_or_opiskelijanro_list <- anti_join(not_found_by_sahkoposti_list, mooc_data, by = "Opiskelijanumero")



