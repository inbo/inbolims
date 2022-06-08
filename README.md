# inbolims

## Beschrijving

R scripts om de in LIMS bewaarde gegevens van het INBO op te halen. Dit werkt enkel binnen het netwerk van het instituut of indien verbonden via vpn met het netwerk.

## Hoe gebruiken

### Eerste gebruik


Zorg dat het pakket remotes of een equivalent pakket geïnstalleerd is

````
install.packages("remotes")

````

Indien dit in orde is kan je de bibliotheek installeren met volgend commando

````
remotes::install_github("inbo/inbolims")

````

Iedere keer R opnieuw opgestart wordt zal je de inbolims library moeten laden

````
library(inbolims)

```` 

De belangrijkste functionaliteit in het script is de connectie met de databank en het ophalen van gegevens van een project. Dit doe je als volgt:

```` 

library(inbolims)
library(tidyverse)

rapport_data <- lims_report_data(project = c("I-19W001-02"), 
                                 template = "default",
                                 show_query = TRUE)

```` 
Bovenstaande code haalt de data op voor het project I-19W001-02 (project = "I-19W001-02") volgens de standaardtemplate  (template = "default") en de query die gebruikt is om het datawarehouse te bevragen wordt getoond (show_query = TRUE)

Om een overzicht van de stalen te hebben van de ingelezen rapport_data

````

staaloverzicht <- lims_report_samples(rapport_data)

```` 

Het formaat van rapport_data is in een lang dataformaat, waarbij ieder resultaat op een andere regel staat. Vaak is eerder gewenst de resultaten van een staal naast elkaar te zien:

````
kruistabel <- lims_report_xtab(rapport_data)

```` 

Bovenstaande kruistabel kan je gemakkelijk naar een door excel leesbaar csv bestand converteren:

````
lims_report_export(kruistabel, path = "test.csv")

```` 

## Textuur

De export van de textuur voor 30 stalen van het laserdiffractietoestel kan via dit pakket omgezet worden naar allemaal losse bestanden met de correcte naam en een bruikbare inhoud.

````
getwd() #gewoon om te tonen in welke werkdirectory je zit

library(tidyverse) #package met veel datafunctionaliteit
library(inbolims) #package die de verwerking van de tekstuurfiles regelt
library(DBI) #package voor DB communicatie

#definieer het path naar de resultatenfile
filename <- system.file("extdata", "textuur_export_voorbeeld.txt", package = "inbolims")

#definieer de directory voor de geparste bestandjes
target_dir <- "tijdelijk"

#parse de file naar een geldige R dataset
textuur_parsed <- parse_texture_content(file, delim = "\t")

#interpreteer de dataset tot een inhoudelijk bruikbaar formaat
textuur_interpreted <- interprate_texture_content(textuur_parsed)

#maak een connectie met het LIMS datawarehouse
conn <- lims_connect() #connect to dwh
textuur_linked <- link_labo_id(conn, textuur_interpreted)

#schrijf de files weg
write_texture_files(target_dir, textuur_linked)

````

