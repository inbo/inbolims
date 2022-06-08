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

## Rapporteren op  basis van het LIMS datawarehouse

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

De export van de textuur voor 30 stalen van het laserdiffractietoestel kan via dit pakket omgezet worden naar allemaal losse bestanden met de correcte naam en een bruikbare inhoud (zie [hier](inst/textuurbatch/textuur.Rmd))

