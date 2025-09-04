---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = file.path("man", "figures", "README-"),
  out.width = "100%"
)
```

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/inbolims.svg)](https://github.com/inbo/inbolims/releases)
![GitHub](https://img.shields.io/github/license/inbo/inbolims)
[![R build status](https://github.com/inbo/inbolims/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/inbolims/actions)
![r-universe name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/inbolims)
[![Codecov test coverage](https://codecov.io/gh/inbo/inbolims/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/inbolims?branch=main)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/inbo/inbolims.svg)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/inbolims.svg)
<!-- badges: end -->


# Beschrijving

R scripts om de in LIMS bewaarde gegevens van het INBO op te halen. Dit werkt enkel binnen het netwerk van het instituut of indien verbonden via vpn met het netwerk.

Voorlopig zijn er 3 hoofdbrokken in dit pakket:

- Inlezen van rapportagegegevens van het labo uit het LIMS datawarehouse
- parsen textuurmetingen uit de output van het laserdiffractietoestel 
- Kwaliteitskenmerken digital droplet PCR voor het genetisch labo

Daarnaast zijn er ook nog enkele andere voorzieningen toegevoegd zoals inlezen van textuurdata en verwerken van gegevens.

# Gegevens uit het LIMS Datawarehouse halen

## Eerste gebruik


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

library(tidyverse)
library(inbolims)

#maak connectie met databank
connection <- lims_connect()

````

Je kan op voorhand al de staalinformatie ophalen via lims_sample_information.
Je kan je project selecteren. Dit mogen er ook meerdere zijn en eventueel joker
karakters mogen ook gebruikt worden.

Standaard worden defaultvelden getoond, maar je kan zelf ook kiezen welke velden je wil zien eventueel gecombineerd met een template, met de variabele fields.

Via de functie `sample_fields_from_template` kan je te weten komen welke templates beschikbaar zijn en welke velden onderdeem uitmaken in elk van de templates. Met de functie `lims_table_fields` kan je voor elke tabel zien welke velden het datawarehouse in aanbieding heeft.

````
#illustratieve stap: krijg de staalinformatie (optioneel)
staal_info <- lims_sample_information(connection, project = c("I-19W001-02"))

````

Als je wil een overzicht hebben over alle testen en componenten die in de betrokken projecten gebruikt (gerapporteerd en niet-gerapporteerd) zijn kan je onderstaande functie gebruiken.

Als je in je rapport enkele niet-gerapporteerde testen wil meerapporteren kan je dit op basis van deze output, waarbij je de LimsAnalysisName en Component aan elkaar plakt met een dubbele underscore 

````
#illustratieve stap: krijg de testinformatie (optioneel)
test_info <- lims_test_information(connection, project = c("I-19W001-02"))

````




De belangrijkste functie is het inlezen van alle rapportdata.
In de toekomst zal die functie ook verbeterd worden zodat ook gemakkelijk kan achterhaald worden wat er historisch allemaal bestond van analyse- en componentnamen.

````
#haal de rapportgegevens binnen uit de databank
rapport_data <- read_lims_data(connection = connection, 
                               project = c("I-19W001-02"), 
                               sql_template = "default", 
                               show_query = FALSE)
                                 
```` 
Bovenstaande code haalt de data op voor het project I-19W001-02 (project = "I-19W001-02") volgens de standaardtemplate  (template = "default") en de query die gebruikt is om het datawarehouse te bevragen wordt getoond (show_query = TRUE). Als eerste variabele moet de databank connectie opgegeven worden, zodat de routine weet waar de gegevens uit gehaald moeten worden.

Je kan ook alle of enkele niet-gerapporteerde componenten ophalen op basis van de informatie die je kan halen uit `lims_test_information`

````
include_nonrep <- c("IC_KATIONEN__Verdunning", "IC_ANIONEN__Verdunning")
rapport_data <- read_lims_data(connection = connection, 
                               project = c("I-19W001-02"),
                               include_non_reportable = include_nonrep)

````

## Afgeleide routines (work in progress)


### Staaloverzicht

Om een overzicht van de stalen te hebben van de ingelezen rapport_data.

````
staaloverzicht <- lims_report_samples(rapport_data)
view(staaloverzicht)

```` 
Op dit moment vereist bovenstaande routine nog heel wat velden, daarom dat dit enkel gegarandeerd is te werken met de *default* `sql_template`.

### Kruistabel

Het formaat van `rapport_data` is in een lang dataformaat, waarbij ieder resultaat op een andere regel staat. Vaak is eerder gewenst de resultaten van een staal naast elkaar te zien:

````
kruistabel <- lims_report_xtab(rapport_data)
View(kruistabel)

```` 

### Exporteren

Bovenstaande kruistabel kan je gemakkelijk naar een door excel leesbaar csv bestand converteren:

````
lims_report_export(kruistabel, path = "test_xtab.csv")
lims_report_export(rapport_data, path = "test.csv")

```` 
Ook hier op dit moment enkel gegarandeerd voor de "default" `sql_template` gebruikt bij `read_lims_data`.

# Textuur

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
system2("mkdir", target_dir)

#parse de file naar een geldige R dataset
textuur_parsed <- parse_texture_content(filename, delim = "\t")

#interpreteer de dataset tot een inhoudelijk bruikbaar formaat
textuur_interpreted <- interpret_texture_content(textuur_parsed)

#maak een connectie met het LIMS datawarehouse
conn <- lims_connect() #connect to dwh
textuur_linked <- link_labo_id(conn, textuur_interpreted)

#schrijf de files weg
write_texture_files(target_dir, textuur_linked)

````
# digital droplet PCR

Zal aangevuld worden eens er vraag naar is vanuit het genetisch labo.
