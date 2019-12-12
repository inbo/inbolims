install.packages("remotes") #eenmalig (pakket nodig om install_github te gebruiken)
remotes::install_github("inbo/inbolims/inbolims") #haal het package op uit github

### laadt de bibliotheek
library(inbolims)

### maak connectie met het datawarehouse
conn <- limsdwh_connect() #Werkt enkel op INBO netwerk, of via VPN

#### restultaatdata ophalen uit het lims datawarehouse
my_data <- limsdwh_report(conn, project = "I-19W005-01") 
# kies project, indien meerdere projecten kan je '%' als Joker teken gebruiken
my_data <- limsdwh_report(conn, project = "I-19W005-%") 

### staalinformatie uit de opgehaalde resultaatdata halen
my_staalinfo <- report_sample(my_data)

### schrijf resultaten weg
write.csv2(my_data,"C:/RAP_CSVdata.csv")
write.csv2(my_staalinfo,"C:/RAP_infostaal.csv")


### kruistabel maken van de resultaatdata
# werkt voorlopig (op 12/12/19) enkel wanneer er geen testen gecanceld werden in het labo
# foutmelding die je dan krijgt: 
#Error in report_xtab(my_data) : 
#  Verschillende waarden voor een analyse van een labostaal zou niet mogen voorkomen. Contacteer de LIMS verantwoordelijke
#Let op! indien je een foutmelding krijgt wordt het object niet bijgewerkt
my_xtab <- report_xtab(my_data)

#werkt enkel wanneer geen foutmelding bij my_xtab <- report_xtab(my_data)
write.csv2(my_xtab,"C:/RAP_XTABdata.csv")
