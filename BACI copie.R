setwd("/Users/binta/Desktop/fichiers excel stage /fichiers modifiés")
setwd("C:/__Natalia")
library(janitor)
library(readxl)
library(stringr)
library(dplyr)
library(writexl)
library(tidyverse)

(.packages())
search()

### importation des données BACI

baci <- read.csv("BACI_HS22_Y2022_V202401b.csv")

summary(baci)

hs_gtap <- read.csv("JobID-58_Concordance_HS_to_GP.CSV")

product_code <- read.csv("product_codes_HS22_V202401b.csv")

colnames(hs_gtap)
colnames(product_code)
colnames(baci)

### renommer et ordonner les colonnes du BACI et du HS_GTAP

baci <- baci %>% dplyr::rename( annee = t , exportation = i , importation = j , produit = k , valeur_thousands_current_usd = v , quantité_metric_tons = q )

baci_order <- arrange(baci, produit)


baci_reordered <- baci_order %>%
  select(produit, everything())
         
hs_gtap <- hs_gtap %>% dplyr::rename(code= HS...Combined..Product.Code)
         
product_hs_gtap <- left_join(hs_gtap,product_code, by=("code"))

baci_reordered$produit

d_10121 <- baci_reordered[baci_reordered$produit == "10121",]
d_100610 <-baci_reordered[baci_reordered$produit == "100610",]
d_100620 <-baci_reordered[baci_reordered$produit == "100620",]

table_baci_reordered <- table(baci_reordered$produit)

dim(table_baci_reordered)

install.packages("kableExtra")
library(kableExtra)

install.packages("data.table")
library(data.table)

install.packages("datawizard")
library(datawizard)

data_rotate(baci_reordered)

baci_reordered_wide <- pivot_wider(data=baci_reordered, names_from = produit, values_from = valeur)


hs_gtap <- rename(hs_gtap, produit = HS...Combined..Product.Code)

baci_gtap_left <- left_join(baci_reordered, hs_gtap, join_by(produit))

baci_gtap_full <- full_join(baci_reordered, hs_gtap, join_by(produit))


### Regroupons les country_iso 3 en régions IMACLIM 



baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("USA")] <- "USA"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("CAN")] <- "CAN"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("BRA")] <- "BRA"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("CHN")] <- "CHN"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("IND")] <- "IND"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("ARM", "AZE", "BLR", "GEO", "KAZ", "KGZ", "MDA", "RUS",
                                                                "TJK", "TKM", "UZB", "UKR","TUR","SUN")] <- "CIS"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("FIN", "Aland Islands", "ALB", "AND", "BIH", "Faroe Islands", "GIB", "Guernsey", "Holy See", 
                                                                "Isle of Man", "Jersey", "Monaco", "MNE", "MKD", "SMR", "SRB", "AUT", "BEL", "BGR", 
                                                                "SHE", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FRA", "Guadeloupe", "Martinique",
                                                                "Réunion", "GBR", "GRC", "HRV", "HUN", "IRL", "ISL", "ITA", "LTU", "LUX", "LVA", "MLT", 
                                                                "NLD", "NOR", "Svalbard and Jan Mayen Islands", "POL", "PRT", "ROU", "SVK", "SVN", "SWE", 
                                                                "IOT", "BLM","CSK","ATF","DDR","ZA1","CHE","R20","SCG")] <- "EUR"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("AUS", "COK","CXR", "CCK", "Heard Island and McDonald Islands", "NFK", "JPN", "KOR", "NZL")] <- "JAN"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("ARE", "BHR", "IRN", "IRQ", "LBN", "PSE", "SYR", "YEM", "ISR", "JOR", "KWT", "OMN", "QAT", "SAU", "TCA")] <- "MDE"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("AGO", "COD", "BDI", "COM", "DJI", "ERI", "MYT", "SYC", "SOM", "SDN", 
                                                                "BEN", "BFA", "BWA", "CAF", "TCD", "COG", "GNQ", "GAB", 
                                                                "STP", "CIV", "CMR", "CPV", "Gambia", "GMB", "LBR", "MLI", "MRT", 
                                                                "NER", "SHN", "SLE", "DZA", "LBY", "Western Sahara", "EGY", "ETH", "GHA", "GIN", "KEN", 
                                                                "SWZ", "LSO", "MAR", "MDG", "MOZ", "MUS", "MWI", "NAM", "NGA", "RWA", "SEN", 
                                                                "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE", "SSD", "GNB")] <- "AFR"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("AFG", "BTN", "MDV", "ASM", "CoK", "FJI", "PYF", "GUM", "KIR", 
                                                                "MHL", "FSM", "NRU", "NCL", "NIU", "MNP", "PLW", 
                                                                "PNG", "PCN", "WSM", "SLB", "TKL", "TON", "TUV", "United States Minor Outlying Islands", 
                                                                "VUT", "WLF", "BGD", "BRN", "China, Hong Kong SAR", "IDN", "KHM", 
                                                                "LAO", "LKA", "PRK", "MAC", "MMR", "TLS", "MNG", 
                                                                "MYS", "NPL", "PAK", "PHL", "SGP", "THA", "China, Taiwan Province of","VNM","HKG","S19")] <- "RAS"
baci_gtap_left$country_iso3 [baci_gtap_left$country_iso3 %in% c("AIA", "ATG", "ABW", "BHS", "BRB", "VGB", "CYM", "CUB", "DMA", 
                                                                "GRD", "HTI", "MSR", "ANT", "CUW", "SXM", "BES", "KNA", "LCA", "VCT", 
                                                                "TCA", "VGB", "ARG", "BLZ", "BMU", "GRL", "SPM", 
                                                                "BOL", "CHL", "COL", "CRI", "DOM", "ECU", "FLK", 
                                                                "French Guiana", "GUY", "South Georgia and South Sandwich Islands", "SUR", "GTM", "HND", "JAM", 
                                                                "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "TTO", "URY", "VEN")] <- "RAL"



BC_CIS + BC_EUR + BC_JAN +BC_CHN+ BC_USA+BC_IND+BC_MDE+BC_AFR+BC_BRA+BC_CAN+BC_RAS+BC_RAL




+sum(baci_gtap_left$valeur_thousands_current_usd)

sum(baci_gtap_left_same_regions$valeur_thousands_current_usd)















