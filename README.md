# directory 
setwd("/Users/binta/Desktop/fichiers excel stage /fichiers modifiés")

install.packages("gridExtra")
install.packages("ggthemes")


# Chargement des packages ----

library(janitor)
library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)
library(writexl)
library(datawizard)
library(kableExtra)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)

# Importation des données ----
baci <- readRDS("BACI_HS22_Y2022_V202401b.rds")
hs_gtap <- read_xlsx("JobID-98_Concordance_H6_to_GP.xlsx")
country_codes <- read_csv("country_codes_V202401b.csv")
Concordance_hs_sitc3 <- read.csv("JobID-69_Concordance_HS_to_S3.CSV")

# Renommer et ordonner les colonnes ----

hs_gtap <- hs_gtap %>% rename(produit = HS.2022.Product.Code)
Concordance_hs_sitc3 <- Concordance_hs_sitc3 %>% rename(produit = HS...Combined..Product.Code)

# Jointure et nettoyage des données ----

baci_gtap_full <- full_join(baci, hs_gtap, by = "produit") %>%
  na.omit() %>%
  full_join(country_codes, by = c("exportation" = "country_code")) %>%
  arrange(exportation)

# Correspondance Imaclim sector ----

baci_gtap_full <- baci_gtap_full %>%
  mutate(imaclim_sector = case_when(
    GTAP.Product.Description %in% c("COA - Coal") ~ "Coal",
    GTAP.Product.Description %in% c("OIL - Oil") ~ "Oil",
    GTAP.Product.Description %in% c("GAS - Gas", "GDT - Gas manufacture, distribution") ~ "Gas",
    GTAP.Product.Description %in% c("P_C - Petroleum, coal products") ~ "Etransf", 
    GTAP.Product.Description %in% c("ELY - Electricity") ~ "Elec", 
    GTAP.Product.Description %in% c("CNS - Construction") ~ "BTP", 
    GTAP.Product.Description %in% c("isr", "trd", "WTR -  Water","CMN - Communication", "ofi", "obs", "ROS - Recreational and other services", "osg", "dwe") ~ "Inddemat",
    GTAP.Product.Description %in% c("ATP - Air transport") ~ "Airtransp", 
    GTAP.Product.Description %in% c("WTP - Water transport") ~ "Watertransp",
    GTAP.Product.Description %in% c("OTP - Transport nec") ~ "Othertransp", 
    GTAP.Product.Description %in% c("FRS - Forestry", "FSH - Fishing", "PDR - Paddy rice", "WHT - Wheat", "GRO - Cereal grains nec", "V_F - Vegetables, fruit, nuts", "OSD - Oil seeds", "C_B - Sugar cane, sugar beet",
                                    "PFB - Plant-based fibers", "OCR - Crops nec", "CTL - Bovine cattle, sheep and goats, horses", "OAP - Animal products nec", "RMK - Raw milk", "WOL - Wool, silk-worm cocoons", 
                                    "B_T - Beverages and tobacco products", "CMT - Bovine meat products", "OMT - Meat products nec", "VOL - Vegetable oils and fats", "MIL - Dairy products", "PCR - Processed rice",
                                    "SGR - Sugar", "OFD - Food products nec") ~ "Agriculture",
    GTAP.Product.Description %in% c("OXT - Other Extraction (formerly omn Minerals nec)", "LUM - Wood products", "PPP - Paper products, publishing", "CRP - Chemical,rubber,plastic products", "NMM - Mineral products nec", "I_S - Ferrous metals",
                                    "NFM - Metals nec", "OME - Machinery and equipment nec", "FMP - Metal products", "ELE - Computer, electronic and optical products", "TEX - Textiles", "WAP - Wearing apparel", "LEA - Leather products", "MVH - Motor vehicles and parts",
                                    "OMF - Manufactures nec","OTN - Transport equipment nec") ~ "Indmat",
  ))

## Correspondance avec imaclim_régions ----

baci_gtap_full$country_codes <- NA 
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("USA")] <- "USA"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Canada")] <- "CAN"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Brazil")] <- "BRA"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("China")] <- "CHN"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("India")] <- "IND"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Armenia", "Azerbaijan", "Belarus", "Georgia", "Kazakhstan", "Kyrgyzstan", "Rep. of Moldova", "Russian Federation",
                                                                 "Tajikistan", "Turkmenistan", "Uzbekistan", "Ukraine","Türkiye","USSR (...1990)")] <- "CIS"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Finland", "Aland Islands", "Albania", "Andorra", "Bosnia Herzegovina", "Faroe Islands", "Gibraltar", "Guernsey", "Holy See", 
                                                                 "Isle of Man", "Jersey", "Monaco", "Montenegro", "North Macedonia", "San Marino", "Serbia", "Austria", "Belgium", "Bulgaria", 
                                                                 "Switzerland", "Cyprus", "Czechia", "Germany", "Denmark", "Spain", "Estonia", "France", "Guadeloupe", "Martinique",
                                                                 "Réunion", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", 
                                                                 "Netherlands", "Norway", "Svalbard and Jan Mayen Islands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Sweden", 
                                                                 "Br. Indian Ocean Terr.", "Bouvet Island", "British Indian Ocean Territory", "French Southern Territories", "Saint Barthélemy","Czechoslovakia (...1992)","Fr. South Antarctic Terr.","Dem. Rep. of Germany (...1990)","Europe EFTA, nes","Serbia and Montenegro (...2005)")] <- "EUR"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Australia", "Cook Isds","Christmas Isds", "Cocos Isds", "Heard Island and McDonald Islands", "Norfolk Isds", "Japan", "Rep. of Korea", "New Zealand")] <- "JAN"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("United Arab Emirates", "Bahrain", "Iran", "Iraq", "Lebanon", "State of Palestine", "Syria", "Yemen", "Israel", "Jordan", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "Turks and Caicos Isds")] <- "MDE"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Angola", "Dem. Rep. of the Congo", "Burundi", "Comoros", "Djibouti", "Eritrea", "Mayotte (Overseas France)", "Seychelles", "Somalia", "Sudan", 
                                                                 "Benin", "Burkina Faso", "Botswana", "Central African Republic.", "Chad", "Congo", "Equatorial Guinea", "Gabon", 
                                                                 "Sao Tome and Principe", "Côte d'Ivoire", "Cameroon", "Cabo Verde", "Gambia", "Guinea-Bissau", "Liberia", "Mali", "Mauritania", 
                                                                 "Niger", "Saint Helena", "Sierra Leone", "Algeria", "Libya", "Western Sahara", "Egypt", "Ethiopia", "Ghana", "Guinea", "Kenya", 
                                                                 "Eswatini", "Lesotho", "Morocco", "Madagascar", "Mozambique", "Mauritius", "Malawi", "Namibia", "Nigeria", "Rwanda", "Senegal", 
                                                                 "Togo", "Tunisia", "United Rep. of Tanzania", "Uganda", "South Africa", "Zambia", "Zimbabwe", "South Sudan","Guinea-Bissau")] <- "AFR"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Afghanistan", "Bhutan", "Maldives", "American Samoa", "Cook Isds", "Fiji", "French Polynesia", "Guam", "Kiribati", 
                                                                 "Marshall Isds", "FS Micronesia", "Nauru", "New Caledonia", "Niue", "N. Mariana Isds", "Palau", 
                                                                 "Papua New Guinea", "Pitcairn", "Samoa", "Solomon Isds", "Tokelau", "Tonga", "Tuvalu", "United States Minor Outlying Islands", 
                                                                 "Vanuatu", "Wallis and Futuna Isds", "Bangladesh", "Brunei Darussalam", "China, Hong Kong SAR", "Indonesia", "Cambodia", 
                                                                 "Lao People's Dem. Rep.", "Sri Lanka", "Dem. People's Rep. of Korea", "China, Macao SAR", "Myanmar", "Timor-Leste", "Mongolia", 
                                                                 "Malaysia", "Nepal", "Pakistan", "Philippines", "Singapore", "Thailand", "China, Taiwan Province of", "Viet Nam","China, Hong Kong SAR","Other Asia, nes")] <- "RAS"
baci_gtap_full$country_codes [baci_gtap_full$country_name %in% c("Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Br. Virgin Isds", "Cayman Isds", "Cuba", "Dominica", 
                                                                 "Grenada", "Haiti", "Montserrat", "Netherlands Antilles (...2010)", "Curaçao", "Saint Maarten", "Bonaire", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                                                                 "Turks and Caicos Isds", "Virgin Isds, US", "Argentina", "Belize", "Bermuda", "Greenland", "Saint Pierre and Miquelon", 
                                                                 "Bolivia (Plurinational State of)", "Chile", "Colombia", "Costa Rica", "Dominican Republic.", "Ecuador", "Falkland Isds (Malvinas)", 
                                                                 "French Guiana", "Guyana", "South Georgia and South Sandwich Islands", "Suriname", "Guatemala", "Honduras", "Jamaica", 
                                                                 "Mexico", "Nicaragua", "Panama", "Peru", "Paraguay", "El Salvador", "Trinidad and Tobago", "Uruguay", "Venezuela")] <- "RAL"


### correspondance entre les codes pays et les codes régions -----

country_codes$regions_codes<- NA 

country_codes$regions_codes [country_codes$country_iso3 %in% c("USA")] <- "USA"
country_codes$regions_codes [country_codes$country_iso3 %in% c("CAN")] <- "CAN"
country_codes$regions_codes [country_codes$country_iso3 %in% c("BRA")] <- "BRA"
country_codes$regions_codes [country_codes$country_iso3 %in% c("CHN")] <- "CHN"
country_codes$regions_codes [country_codes$country_iso3 %in% c("IND")] <- "IND"
country_codes$regions_codes [country_codes$country_iso3 %in% c("ARM", "AZE", "BLR", "GEO", "KAZ", "KGZ", "MDA", "RUS",
                                                               "TJK", "TKM", "UZB", "UKR","TUR","SUN")] <- "CIS"
country_codes$regions_codes [country_codes$country_iso3 %in% c("FIN", "Aland Islands", "ALB", "AND", "BIH", "Faroe Islands", "GIB", "Guernsey", "Holy See", 
                                                               "Isle of Man", "Jersey", "Monaco", "MNE", "MKD", "SMR", "SRB", "AUT", "BEL", "BGR", 
                                                               "SHE", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FRA", "Guadeloupe", "Martinique",
                                                               "Réunion", "GBR", "GRC", "HRV", "HUN", "IRL", "ISL", "ITA", "LTU", "LUX", "LVA", "MLT", 
                                                               "NLD", "NOR", "Svalbard and Jan Mayen Islands", "POL", "PRT", "ROU", "SVK", "SVN", "SWE", 
                                                               "IOT", "BLM","CSK","ATF","DDR","ZA1","CHE","R20","SCG")] <- "EUR"
country_codes$regions_codes [country_codes$country_iso3 %in% c("AUS","COK", "CXR", "CCK", "Heard Island and McDonald Islands", "NFK", "JPN", "KOR", "NZL")] <- "JAN"
country_codes$regions_codes [country_codes$country_iso3 %in% c("ARE", "BHR", "IRN", "IRQ", "LBN", "PSE", "SYR", "YEM", "ISR", "JOR", "KWT", "OMN", "QAT", "SAU", "TCA")] <- "MDE"
country_codes$regions_codes [country_codes$country_iso3 %in% c("AGO", "COD", "BDI", "COM", "DJI", "ERI", "MYT", "SYC", "SOM", "SDN", 
                                                               "BEN", "BFA", "BWA", "CAF", "TCD", "COG", "GNQ", "GAB", 
                                                               "STP", "CIV", "CMR", "CPV", "Gambia", "GMB", "LBR", "MLI", "MRT", 
                                                               "NER", "SHN", "SLE", "DZA", "LBY", "Western Sahara", "EGY", "ETH", "GHA", "GIN", "KEN", 
                                                               "SWZ", "LSO", "MAR", "MDG", "MOZ", "MUS", "MWI", "NAM", "NGA", "RWA", "SEN", 
                                                               "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE", "SSD","GNB")] <- "AFR"
country_codes$regions_codes [country_codes$country_iso3 %in% c("AFG", "BTN", "MDV", "ASM", "CoK", "FJI", "PYF", "GUM", "KIR", 
                                                               "MHL", "FSM", "NRU", "NCL", "NIU", "MNP", "PLW", 
                                                               "PNG", "PCN", "WSM", "SLB", "TKL", "TON", "TUV", "United States Minor Outlying Islands", 
                                                               "VUT", "WLF", "BGD", "BRN", "China, Hong Kong SAR", "IDN", "KHM", 
                                                               "LAO", "LKA", "PRK", "MAC", "MMR", "TLS", "MNG", 
                                                               "MYS", "NPL", "PAK", "PHL", "SGP", "THA", "China, Taiwan Province of", "VNM","HKG","S19")] <- "RAS"
country_codes$regions_codes [country_codes$country_iso3 %in% c("AIA", "ATG", "ABW", "BHS", "BRB", "VGB", "CYM", "CUB", "DMA", 
                                                               "GRD", "HTI", "MSR", "ANT", "CUW", "SXM", "BES", "KNA", "LCA", "VCT", 
                                                               "TCA", "VGB", "ARG", "BLZ", "BMU", "GRL", "SPM", 
                                                               "BOL", "CHL", "COL", "CRI", "DOM", "ECU", "FLK", 
                                                               "French Guiana", "GUY", "South Georgia and South Sandwich Islands", "SUR", "GTM", "HND", "JAM", 
                                                               "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "TTO", "URY", "VEN")] <- "RAL"

regions_codes <- country_codes[order(country_codes$regions_codes), ]


### réimporter country_code pour l'adapter aux importations ----

regions_codes <- rename(regions_codes, importation = country_code)

baci_gtap_full <- full_join(baci_gtap_full, regions_codes, join_by("importation"))

baci_gtap_full <- arrange(baci_gtap_full, regions_codes,importation)

baci_gtap_full <- na.omit(baci_gtap_full)

##@ Fonction pour calculer le commerce international corrigé du commerce intra-zone ----

# Fonction pour le commerce intra-zone

calculate_intra_zone_trade <- function(data, region, sector) {
  sum(data$valeur_thousands_current_usd[
    data$imaclim_sector == sector &
      data$regions_codes == region &
      data$country_codes == region
  ], na.rm = TRUE)
}


calculate_all_intra_zone_trade <- function(data) {
  regions <- unique(data$regions_codes)
  sectors <- unique(data$imaclim_sector)
  
  results <- data.frame(
    Region = character(),
    Sector = character(),
    IntraZoneTrade = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (region in regions) {
    for (sector in sectors) {
      intra_zone_trade <- calculate_intra_zone_trade(data, region, sector)
      
      results <- rbind(results, data.frame(
        Region = region,
        Sector = sector,
        IntraZoneTrade = intra_zone_trade,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# Calcul du commerce intra-zone

intra_zone_trade <- calculate_all_intra_zone_trade(baci_gtap_full)

## Calcul des exportations et importations totales par région et par secteur ----

export_totals <- baci_gtap_full %>%
  group_by(country_codes, imaclim_sector) %>%
  summarise(TotalExports = sum(valeur_thousands_current_usd, na.rm = TRUE))

import_totals <- baci_gtap_full %>%
  group_by(regions_codes, imaclim_sector) %>%
  summarise(TotalImports = sum(valeur_thousands_current_usd, na.rm = TRUE))

# Fusion des résultats pour soustraire le commerce intra-zone des exportations totales et des importations totales

final_exports <- export_totals %>%
  full_join(intra_zone_trade, by = c("country_codes" = "Region", "imaclim_sector" = "Sector")) %>%
  mutate(AdjustedExports = TotalExports - IntraZoneTrade) %>%
  select(country_codes, imaclim_sector, AdjustedExports)

final_imports <- import_totals %>%
  full_join(intra_zone_trade, by = c("regions_codes" = "Region", "imaclim_sector" = "Sector")) %>%
  mutate(AdjustedImports = TotalImports - IntraZoneTrade) %>%
  select(regions_codes, imaclim_sector, AdjustedImports)

#### Création d'un tableau excel des exportations et des importations totales corrigées du commerce intra-zone par région et par secteur

pivot_table_import <- final_imports %>%
  pivot_wider(names_from = regions_codes, values_from = AdjustedImports) 

pivot_table_export <- final_exports %>% 
  pivot_wider(names_from = country_codes, values_from = AdjustedExports)

table_exports_imports <- left_join(pivot_table_export,pivot_table_import, join_by(imaclim_sector))

table_exports_imports <- rename(table_exports_imports, AFR.Exportation = AFR.x, AFR.Importation = AFR.y, BRA.Exportation=BRA.x, BRA.Importation=BRA.y,
                                CHN.Exportation=CHN.x,CHN.Importation=CHN.y,CAN.Exportation=CAN.x,CAN.Importation= CAN.y,CIS.Exportation=CIS.x,CIS.Importation=CIS.y,
                                EUR.Exportation=EUR.x,EUR.Importation=EUR.y,IND.Exportation=IND.x,IND.Importation=IND.y,JAN.Exportation=JAN.x,JAN.Importation=JAN.y,
                                MDE.Exportation=MDE.x,MDE.Importation=MDE.y,RAS.Exportation=RAS.x,RAS.Importation=RAS.y,RAL.Exportation=RAL.x,RAL.Importation=RAL.y,
                                USA.Exportation=USA.x,USA.Importation=USA.y)

table_exports_imports <- arrange(table_exports_imports)
write_xlsx(table_exports_imports,"table_exports_imports.xlsx")

## Calcul de la balance commerciale 
# Renommons le country_codes dans le tableau des exportations finales en regions_codes pour faciliter la fusion des deux tableaux

final_exports <- final_exports %>%
  rename(regions_codes = country_codes)

# Faire la somme des exportations et des importations de chaque région afin de calculer la balance commerciale ----

final_exports <-  final_exports %>%
  group_by(regions_codes) %>%
  summarise(total_exports = sum(AdjustedExports, na.rm = TRUE))

final_imports <-  final_imports %>%
  group_by(regions_codes) %>%
  summarise(total_imports = sum(AdjustedImports, na.rm = TRUE))

# Réaliser la jointure entre les deux dataframes

final_exports_imports <- left_join(final_exports,final_imports, join_by(regions_codes))

## Calcul de la balance commerciale ---- 

balance_commerciale <- final_exports_imports %>%
  mutate(Balance = total_exports - total_imports) %>%
  select(regions_codes, Balance)

# Afficher le résultat

print(balance_commerciale)

## Calcul du deséquilibre mondiale 

déséquilibre_mondial <- c(sum(balance_commerciale$Balance))

### Création des graphiques 

table_exports_imports_longer <- table_exports_imports %>%
  pivot_longer(cols = -imaclim_sector, names_to = "Region", values_to = "Value")

data_totals <- table_exports_imports_longer %>%
  group_by(imaclim_sector) %>%
  summarize(total_valeur = sum(Value, na.rm = TRUE)) %>% 
  mutate(pourcentage = (total_valeur * 100 ) / sum(total_valeur))

## Part du commerce selon le secteur

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", )), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


plot <- ggplot(data_totals, aes(x = imaclim_sector, y = pourcentage, fill = imaclim_sector))  +
  geom_bar(position = "identity" , stat = "identity", width = 0.75) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800","#FF6347","#fdb462","#fb9a99","#ef3b2c","#662506","#a6cee3","#386cb0","#984ea3", "#9ACD32", "#4B0082"))  + 
  labs(title = "Part du Commerce par Secteur",
       x = "Secteur Imaclim",
       y = "Part du commerce",
       fill = "Secteur_Imaclim",
       caption = "Source: Données BACI, 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

  
plot
  
grid.arrange(plot +  scale_colour_Publication() + 
                   theme_Publication(), nrow = 1 ) 

## Part du commerce selon les régions 

balance_commerciale_pourcentage <- balance_commerciale %>%
  group_by(regions_codes) %>% 
  summarize(balance_total = sum(abs(Balance), na.rm = TRUE)) %>% 
  mutate(pourcentage = ( balance_total * 100 ) / sum(balance_total)) 


plot_region <- ggplot(balance_commerciale_pourcentage, aes(x = regions_codes, y = pourcentage, fill = regions_codes)) +
  geom_bar(stat = "identity", position = "identity", width = 1) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Part du Commerce par région",
       x = "Régions",
       y = "Part du commerce",
       fill = "Région Imaclim", 
       caption = "Source: Données BACI, 2022") +
  theme_minimal()

grid.arrange(plot_region +  scale_colour_Publication() + 
               theme_Publication(), nrow = 1 ) 

## part du commerce par régions et par secteurs

table_exports_imports_longer <- table_exports_imports_longer %>%
  group_by(Region,imaclim_sector) %>% 
  summarize(value_total = sum(Value)) %>% 
  mutate(pourcentage = ( value_total * 100 ) / sum(value_total)) 


facet <- ggplot(table_exports_imports_longer, aes(x = imaclim_sector, y = pourcentage, fill = imaclim_sector)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Region, scales = "free_x", ncol=4) +
  labs(
    title = "Part du Commerce par Région et par Secteur",
    x = "Région",
    y = "Valeur en pourcentage",
    fill = "Secteur",
    caption = "Source: Données BACI, 2022"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 10 , face = "bold", hjust = 0.5),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 8),  
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold")  
  )

print(facet)

