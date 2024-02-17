library(rvest)
library(tidyverse)
library(xlsx)

# Initial Data ####
# Initial page 
initial_page <-  "https://www.paginasamarillas.es/a/administrador-de-fincas/madrid/madrid/"
initial_page_Getafe <- "https://www.paginasamarillas.es/search/administrador-de-fincas/all-ma/madrid/all-is/getafe/all-ba/all-pu/all-nc/1?what=Administrador+de+fincas&where=Getafe%2C+Madrid&qc=true"

# Model of interior page
# https://www.paginasamarillas.es/a/administrador-de-fincas/madrid/madrid/7

# Total number of pages. 
# Empirically found since no data was available in the initial page
total_number_pages <- 21

# Tags
# All the data
# class="listado-item item-pos advert-shadow"
# class='listado-item item-ip'
# class='listado-item item-ig'
# Name of the company
# <span itemprop="name">MBR Administradores</span>
# Web of the company
# <a class="web" target="_blank" href="http://www.mbr-administradores.es?utm_campaign=paginasamarillas&amp;utm_source=paginasamarillas&amp;utm_medium=referral" rel="noopener nofollow" itemprop="url" title="Se abre en una nueva ventana" data-omniclick="website"><span>Web</span></a>
# Address
# <span itemprop="streetAddress">Calle de Velázquez, 27 1º Izda.</span>
# Postal Code
# <span itemprop="postalCode">28001</span>
# City
# <span itemprop="addressLocality">Madrid</span>
# Telephone
# <span itemprop="telephone">914893071</span>

# Data output
dir.create("./Data_Output")

# Functions
# Scraper for text
scraper <- function(list, numerator, elementHTML){
  x <- html_nodes(list[[numerator]],elementHTML ) %>% 
    html_text()
  return(x)
}

# Replacement when there is no text
no_data <- function(nt){
  x <- ifelse(length(nt) == 0, "No Data", nt)
}

# Vector of pages ####
# Madrid
all_the_pages <- c(
  initial_page,
  paste0(initial_page, seq(from = 2, to = total_number_pages))
)

# Places with one page
all_the_pages <- initial_page_Getafe

# Collect the pages ####
realStateCompaniesInfo <- list()
for(p in 1:length(all_the_pages)){
  leida <- read_html(all_the_pages[p]) 
  realStateCompaniesInfo[[paste0("element", p)]] <- leida
}


#Get data
total_companies_data <- NULL
for(i in 1:length(realStateCompaniesInfo)){
  company_node_advertshadow <- html_nodes(realStateCompaniesInfo[[i]], "[class = 'listado-item item-pos advert-shadow']")
  company_node_ip <- html_nodes(realStateCompaniesInfo[[i]], "[class='listado-item item-ip']")
  company_node_ig <- html_nodes(realStateCompaniesInfo[[i]], "[class='listado-item item-ig']")
  super_lista <- c(company_node_advertshadow, company_node_ip, company_node_ig)
  companies_data <- NULL
  for(c in 1:length(super_lista)){
    
    tryCatch({
      #Name
      company_name<- scraper(super_lista, c, "span[itemprop='name']")
      company_name<- no_data(company_name)
    }, error = function(e) NA)  
  
    tryCatch({    
      #Address
      company_Address <- scraper(super_lista, c, "span[itemprop='streetAddress']")
      company_Address <- no_data(company_Address)
    }, error = function(e) NA)    
      
    tryCatch({  
      #Postal code
      company_postalCode <- scraper(super_lista, c, "span[itemprop='postalCode']")
      company_postalCode <- no_data(company_postalCode)
    }, error = function(e) NA)    
      
    tryCatch({  
      #City
      company_city <- scraper(super_lista, c, "span[itemprop='addressLocality']")
      company_city <- no_data(company_city)
    }, error = function(e) NA)    
      
    tryCatch({  
      #Telephone
      company_telephone <- scraper(super_lista, c, "span[itemprop='telephone']")
      company_telephone <- no_data(company_telephone)
    }, error = function(e) NA)    
      
    tryCatch({  
      #Web
      company_web <- html_nodes(super_lista[[c]],"a[class = 'web']") %>% 
        html_attr('href') %>%
        str_extract("[^\\?]+")
      company_web <- ifelse(length(company_web)==0, "No data", company_web)
    }, error = function(e) NA)  
    
      #Data Frame
      company_data <- data.frame(
        company_name,
        company_Address, 
        company_postalCode,
        company_city,
        company_telephone,
        company_web
      )
      
      #Data in element List
      companies_data <- rbind(companies_data, company_data)
  }
  
  #Bind each companies_data
  total_companies_data <- rbind(total_companies_data, companies_data)
}  

# Change postal code from numeric to character
total_companies_data$company_postalCode <- as.character(total_companies_data$company_postalCode)

# Join place by postal code
# postal_codes <- read_csv("Data_Input/listado-de-codigos-postales-de-espana.csv", locale=locale(encoding="latin1"))
# colnames(postal_codes) <- c("Province", "Place", "Postal_Code")
# postal_codes$Postal_Code <- as.character(postal_codes$Postal_Code)
# postal_codes$Postal_Code <- ifelse(nchar(postal_codes$Postal_Code) == 3,
#                                    paste0("00", postal_codes$Postal_Code),
#                                    ifelse(nchar(postal_codes$Postal_Code) == 4,
#                                           paste0("0",postal_codes$Postal_Code),
#                                           postal_codes$Postal_Code))
# 
# total_companies_data <- left_join(total_companies_data, 
#                                   postal_codes[, 2:3],
#                                   by = c("company_postalCode" = "Postal_Code"))
# 
# total_companies_data <- total_companies_data[,1:6]

# To excel
main_file <- write.xlsx(total_companies_data, file = "Data_Output/Administradoras04.xlsx", sheetName="Madrid", 
                        col.names=TRUE, row.names=TRUE, append=TRUE)
