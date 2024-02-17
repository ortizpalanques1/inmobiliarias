library(rvest)
library(tidyverse)
library(xlsx)
source("functions.R")
# Initial Data ####
initial_data <- read.csv(file = "Data_Input/municipios_buscados.csv", header=TRUE)

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
#dir.create("./Data_Output")

# All the councils
councils_total <- NULL
for(m in 1:nrow(initial_data)){

  # Vector of pages ####
  # Madrid
  all_the_pages <- if(initial_data$hojas[m] == 0){
    initial_data$Web[m]
  }else{
    c(
      initial_data$Web[m],
      paste0(initial_data$Web[m], seq(from = 2, to = initial_data$hojas[m]))
    )
  }
  
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
  councils_total <- rbind(councils_total, total_companies_data)
}  

# Keep unique names
councils_total_unique <- councils_total[!duplicated(councils_total$company_name),]

# Create a compatible file
final_data <- data.frame(
  "Nombre" = councils_total_unique$company_name,
  "Dirección" = councils_total_unique$company_Address,
  "CP" = councils_total_unique$company_postalCode,
  "Ciudad" = councils_total_unique$company_city,
  "Provincia" = "Madrid",
  "Comunidad" = "Madrid",
  "Tel01" = councils_total_unique$company_telephone,
  "Tel02" = "",
  "Email01" = "",
  "Email02" = "",
  "company_web" = councils_total_unique$company_web
)


# To excel
write.xlsx(
  final_data, 
  file = "Data_Output/Administradoras04.xlsx", 
  sheetName= "Municipios",
  col.names=TRUE, 
  row.names=FALSE,
  append=TRUE
)
