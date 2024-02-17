# InicioÑ librerías y funciones ####
library(tidyverse)
library(xlsx)
source("functions.R")

# Leer archivos y crear las finchas de administrador ####
administradores_Madrid <- read_file("Data_Input/ColegioAdministradoresFincaCortado.txt")
administradores_Madrid2 <- strsplit(administradores_Madrid, "</td></tr>")

# Bucle para crear el data frame ####

administradores <- NULL

for(a in 1:length(administradores_Madrid2[[1]])){
  # Nombre ####
  tryCatch({ 
  the_name <- str_extract(administradores_Madrid2[[1]][a], "Nombre.*")
  the_name <- sub("Nombre</strong>: ", "", the_name)
  the_name <- sub("<br>","", the_name)
  the_name <- no_data(the_name)
  }, error = function(e) NA) 
  
  # Dirección ####
  tryCatch({ 
  the_address <- str_extract(administradores_Madrid2[[1]][a], "Dirección.*")
  the_address <- sub("Dirección</strong>: ", "", the_address)
  the_address <- sub("<br>", "", the_address)
  the_address <- no_data(the_address)
  }, error = function(e) NA) 
  
  # Código Postal ####
  tryCatch({ 
  the_cp <- str_extract(administradores_Madrid2[[1]][a], "Código postal.*")
  the_cp <- sub("Código postal</strong>: ", "", the_cp)
  the_cp <- sub("<br>", "", the_cp)
  the_cp <- no_data(the_cp)
  }, error = function(e) NA) 
  
  # Ciudad ####
  tryCatch({ 
  the_city <- str_extract(administradores_Madrid2[[1]][a], "Población</strong>: .+?<br>")
  the_city <- sub("Población</strong>: ", "", the_city)
  the_city <- sub("<br>", "", the_city)
  the_city <- title_form(the_city)
  the_city <- no_data(the_city)
  }, error = function(e) NA) 
  
  # Provincia ####
  tryCatch({ 
  the_province <- str_extract(administradores_Madrid2[[1]][a], "Provincia</strong>: [[:alpha:]]*")
  the_province <- sub("Provincia</strong>: ", "", the_province)
  the_province <- no_data(the_province)
  }, error = function(e) NA) 
  
  # Teléfono ####
  tryCatch({ 
  the_telephone <- str_extract(administradores_Madrid2[[1]][a], "Teléfono</strong>: [[:digit:]]*")
  the_telephone <- sub("Teléfono</strong>: ", "", the_telephone)
  the_telephone <- no_data(the_telephone)
  }, error = function(e) NA) 

  # URL ####
  tryCatch({ 
  the_web <- str_extract(administradores_Madrid2[[1]][a], "href=\\\".*\"")
  the_web <- sub("href=\"", "", the_web)
  the_web <- sub("\\\"", "", the_web)
  the_web <- no_data(the_web)
  }, error = function(e) NA) 
  
  the_administrator <- data.frame(
    "Nombre" = the_name,
    "Direccion" = the_address,
    "CP" = the_cp,
    "Ciudad" = the_city,
    "Provincia" = the_province,
    "Telefono" = the_telephone,
    "Web" = the_web
  )
  
  administradores <- rbind(administradores, the_administrator)
}

# Pasar a excel ####
write.xlsx(
  administradores, 
  file = "Data_Output/Administradoras04.xlsx", 
  sheetName= "Administradores",
  col.names=TRUE, 
  row.names=FALSE,
  append=TRUE
)
