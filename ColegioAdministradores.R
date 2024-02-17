# InicioÑ librerías y funciones ####
library(tidyverse)
library(xlsx)
source("functions.R")

# Leer archivos y crear las finchas de administrador ####
administradores_Madrid <- read_file("Data_Input/ColegioAdministradoresFincaCortado.txt")
administradores_Madrid2 <- strsplit(administradores_Madrid, "</td></tr>")
municipios_Madrid <- sort(unlist(strsplit(read_file("Data_Input/MadridMunicipios.txt"), ", ")))

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

# Script para lograr homologar ciudades ####

only_unique_cities <- sort(unique(administradores$Ciudad))

# Quitar espacios en blanco
administradores$Ciudad <- trimws(administradores$Ciudad)

# Corregir manualmente algunos errores
administradores$Ciudad <- ifelse(administradores$Ciudad == "<strong>provincia</strong>: Madrid<br>", "Madrid", administradores$Ciudad)

# Cambiar el código postal 28038 por "Madrid"
cp_and_no_city <- which(!is.na(as.numeric(only_unique_cities)))
administradores$Ciudad <- ifelse(administradores$Ciudad == only_unique_cities[cp_and_no_city], "Madrid", administradores$Ciudad)

# Quitar paréntesis
parentesis <- grep("\\(", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- ifelse(grepl("\\(", administradores$Ciudad) == TRUE,
                                 sub(" \\(.+", "", administradores$Ciudad),
                                 administradores$Ciudad
                                 )

# Quitar guiones (solo se mantiene la primera ciudad)
guiones <- grep("-", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- ifelse(grepl("-", administradores$Ciudad) == TRUE,
                                 sub(" -.+", "", administradores$Ciudad),
                                 administradores$Ciudad
                                )
# Cambiar "S." por "San"
S_punto <- grep("S\\.", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- sub("S\\.", "San ", administradores$Ciudad)

# Cambiar "s." por "Sebastián"
administradores$Ciudad <- sub("s\\.", "Sebastián", administradores$Ciudad)

# Cambiar "L." por "Lorenzo"
administradores$Ciudad <- sub("L\\.", "Lorenzo", administradores$Ciudad)

# Quitar dos ciudades unidas por " y " o " Y "
y_union <- grep(" y | Y ", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- sub(" Y .+", "", administradores$Ciudad)

# Arreglar los Fernandos
fernandos <- grep(" Fdo\\. | Ferndº | Fdo\\.", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- sub(" Fdo\\. | Ferndº | Fdo\\.", " Fernando ", administradores$Ciudad)

# Cambiar "Las Rozas" por "Las Rozas De Madrid"
las_rozas <- grep("Las Rozas$", administradores$Ciudad, value = FALSE)
administradores$Ciudad <- sub("Las Rozas$", "Las Rozas De Madrid", administradores$Ciudad)


# Errores comunes
errores <- c("Alcala","Alcorcon", "Alarcon", "Agustin", "Avila", "Chinchon", "Alamo", "^El Escorial", "Griñon", "Humanes", "Leganes", "Mostoles", "Pozuelo$", "Agust\\.", "Martin", "Sebastian", "Torrejon", "Odon")
correctos <- c("Alcalá","Alcorcón", "Alarcón","Agustín", "Ávila", "Chinchón", "Álamo", "San Lorenzo De El Escorial", "Griñón", "Humanes De Madrid", "Leganés", "Móstoles", "Pozuelo De Alarcón", "Agustín ", "Martín", "Sebastián", "Torrejón", "Odón")
tabla_de_correcciones <- data.frame(errores, correctos)
for(e in 1:length(administradores$Ciudad)){
  for(c in 1:length(tabla_de_correcciones$errores)){
    administradores$Ciudad[e] <- ifelse(
      grepl(tabla_de_correcciones$errores[c], administradores$Ciudad[e]),
      sub(tabla_de_correcciones$errores[c], tabla_de_correcciones$correctos[c], administradores$Ciudad[e]),
      administradores$Ciudad[e])
  }
}

# Un espacio
administradores$Ciudad <- gsub("  ", " ", administradores$Ciudad)

# Repetir title_form
administradores$Ciudad <- sapply(administradores$Ciudad, title_form)

# Repetir y revisar
only_unique_cities_after <- sort(unique(administradores$Ciudad))

# Asignar municipio de la lista oficial municipios_Madrid
# municipio_final <- NULL
# for(m in 1:length(only_unique_cities_after)){
#   for(d in 1:length(municipios_Madrid)){
#     if(agrepl(only_unique_cities_after[m], municipios_Madrid[d], max.distance = 5, costs = 4)){
#       municipio_final[m] <- municipios_Madrid[d]
#       break
#     }
#   }
# }

municipios_join_table <- data.frame(
  "Ciudad" = only_unique_cities_after,
  "Municipio" = municipio_final
)


# Pasar a excel ####
write.xlsx(
  administradores, 
  file = "Data_Output/Administradoras04.xlsx", 
  sheetName= "Administradores",
  col.names=TRUE, 
  row.names=FALSE,
  append=TRUE
)
