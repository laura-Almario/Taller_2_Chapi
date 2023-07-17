##Corregimos la base y quitamos los NA

# Verificamos las variables con NA
names(bd)[sapply(bd, function(x) any(is.na(x)))]

# Porcentaje
na_pct <- sapply(bd, function(x) mean(is.na(x)))*100
result <- data.frame(variable = names(bd), na_pct = na_pct)
result[result$na_pct > 0,]

# Cantidad
na_count <- sapply(bd, function(x) sum(is.na(x)))
result <- data.frame(variable = names(bd), na_count = na_count)
result[result$na_count > 0,]

#BAÑOS

# Calcular el promedio de baños para apartamentos y casas
mean_bath_apto <- round(mean(bd$bathrooms[bd$property_type == "Apartamento"], na.rm = TRUE))
mean_bath_casa <- round(mean(bd$bathrooms[bd$property_type == "Casa"], na.rm = TRUE))

# Reemplazar los NA en función de property_type
bd$bathrooms <- ifelse(
  is.na(bd$bathrooms) & bd$property_type == "Apartamento",
  mean_bath_apto,
  ifelse(
    is.na(bd$bathrooms) & bd$property_type == "Casa",
    mean_bath_casa,
    bd$bathrooms
  )
)

# Redondear la variable bathrooms a números enteros
bd$bathrooms <- round(bd$bathrooms)

# Verificar que se hayan reemplazado los NA correctamente
sum(is.na(bd$bathrooms))

# Surface_total

install.packages("stringr")
library(stringr)

# Extraemos información de la descripción sobre el área
surface_str <- str_extract(bd$description, "(?i)(\\d+)\\s*(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b|(?i)\\b(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b\\s*(\\d+)")

# Convertirlo en valores numéricos
total_surface <- as.numeric(gsub("(?i)\\b(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b|\\s", "", surface_str, ignore.case = TRUE))

# Remplazamos los NAs in 'total_surface' con los datos extraídos
bd$surface_total<- ifelse(is.na(bd$surface_total), total_surface, bd$surface_total)

sum(is.na(bd$surface_total))

#Se remplazan los NA faltantes por el área cubierta
bd$surface_total <- ifelse(is.na(bd$surface_total), bd$surface_covered, bd$surface_total)

# Calculamos el promedio del área total por el número de habitaciones
mean_surface_total <- ave(bd$surface_total, bd$bedrooms, FUN = function(x) mean(x, na.rm = TRUE))

# Reemplazar los NA con el promedio correspondiente
bd$surface_total <- ifelse(is.na(bd$surface_total), mean_surface_total[match(bd$bedrooms, unique(bd$bedrooms))], bd$surface_total)

sum(is.na(bd$surface_total))

saveRDS(bd,"bd_NA.rds")