##Estadístias Descriptivas

#Separamos nuestras bases en train y test

# Subconjunto de datos train
train <- subset(bd, !is.na(price))

# Subconjunto de datos test
test <- subset(bd, is.na(price))

train_s<- train
test_s <- test

#Se crean las estadisticas descriptivas:

dim(train_s)
colnames(train_s)
summary(train_s)

install.packages("gtsummary")
library(gtsummary)

tbl_summary (train_s, 
             statistic = list(all_continuous() ~ "{mean} ({sd})"),
             type = list(c(geometry) ~ "categorical")
             
#Exportamos las tablas:

install.packages("pastecs")
library(pastecs)
install.packages("writexl")
library(writexl)

stat.desc(train_s)
descriptivas_train <- stat.desc(train_s)
descriptivas_train$Estadisticas <- row.names(descriptivas_train) 
descriptivas_train <- descriptivas_train %>% select(Estadisticas, everything())
write_xlsx(descriptivas_train, "descrip_train_s.xlsx")

# Gráficos: 

#Precio de venta: 

install.packages("plotly")
library(plotly)

precio <- ggplot(train_s, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta", y = "Cantidad de unmuebles") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(precio)

#Distancias con parques:

parques <- ggplot(train_s, aes(x = dist_park_bogota, y = price)) +
  geom_point(col = "lightgreen", alpha = 0.4) +
  labs(x = "Distancia", 
       y = "Valor venta inmueble",
       title = "Parques y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(parques) 

#Distancias con restaurantes:

restaurantes <- ggplot(train_s, aes(x = dist_rest_bogota, y = price)) +
  geom_point(col = "orange", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Restaurantes y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(restaurantes) 

#Distancias con bancos:

banco <- ggplot(train_s, aes(x = dist_banco_bogota, y = price)) +
  geom_point(col = "purple", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Bancos y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(banco) 

#Distancias con bares:

bares <- ggplot(train_s, aes(x = dist_bar_bogota, y = price)) +
  geom_point(col = "pink", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Bares y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(bares)

#Distancias con CAI:

seguridad <- ggplot(train_s, aes(x = dist_seguridad_bogota, y = price)) +
  geom_point(col = "magenta", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "CAI y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(seguridad)

#Distancias con estaciones de bus:

bus <- ggplot(train_s, aes(x = dist_bus_bogota, y = price)) +
  geom_point(col = "grey", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Estaciones de bus y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(bus) 

# Estadisticas descriptivas para entrenamiento

#Se crean las estadisticas descriptivas:

dim(test_s)
colnames(test_s)
summary(test_s)

tbl_summary (test_s, 
             statistic = list(all_continuous() ~ "{mean} ({sd})"),
             type = list(c(geometry) ~ "categorical")

#Se exportan las tablas:

stat.desc(test_s)
descriptivas_test <- stat.desc(test_s)
descriptivas_test$Estadisticas <- row.names(descriptivas_test) 
descriptivas_test <- descriptivas_test %>% select(Estadisticas, everything())
write_xlsx(descriptivas_test, "descrip_test_s.xlsx")