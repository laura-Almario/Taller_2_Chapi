# Subconjunto de datos train
train <- subset(bd, !is.na(price))
train <- train %>% sample_n(size = 10286, replace = FALSE)
# Subconjunto de datos test
test <- subset(bd, is.na(price))
install.packages("stargazer")
library(stargazer)
reg <- lm(log(price) ~ surface_total + bedrooms + property_type + 
            property_type + Parking + Estudio + dist_bus_bogota + 
            dist_park_bogota,
          data=train)
stargazer(reg,type="text")
str(reg)

log_price_hat0 <- predict(reg)

train <- cbind(train, log_price_hat0)

test <- test %>% 
  mutate(price_hat0=exp(log_price_hat0)) %>% 
  mutate(price_hat0=round(price_hat0,-5))

head(test %>% st_drop_geometry())

submit<- test[, c('property_id','price_hat0')]
submit <- submit  %>% rename(price=price_hat0) %>% st_drop_geometry()
write.csv(submit,"regresion_lineal.csv",row.names = FALSE)
