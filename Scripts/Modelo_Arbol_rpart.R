# Subconjunto de datos train
train <- subset(bd, !is.na(price))

# Subconjunto de datos test
test <- subset(bd, is.na(price))

train_s<- train
test_s <- test

#Modelo de arboles
install.packages("caret")
library(caret)
install.packages("sf")
library(sf)
test <-test_s  %>% 
  mutate(sample="test")
train<-train_s  %>% 
  mutate(sample="train")

train_reducido <- train %>% sample_n(size = 10286, replace = FALSE)

if (anyNA(test)) {}

fitControl<-trainControl(method ="cv", number=5)

set.seed(2023)
tree <- train(
  log(price) ~surface_total + bedrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota,
  data = train_reducido,
  method = "rpart",
  trControl = fitControl,
  tuneLength=5
)


#test <- bd %>% filter(sample = "test")
test$pred_tree<-predict(tree, test)

head(test)

test_data <- test   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data)

submit<- test_data[, c('property_id','pred_tree')]
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)
