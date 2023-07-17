# Subconjunto de datos train
train <- subset(bd, !is.na(price))

# Subconjunto de datos test
test <- subset(bd, is.na(price))

#Random Forests
p_load("ranger")
p_load("caret")
library(caret)
set.seed(123)
fitControl<-trainControl(method ="cv",
                         number=5)

tree_ranger <- train(
  log(price) ~ surface_total  + bedrooms + bathrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota,
  data= train,
  method = "ranger",
  trControl = fitControl,
  metric = 'RMSE',
  tuneGrid=expand.grid(
    mtry = 1,
    splitrule = "variance",
    min.node.size = 5)
)

tree_ranger 

set.seed(123)

tree_ranger_grid <- train(
  log(price) ~ surface_total  + bedrooms + bathrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota,
  data= train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = c(1,2,3),
    splitrule = "variance",
    min.node.size = c(5,10,15))
)

tree_ranger_grid

tree_ranger_final <- train(
  log(price) ~ surface_total  + bedrooms + bathrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota,
  data= train,
  method = "ranger",
  trControl = fitControl,
  metric = 'RMSE',
  tuneGrid=expand.grid(
    mtry = 3,
    splitrule = "variance",
    min.node.size = 5)
)

tree_final <- train(
  log(price) ~surface_total + bedrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota,
  data = train,
  method = "ranger",
  trControl = fitControl,
  metric = 'RMSE',
  tuneLength=5)

head(tree_final)
test$pred_tree<-predict(tree_final, newdata = test)
head(test)

test_data <- test   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data)

submit1<- test_data[, c('property_id','pred_tree')]
submit1<- submit1  %>% rename(price=pred_tree)
write.csv(submit1,"rf_tree_v1.csv",row.names = FALSE)

#modelo de arbol con las variables de random forest (prueba)
install.packages('rpart')
library(rpart)
control_params <- rpart.control(mtry = 3, splitrule = "variance", minsplit = 5)
tree_model <- rpart(log(price) ~surface_total + bedrooms + property_type + 
                      property_type + Parking + Estudio + dist_bus_bogota + 
                      dist_park_bogota, data = train, control = control_params)

head(tree_model)
test$pred_tree<-predict(tree_model, newdata = test)
head(test)

test_data2 <- test  %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data2)

submit2<- test_data2[, c('property_id','pred_tree')]
submit2<- submit2  %>% rename(price=pred_tree)
write.csv(submit2,"rf_tree_v2.csv",row.names = FALSE)