# Subconjunto de datos train
train <- subset(bd, !is.na(price))

# Subconjunto de datos test
test <- subset(bd, is.na(price))

#Random Forests
library(ranger)
library(caret)
set.seed(123)
fitControl<-trainControl(method ="boot",
                         number=5)

set.seed(123)

tree_ranger <- train(
  log(price) ~ surface_total + bedrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota + dist_rest_bogota + dist_seguridad_bogota,
  data= train,
  method = "ranger",
  trControl = fitControl,
  metric = 'RMSE',
  tuneGrid=expand.grid(
    mtry = c(1,2,3),
    splitrule = "variance",
    min.node.size = c(5,10,15))
)

head(tree_ranger) 

tree_final <- train(
  log(price) ~ surface_total + bedrooms + property_type + 
    property_type + Parking + Estudio + dist_bus_bogota + 
    dist_park_bogota + dist_rest_bogota + dist_seguridad_bogota,
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

submit3<- test_data[, c('property_id','pred_tree')]
submit3<- submit3  %>% rename(price=pred_tree)
write.csv(submit3,"rf_tree_v3.csv",row.names = FALSE)