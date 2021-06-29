library(caret)
#overall accuracy can be a deceptive measure
table(predicted = y_hat, actual = test_set$sex)

# If we compute the accuracy separately for each sex, we get:
test_set%>%
  mutate(y_hat = y_hat)%>%
  group_by(sex)%>%
  summarize(accuracy= mean(y_hat == sex))

#because the prevalence of males in this dataset is high
prev<- mean(y == "Male")
prev #0.7733333

#compute confusion matrix
#install.packages("e1071")
library(e1071)

cm<-confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Balanced accuracy and F1 score
cutoff<- seq(61, 70)
F_1<- map_dbl(cutoff, function(x){
  y_hat<- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

#As before, we can plot these F1 measures versus the cutoffs:
data.frame(cutoff, F_1)%>%
  ggplot(aes(cutoff, F_1))+
  geom_point()+
  geom_line()

max(F_1)
#This maximum is achieved when we use the following cutoff:
best_cutoff<- cutoff[which.max(F_1)]# it requires breckate which was not mentioned before
best_cutoff

############################################
#SOMETHING WRONG ABOVE-CLEARED <- best_cutoff was wrongly specified
###############################################
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff #THIS TIME IT IS CORRECT
#> [1] 66

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
#> [1] 0.63
specificity(data = y_hat, reference = test_set$sex)
#> [1] 0.833

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#27.4.7 ROC and precision-recall curves
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
p<- 0.9
n<- length(test_index)
y_hat<- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p))%>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

