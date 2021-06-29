#Caret package, training and test sets, and overall accuracy
#install.packages("tidyverse")
library(tidyverse)
#install.packages("caret")
library(caret)
#install.packages("dslabs")
library(dslabs)
data("heights")

# define the outcome and predictors
y<- heights$sex
x<- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 
#or earlier, remove the sample.kind argument
# check using comman R.version

test_index<- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set<- heights[test_index, ]
train_set<- heights[-test_index, ]
 
#guess the outcome
y_hat<- sample(c("Male", "Female"), length(test_index), replace = TRUE )

y_hat<- sample(c("Male", "Female"), length(test_index), replace = TRUE)%>%
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarise(mean(height), sd(height))

#predict Male if height is within two standard deviations from the average male
y_hat<- ifelse(x>62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
#The accuracy goes up from 0.50 to about 0.80:
mean(y== y_hat)

# can we do better?
# we examine 10 cutoffs and choose the best one
cutoff<- seq(61,70)
accuracy<- map_dbl(cutoff, function(x){
  y_hat<- ifelse(train_set$height>x, "Male", "Female")%>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy)%>%
  ggplot(aes(cutoff, accuracy))+
  geom_point()+
  geom_line()

max(accuracy)

#what is the best cutoff
best_cutoff<- cutoff[which.max(accuracy)]
best_cutoff

#check that it is not over optimistic
y_hat<- ifelse(test_set$height > best_cutoff, "Male", "Female")%>%
  factor(levels = levels(test_set$sex))

y_hat<- factor(y_hat)

mean(y_hat == test_set$sex)
