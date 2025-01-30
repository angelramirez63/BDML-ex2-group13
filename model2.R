
#Influential Observations.
## leverage (when the value is high the observation can be influential)
db_int<- db_int %>% mutate(leverage = hatvalues(linear_model))

## residuals (If is small enough the observation will not be influential)
db_int<- db_int %>% mutate(residuals= linear_model$residuals)

N <- nrow(db_int)

db_int$id<- seq(1 , N)

a<- ggplot(db_int , aes(y = leverage , x = id , color= ofic_ingLab, shape= as.factor(gender) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Leverage",
       title = "") # labels
print(a)


b<- ggplot(db_int , aes(y = leverage , x = residuals  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Residuals",  
       y = "Leverage",
       title = "") # labels


# Arrange the ggplot2 plots side by side using grid.arrange()
grid.arrange(a, b, ncol = 2)

#The mean leverage:
p <- mean(db_int$leverage)
p

#Rule of thumb -> influential observations these that have a 
#leverage bigger that 2 or 3 times the mean leverage.
cutt <- 3*p
cutt

#drop
db_int2 <-  db_int %>% 
  dplyr:: filter(leverage<= cutt)

# re run the model
linear_model2<- lm(totalHoursWorked ~ ofic_ingLab + nmenores  +  nmenores*gender + H_Head + age + gender, data=db_int2  )

