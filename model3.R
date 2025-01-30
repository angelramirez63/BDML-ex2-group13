# Fill here the code to estimate the linear_model3 from the notebook
## use the studres funtion from MASS package

db_int<-db_int %>% mutate(m1_std_residuals= studres(linear_model) )


ggplot(db_int , aes(y = m1_std_residuals , x = id , color= age, shape= as.factor(gender) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Residuals",
       title = "") # labels

db_int <- db_int %>% filter(m1_std_residuals<2 & m1_std_residuals>-2 )

# re run the model
linear_model3<- lm(
  totalHoursWorked ~ ofic_ingLab + nmenores  +  nmenores*gender
                                 + H_Head + age + gender,
  data=db_int)



stargazer(linear_model, linear_model2, linear_model3  , type="text",
          covariate.labels=c("Mean Ocu Income","N under 18","Male",
                             "Hausehold Head","Age", "N under 18 x Male" ))
