WHOdata<-read.csv(file.choose(), header=T)
WHOdata2 <- WHOdata[complete.cases(WHOdata),]
install.packages("ggplot2")
install.packages("car")
install.packages("psych")

write.csv(WHOdata2,file = "documents.csv", row.names = F)

library(car)
library(ggplot2)
library(psych)

##Full Model##
modelP<-lm(life_expectancy ~ alcohol + health_spend + bmi + population, data = WHOdata2)
summary(modelP)

##Means and standard deviation##
print(mean(WHOdata2$life_expectancy,na.rm = TRUE))
print(SD(WHOdata2$life_expectancy,na.rm = TRUE))

print(mean(WHOdata2$alcohol,na.rm = TRUE))
print(sd(WHOdata2$alcohol,na.rm = TRUE))

print(mean(WHOdata2$bmi,na.rm = TRUE))
print(sd(WHOdata2$bmi,na.rm = TRUE))

print(mean(WHOdata2$health_spend,na.rm = TRUE))
print(sd(WHOdata2$health_spend,na.rm = TRUE))

print(mean(WHOdata2$population,na.rm = TRUE))
print(sd(WHOdata2$population,na.rm = TRUE))

##Correlation Matrix##
cormatrix <- cor(WHOdata[,c(3, 4, 5, 6, 7)])

##Scatterplot Matrix##
pairs(~ life_expectancy + alcohol + health_spend + bmi + population, data = WHOdata2, 
      main = "Scatterplot Matrix")

##Confidence intervals for slopes##
confint(modelP, level=0.95)

##check for any high standardized residuals, leverage points, and influential points##
WHO2 = fortify(modelP)
lev.cutoff.low<-2*(4+1)/139
lev.cutoff.high<-3*(4+1)/139
ggplot(WHO2, aes(x=.hat, y=.stdresid, size=.cooksd)) + geom_point() + 
  labs(x = "Leverage Values",y="Standardized Residuals",title="Influence Plot")+
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")

##Residual Plot##
ggplot(WHO2, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Prestige",y="Standardized Residuals",title="Standardized Residual Plot") +
  theme_bw()+geom_hline(yintercept=0)+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")

##Levene Test##
WHO2$life_expectancy <- ifelse(WHO2$.fitted < median(WHO2$.fitted), c("group1"), c("group2")) 
WHO2$life_expectancy <- factor(WHO2$life_expectancy, levels = c("group1", "group2"))
leveneTest(.resid ~ life_expectancy, data=WHO2)
par(mfrow=c(1,2))

##Histogram of Residuals##
hist(WHO2$.resid, main="Histogram of Residuals", xlab="Residuals")

##Boxplot of standardizd residual##
boxplot(WHO2$.stdresid, ylab = "Standardized Residuals", main = "Boxplot of Standardized Residuals")

##Normal QQ Plot##
qqnorm(WHO2$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE, pch = 16)
qqline(WHO2$.resid)

##Shapiro-Wilk test for Normality##
shapiro.test(WHO2$.resid)

##Model Selection Process *This code was done seperate from the above code##
data123=read.csv("who2013.csv", header=T)
data123
y=lm(life_expectancy~alcohol+ health_spend+bmi+population, data=WHO2)
summary(y)
install.packages("olsrr")
library(olsrr)

##Forward selection##
ols_step_forward_p(y,penter=.05, details=TRUE)

##Backward elimination##
ols_step_backward_p(y, prem=.05, details=TRUE)

##Final Model##
x = lm(life_expectancy~alcohol+ health_spend+bmi, data=WHO2)
summary(x)