library(olsrr)
library(lattice)
library(MASS)
library(car)
z <- read.csv('C:/Users/DELL/Desktop/petrol_data.csv')
z

summary(z)
mod1 <- lm( CP ~ Ptax + AI + PH + PPDL, data = z )

plot(mod1)

summary(mod1)
anova(mod1)
ols_plot_cooksd_bar(mod1)

y.hat <- fitted(mod1)
z$e.hat <- round(e.hat, 4)
head(e.hat)
e.hat <- resid(mod1)
z$e.hat <- round(e.hat, 4)
head(e.hat)
inter.stud.resid <- rstandard(mod1)
exter.stud.resid <- rstudent(mod1)
hii <- hatvalues(mod1)

plot(z$CP,col = "red",type = "b" )
points(y.hat,col = "blue", type = "b")
legend(5,950,c('Observed','Fitted'),col = c('red','blue'),pch = c(1,1))

plot( e.hat ~ z$CP)

plot( y.hat ~ inter.stud.resid )
plot(e.hat ~ y.hat)
round(cor(y.hat,e.hat),4)

plot(y)

#test for normality
qqnorm(z$e.hat)
qqline(z$e.hat)
shapiro.test(z$CP)
#remove normality
l <- boxcox(mod1)
lmda <- l $ x[which.max(l$y)]
lmda
#removal of normlity
new_CP <- ((z$CP ^ lmda) - 1)/ lmda
z$new_CP <- round(new_CP, 4)
head(new_CP)
shapiro.test(new_CP)

mod2 <- lm( new_CP ~ Ptax + AI + PH + PPDL, data = z)
e.hat.new <- resid(mod2)
y.hat.new <- fitted(mod2)
z$e.hat.new <- round(e.hat.new, 4)
head(e.hat.new)
summary(mod2)

qqnorm(e.hat.new)
qqline(e.hat.new)

plot(z$new_CP,col = "red",type = "b" )
points(y.hat.new,col = "blue", type = "b")
legend(40,1.655,c('Observed','Fitted'),col = c('red','blue'),pch = c(1,1))

plot( e.hat ~ z$CP)

cor(z[ c ("new_CP","PH")])
ols_test_score(mod2)
##checking linearity

e.hat1 <- e.hat.new + (-1.287659e-03)*(z$Ptax)
e.hat2 <- e.hat.new + (-3.610020e-06)*(z$AI)
e.hat3 <- e.hat.new + (-6.435308e-08)*(z$PH)
e.hat4 <- e.hat.new + (5.539794e-02)*(z$PPDL)

xyplot(e.hat1~z$Ptax)
xyplot(e.hat1~z$Ptax, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y,...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
cor(e.hat1,z$Ptax)
xyplot(e.hat2~z$AI)
xyplot(e.hat2~z$AI, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y,...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
cor(e.hat2,z$AI)
xyplot(e.hat3~z$PH)
xyplot(e.hat3~z$PH, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y,...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
cor(e.hat3,z$PH)
xyplot(e.hat4~z$PPDL)
xyplot(e.hat4~z$PPDL, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y,...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
cor(e.hat4,z$PPD)

#detection of outlier and high-leverage point
z$hii <- round(hii, 4)
head(hii)
z$inter.stud.resid <- round(inter.stud.resid, 4)
head(inter.stud.resid)
x = 1:48
plot(z$inter.stud.resid ~ x)
plot(z$hii ~ x)
abline(0,0,0.2083)
z$hii <-FALSE
z$hii[z$hii > 10 / 48] <- TRUE
which(hii > 10/48)
z[c(6, 7,12, 37, 45),]

summary(mod2)
anova(mod2)

 di <- resid(mod2)/sqrt(0.00025608)
 CDi <- cooks.distance(mod2)
  HI <- ols_hadi(mod2)$hadi
 potential.function <- hii / (1-hii)
 residual.function <- ((hii + 1)/(1 - hii)) * (di^2 / (1-di^2))
 plot(residual.function ~ potential.function)
 plot(HI ~ x)
 which(residual.function > 0.14)
 which(potential.function>=0.29)
 which( HI > 1)
 exter.stud.resid1 <- rstudent(mod2)
 plot( exter.stud.resid1 ~ x)
 abline(-2,0,2)
 which(abs(exter.stud.resid1) > 2)




##multicollinearity checking
library(MASS)
vif(mod2)
cor(z[c("Ptax","AI","PH","PPDL")])
eigen.values <- eigen(cor(z[c("Ptax","AI","PH","PPDL")]))
eigen.values
kappa <- sqrt(1.5794476/0.3590563)
kappa
sum(1/1.5794476, 1/1.1439556, 1/ 0.9175405, 1/0.3590563)
#model selection
 mod3 <- lm( new_CP ~ Ptax + AI + PPDL, data = z)
summary(mod3)

y.hat.new1 <- fitted(mod3)

plot(z$new_CP,col = "red",type = "b" )
points(y.hat.new1,col = "blue", type = "b")
legend(40,1.655,c('Observed','Fitted'),col = c('red','blue'),pch = c(1,1))

plot( e.hat ~ z$CP)

anova(mod3)
e.hat.new1 <- resid(mod3)
qqnorm(e.hat.new1)
qqline(e.hat.new1)
exter.stud.resid2 <- rstudent(mod3)
#outlier detection in new model
 ols_plot_cooksd_bar(mod3)
 ols_plot_dffits(mod3)
 HI <- ols_hadi(mod3)$hadi
 plot(x,HI)
 which(HI >0.6)
 
 plot(exter.stud.resid2 ~x)
 abline(-2,0,2)
 which(abs(exter.stud.resid2) > 2)
 
 hii2 <- hatvalues(mod3)
plot(hii2 ~ x) 
abline(0,0,0.166)
which( hii2 > 1/6)
plot(exter.stud.resid2 ~ hii2)

di2 <- resid(mod3)/sqrt(0.00025766)
potential.function2 <- hii2 / (1-hii2)
residual.function2 <- ((hii2 + 1)/(1 - hii2)) * (di2^2 / (1-di2^2))
plot(residual.function2 ~ potential.function2)
abline(0,1)
text(potential.function2,residual.function2,labels = 1:48, pos = 4)
which(potential.function2 > 0.15)
which(residual.function2 > 0.14)
ols_plot_resid_lev(mod3)
HI1 <- ols_hadi(mod3)$hadi
plot(HI1 ~ x)
which(HI > 0.6)
#linearity again
e.hat11 <- e.hat.new1 + (-1.147e-03 )*(z$Ptax)
e.hat21 <- e.hat.new1 + ( -3.648e-06)*(z$AI)
#e.hat31 <- e.hat.new1 + (-6.435308e-08)*(z$PH)
e.hat41 <- e.hat.new1 + (5.641e-02)*(z$PPDL)

xyplot(e.hat11~z$Ptax)
xyplot(e.hat11~z$Ptax, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
xyplot(e.hat21~z$AI)
xyplot(e.hat21~z$AI, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
        fit <- lm(y ~ x)
         panel.abline(fit)
       })
xyplot(e.hat41~z$PPDL)
xyplot(e.hat41~z$PPDL, data = z, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
##outlier removal
z_new <- z[-c(5,33,40),]
 mod4 <- lm(new_CP ~ Ptax + AI + PPDL, data = z_new)
summary(mod4)

y.hat.new3 <- fitted(mod4)
e.hat.new3 <- resid(mod4)

plot(z_new$new_CP,col = "red",type = "b" )
points(y.hat.new3,col = "blue", type = "b")
legend(40,1.655,c('Observed','Fitted'),col = c('red','blue'),pch = c(1,1))
##linearity
e.hat12 <- e.hat.new3 + (-1.043091e-03 )*(z_new$Ptax)
e.hat22 <- e.hat.new3 + ( -3.873523e-06)*(z_new$AI)
#e.hat31 <- e.hat.new1 + (-6.435308e-08)*(z$PH)
e.hat42 <- e.hat.new3 + (5.234512e-02)*(z_new$PPDL)

xyplot(e.hat12~z_new$Ptax)
xyplot(e.hat12~z_new$Ptax, data = z_new, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
xyplot(e.hat22~z_new$AI)
xyplot(e.hat22~z_new$AI, data = z_new, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })
xyplot(e.hat42~z_new$PPDL)
xyplot(e.hat42~z_new$PPDL, data = z_new, pch = 18,
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.loess(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit)
       })


#polynomial regression

mod5 <- lm( new_CP ~ Ptax + AI + PPDL, data = z_new)
summary(mod5)
 y.hat.new4 <- fitted(mod5)
 
 plot(z_new$new_CP,col = "red",type = "b" )
 points(y.hat.new4,col = "blue", type = "b")
 legend(40,1.655,c('Observed','Fitted'),col = c('red','blue'),pch = c(1,1))
 
 
 #model selection
mod21 <- lm( new_CP ~ Ptax + AI + PH + PPDL, data = z_new)
 
 
ols_step_all_possible(mod21)
