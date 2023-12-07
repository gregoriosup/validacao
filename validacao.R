### pacotes####

library(lmtest)
library(car)


#### exatidao ####

rec_temp0 <- c(104.21,
110.05,
99.03,
102.04,
101.06,
94.06,
100.60,
99.00,
96.45)

t.test(rec_temp0, mu=100)



rec_temp1 <- c(89.73,
               87.90,
               89.63,
               88.20,
               87.24,
               88.02,
               88.59,
               86.32,
               88.11)


t.test(rec_temp1, mu=100)


#### linearidade ####


conc <- c(2.5,
          5,
          10,
          15,
          20,
          25,
          30,
          35,
          40)
abs <- c(0.057,
         0.1043333333,
         0.1963333333,
         0.2933333333,
         0.4093333333,
         0.5123333333,
         0.6153333333,
         0.712,
         0.8183333333)

df_lin <- data.frame(conc, abs)

mod <- lm(df_lin)

anova(mod)

anova(mod) #teste F <0,05 nao rejeita sig do mod
par(mfrow=c(2,2))
plot(mod) #homocedasticidade e residuos
par(mfrow=c(1,1))
bptest(mod) #homocedasticidade
shapiro.test(mod$residuals) #normalidade dos residuos >0,05 é normal
boxplot(mod$residuals) #outlier dos residuos
boxplot(df_lin$abs)
summary(mod)
cor(df_lin$conc, df_lin$abs, method = "pearson")
plot(df_lin$conc, df_lin$abs) #verificar linearidade



