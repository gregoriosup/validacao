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


conc <- c(2.53,
          5.05,
          10.11,
          15.16,
          20.21,
          25.26,
          30.32,
          35.37,
          40.42)
abs <- c(0.052,
         0.107,
         0.199,
         0.294,
         0.392,
         0.481,
         0.580,
         0.676,
         0.772)

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


# Supondo que 'lm_model' é o seu modelo de regressão linear
residuals <- residuals(mod)

# Calcular a função de autocorrelação e autocorrelação parcial dos resíduos
acf_res <- acf(residuals)
pacf_res <- pacf(residuals)

# Plotar os resultados
par(mfrow=c(2,1))
plot(acf_res, main="Função de Autocorrelação dos Resíduos")
plot(pacf_res, main="Função de Autocorrelação Parcial dos Resíduos")

