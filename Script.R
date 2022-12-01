#APRÓXIMAÇÃOINICIALREGRESSÃO####

plot(sen$votos, sen$vgasto, xlab = "Gastos R$", ylab = "Votação", col = "blue")

#A princípio o gráfico plotado apresenta os valores em notação
#científica então usamos:

options(scipen = 999)


#Temos que logaritmizar 
#log = "xy"
#or 
sen$log.vgasto <- log(sen$vgasto)
sen$log.votos <- log(sen$votos)


plot(sen$log.votos, sen$log.vgasto, xlab = "Gastos R$", 
     ylab = "Votação", col = "black", pch= 16)

lines(lowess(sen$log.votos,sen$log.vgasto), col="red", lwd = 3,
      lty = 2)

#título main 

#Informações sobre as opções gráficas do r base
# https://r-coder.com/plot-r/

#Regressão simples####  


modelo1 <- lm(log.votos ~ log.vgasto, data= sen)#Modelo

summary(modelo1)#visualização

#Interpretação: na primeira linha a descrição do modelo que pedimos,
#Nos residuals servem pra gente identificar os pontos influentes, 
#que são os casos que erramos significativamente
#Temos um valor mínimo de -4.69, isto é, errei nisso tudo 
#pra negativo e errei pra mais em 4 (máx).
#O que eu tenho que ver depois ? se eu errei em muitos casos, 
#ou então se foram poucos casos.
#meu valor do p ? de 0.02 ou seja, significativo, 
#pois menor que que 0.05.
#Agora olhando do meu texto (minha v independenten vgastos) 
#é significativa, positiva e a magnitude do teste
#é de 0,95 ou seja, a cada gasto de campanha o candidato 
#tem quase 1% de voto a mais. 

#Análise dos resíduos#### 

resid <- (cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))

obj <- head(cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))
obj2 <- (cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))

#Análise gráfica####
#Gráfico pra análise dos resíduos e outros condicionantes 
par(mfrow=c(2,2))
plot(modelo1)

#Elaboração dos gráficos 
library(ggplot2)
library(ggfortify)
obj1 <- autoplot(modelo1,
                 which = 1:4,
                 nrow = 2,
                 ncol = 2
) 

obj1 + theme_classic()  

#install.packages("psych")
library(psych)


#Podemos fazer mais testes??

#Apresentações do modelo####

##Tabela####
#install.packages("sjPlot")
library(sjPlot)


tab_model(modelo1, show.ci = F, auto.label = T,
         show.se= T, collapse.se = T,
         wrap.labels= 60, p.style = "stars")

##Gráfico####
#install.packages("coefplot")
library(coefplot)

#apresentação gráfica coefplot
coefplot(modelo1, intercept = F, outerCI = F)

#Coefplot elaborado
obj1 <- coefplot(modelo1, title = "2008",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 2)

obj1 + theme_classic() + geom_point(size=2, pch=21, fill="red",
                                    alpha=105) 
##Gráfico de dispersão####
#ggplot
#install.packages("ggplot2")
library(ggplot2)

# Para colocar a fórmula no gráfico
coeficientes <- modelo1$coefficients

#Utilização da funçao sprintf, documentação:
#https://statisticsglobe.com/sprintf-r-function-example
#Basicamente essa função serve para organizar 
#textualmente as informações númericas e 
#não númericas (como o +) que quero que 
#apareçam. 
#NO exemplo que estamos usando, eu quero que apareça 
#2 casa após o ponto, então após o ponto eu
#colocamos o número de casas decimais, 

texto <- sprintf('y = %.2f + %.2fx, r² = %.2f', 
                 coeficientes[1], coeficientes[2], 
                 summary(modelo1)$r.squared)

ggplot(data = sen, 
       aes(x = log.votos, y = log.vgasto)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  geom_text(aes(x=min(log.votos), 
                y=max(log.vgasto), label=texto),
            hjust=0, vjust=1) +
  theme_classic()


#Regressão multipla####

#O que explica os gastos dos candidatos não eleitos?

table(sen$resultado)
# Eleitos Não eleitos 
# 27         175 

library(tidyverse)
#Falar sobre os bancos que apresentam conflitos
#relembrar o == TRUE

SenNaoEleito <- sen %>%
  filter(resultado == "Não eleitos") 


save(SenNaoEleito, file = "SenNaoEleito.RData")

#Variável dependente: Votos
#Variável indepdente: idade, est.civil, 
#escola, vgasto, bens
#Verificar o resumo dos dados glimpse(SenNaoEleito)
glimpse(SenNaoEleito)

#ftc- factor
#int- integer
#Ord- Ordination
#dbl - double

#Int assim como o dbl são usados para armanezar valores 
#numéricos com pontos decimais

SenNaoEleito$votos <- as.numeric(SenNaoEleito$votos)
SenNaoEleito$bens <- as.numeric(SenNaoEleito$bens)
SenNaoEleito$vgasto <- as.numeric(SenNaoEleito$vgasto)

summary(sen$est.civil)

modelo2 <- lm(votos ~ vgasto + bens + idade + 
                est.civil + escola,
              data = SenNaoEleito) # modelo multivarido
options(scipen = 999)
summary(modelo2)


#Análise dos resíduos#### 

resid <- (cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))

obj <- head(cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))
obj2 <- (cbind(sen$log.votos, predict(modelo1), residuals(modelo1)))

#Análise gráfica####
#Gráfico pra análise dos resíduos e outros condicionantes 
par(mfrow=c(2,2))
plot(modelo2)

#Elaboração dos gráficos 
library(ggplot2)
library(ggfortify)
obj1 <- autoplot(modelo2,
                 which = 1:4,
                 nrow = 2,
                 ncol = 2
) 

obj1 + theme_classic()  
#Diagnóstico de Colinearidade####
# Variance Inflation Factor

#install.packages("olsrr")
library(olsrr)

ols_vif_tol(modelo2)


#Colinear- quando existe só dois preditores estão relacionados
#multi- quando tem mais de dois
#Quando dois preditores se relacionam fortemente significa que eles estão medindo 
#a mesma coisa.

#1ª coluna cada um dos preditores, 2º coluna com valores de tolerância
#Quanto mais próximo o coeficiente está de 1, mais ele é livre 
#isto é, não dependente de nenhum outro valor.Isso significa que não significa que 
#não há problema de colenearidade. 
#VIF tem que está menor do que 4 ou 5

ols_eigen_cindex(modelo2)
#Se o maior condition index for maior do que 30 então há um problema. 

#Apresentações do modelo####

##Tabela####
#install.packages("sjPlot")
library(sjPlot)


tab_model(modelo2, show.ci = F, auto.label = T,
          show.se= T, collapse.se = T,
          wrap.labels= 60, p.style = "stars")

##Gráfico####
#install.packages("coefplot")
library(coefplot)

#apresentação gráfica coefplot
coefplot(modelo2, intercept = F, outerCI = F)

#Coefplot elaborado
obj1 <- coefplot(modelo2, title = "2008",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 2)

obj1 + theme_classic() + geom_point(size=2, pch=21, fill="red",
                                    alpha=105) 

#Gráfico de dispersão####
graph <- scatterplot3d(SenNaoEleito$votos ~ SenNaoEleito$vgasto + 
                         SenNaoEleito$bens + SenNaoEleito$idade +
                         SenNaoEleito$idade,
                       pch = 16, angle = 25
                       , color = "steelblue", box = FALSE,
                       xlab="", ylab="", zlab="")

