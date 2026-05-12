# Librerías
library(dplyr)
library(ggplot2)

# Dataset
df<-women

# Renombrado de los índices
colnames(df)<-c("Altura", "Peso")

# Visualización rápida
glimpse(df)
summary(df)
sum(is.na(df))

# Identificaión de valores atípicos mediante boxplots
boxplot(df$Altura, col="Purple")
  title("Altura")

boxplot(df$Peso, col="Orange")
  title("Peso")

# Creación del modelo
modelo_rg<-lm(Peso~Altura, data=df)

# Resultados
summary(modelo_rg)
plot(modelo_rg)

# Residuos
shapiro.test(residuals(modelo_rg))

# Elementos para la ecuación
coeficientes=modelo_rg$coefficients
bo=coeficientes[1]
b1=coeficientes[2]
ec<-paste("f(x)= ",coeficientes[2],"x + ", round(coeficientes[1],3))

# Gráfica con la ecuación
ggplot(df, aes(x=Altura, y=Peso))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = "salmon")+
  theme_light()+
  labs(xlabel="Altura", ylabel="Peso")+
  annotate("text", x=68, y=125, label=ec, color="darkgreen", size=5)

# Corrección del modelo, de lineal a curvo
# Ajuste del modelo
modelo_rg2<-lm(Peso~Altura+I(Altura^2), data=df)

# Resultados
summary(modelo_rg2)
plot(modelo_rg2)

# Test para verificar
shapiro.test(residuals(modelo_rg2))

# Elementos para la ecuación
coeficientes=modelo_rg2$coefficients
bo=coeficientes[1]
b1=coeficientes[2]
ec<-paste("f(x)= ",round(coeficientes[2],3),"x + ", round(coeficientes[2],3),"x^2 + ", round(coeficientes[1],3))

# Gráfica con la ecuación
ggplot(df, aes(x=Altura, y=Peso))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x+I(x^2), se = FALSE, col = "black")+
  theme_light()+
  labs(xlabel="Altura", ylabel="Peso")+
  annotate("text", x=68, y=125, label=ec, color="blue", size=5)
