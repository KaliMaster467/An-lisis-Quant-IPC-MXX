library(tidyquant)
library(quantmod)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(corrplot)
library(kableExtra)
library(knitr)
library(quadprog)
library(tibble)
library(PerformanceAnalytics)
library(timetk)
library(timeSeries)
library(fPortfolio)
library(patchwork)


tickers <- c("RA.MX", "OMAB.MX", "ALSEA.MX", "BBAJIOO.MX", "VESTA.MX", "KOFUBL.MX", "CHDRAUIB.MX",
             "ASURB.MX", "CUERVO.MX", "GRUMAB.MX", "GCC.MX", "BIMBOA.MX", "AC.MX", "TLEVISACPO.MX",
             "LABB.MX", "GMEXICOB.MX", "PINFRA.MX", "GAPB.MX", "KIMBERA.MX", "CEMEXCPO.MX", "AMXB.MX",
             "Q.MX", "BOLSAA.MX", "GCARSOA1.MX", "GFNORTEO.MX", "MEGACPO.MX", "ORBIA.MX", "GENTERA.MX")

length(tickers)
ipc_data <- tq_get(tickers, from = "2023-01-01", to = "2025-02-15", get = "stock.prices")
key <- tq_get(tickers, from = "2023-01-01", to = "2025-02-15", get = "key.ratios")

ipc_data <- ipc_data |>
  group_by(symbol) |>
  tq_transmute(select = close, 
               mutate_fun = periodReturn, 
               period = "monthly",
               method = "arithmetic") 

ipc_data |> 
  ggplot() + 
  geom_line(aes( x = date, y = monthly.returns*100, color = symbol)) + 
  labs(
    title = "Histórico de retornos IPC MEXICO (^MXX)", 
    subtitle = "Elaboración propia", 
    caption = "Fuente: Yahoo Finance", 
    x = "Fecha", 
    y = "Retornos mensuales %"
  ) + 
  theme_tq() + 
  scale_colour_tq()

####Tabla con estimadores

estimadores <- ipc_data |>
  group_by(symbol) |>
  summarise(
    Media = mean(monthly.returns), 
    Varianza = var(monthly.returns),
    Desviacion_Estandar = sd(monthly.returns),
    Skewness = skewness(monthly.returns)
  )
##Rendimiento mensual esperado del mercado

rm <- mean(estimadores$Media)
riesgo_activos <- mean(estimadores$Desviacion_Estandar)

riesgo_activos

estimadores |>
  ggplot() + 
  geom_col(aes( x = symbol, y = Media*100), fill = "darkblue") + 
  geom_hline(yintercept = rm, linetype = "dashed", color = "red", size = 1) + 
  annotate("text", x = max(estimadores$symbol), y = max(estimadores$Media-.2), 
           label = "Rendimiento promedio = 0.7%", color = "red", hjust = 1, vjust = 1, fontface = "bold") +
  theme_tq() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title = "Rendimientos mensuales de stock´s promedio IPC MEXICO (^MXX)", 
    subtitle = "Elaboración propia",
    caption = "Fuente: Yahoo Finance", 
    x = "Stock", 
    y = "%"
  )
###Gráfica de riesgo mensual (Desviación Estándar)

estimadores |>
  ggplot() + 
  geom_col(aes( x = symbol, y = Desviacion_Estandar*100), fill = "darkblue") + 
  theme_tq() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title = "Desviación estándar mensual por stock IPC MEXICO (^MXX)", 
    subtitle = "Elaboración propia",
    caption = "Fuente: Yahoo Finance", 
    x = "Stock", 
    y = "%"
  )

##Matriz Varianza Covarianza

# Calcular retornos mensuales aritméticos
ipc_returns <- ipc_data |>
  spread(symbol, monthly.returns) 

# Calcular la matriz de varianza-covarianza
cov_matrix <- cov(ipc_returns[,-1], use = "pairwise.complete.obs")

# Convertir la matriz en formato largo para ggplot
cov_long <- melt(cov_matrix)

# Graficar heatmap de la matriz de varianza-covarianza
ggplot(cov_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(cov_long$value, na.rm = TRUE)) +
  labs(title = "Matriz de Varianza-Covarianza de Retornos Mensuales (IPC México)",
       x = "Ticker",
       y = "Ticker",
       fill = "Cov") +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### CORRPLOT
cor_matriz <- cor(ipc_returns[,-1], use = "pairwise.complete.obs")
corrplot(cor_matriz,  # Tipo de visualización
         tl.col = "black",   # Color de los tickers (puedes cambiar "black" por otro)
         tl.cex = .8)

####RENDIMIENTO DEL MERCADO


ipc_mxx <- tq_get("^MXX", from = "2023-01-01", to = "2025-02-15", get = "stock.prices")

# Calcular los rendimientos mensuales aritméticos
ipc_mxx_returns <- ipc_mxx |>
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "monthly",
               method = "arithmetic")

# Mostrar los primeros datos
head(ipc_mxx_returns)
rm <- mean(ipc_mxx_returns$monthly.returns)
rm
risk_rm <- sd(ipc_mxx_returns$monthly.returns)
risk_rm
sharpe_ratio <- (rm-.009)/sd(ipc_mxx_returns$monthly.returns)
sharpe_ratio

library(ggplot2)
library(dplyr)

# Encontrar los puntos máximos y mínimos
max_point <- ipc_mxx_returns %>% filter(monthly.returns == max(monthly.returns, na.rm = TRUE))
min_point <- ipc_mxx_returns %>% filter(monthly.returns == min(monthly.returns, na.rm = TRUE))

# Graficar
ipc_mxx_returns |>
  ggplot() + 
  geom_line(aes(x = date, y = monthly.returns*100), color = "darkblue") + 
  geom_hline(yintercept = rm*100, color = "red") + 
  geom_hline(yintercept = 0, color = "black") + 
  geom_point(data = max_point, aes(x = date, y = monthly.returns*100), color = "green", size = 3) + 
  geom_point(data = min_point, aes(x = date, y = monthly.returns*100), color = "red", size = 3) + 
  geom_text(data = max_point, aes(x = date, y = monthly.returns*100-2, label = round(monthly.returns*100, 2)), 
            vjust = -1, color = "green", fontface = "bold") +
  geom_text(data = min_point, aes(x = date, y = monthly.returns*100, label = round(monthly.returns*100, 2)), 
            vjust = 1.5, color = "red", fontface = "bold") +
  labs(
    title = "Rendimiento esperado de mercado RM (^MXX)", 
    subtitle = "Elaboración propia",
    caption = "Fuente: Yahoo Finance", 
    x = "Fecha", 
    y = "%"
  ) + 
  theme_tq() + 
  scale_colour_tq()


##Unir las tablas

ipc_returns <- ipc_returns |>
  right_join(ipc_mxx_returns, by = "date")

ipc_returns <- ipc_returns |>
  rename(MXX = monthly.returns)

##BETA DE CADA STOCK



ipc_reg <- tq_get(tickers, from = "2023-01-01", to = "2025-02-15", get = "stock.prices")


ipc_reg <- ipc_reg |>
  group_by(symbol) |>
  tq_transmute(select = close, 
               mutate_fun = periodReturn, 
               period = "monthly",
               method = "log")

ipc_reg <- ipc_reg |>
  right_join(ipc_mxx_returns, by = "date")

#Hacemos regresión

bet <- ipc_reg |>
  group_by(symbol) |>
  summarise(
    beta = cov(monthly.returns.x, monthly.returns.y) / var(monthly.returns.y), .groups = "drop"
  )

#UNIMOS A TABLA DE ESTIMADORES

estimadores <- estimadores |>
  right_join(bet, by = "symbol")

#Calculamos el Sharpie-Ratio

estimadores <- estimadores |>
  mutate(
    Sharpe_Ratio = (Media - .009)/Desviacion_Estandar, 
    Treynor_Ratio = (Media - .009)/beta
  )
##Guardamos tabla
sink("tabla.tex")
cat(
  kable(estimadores, format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = "striped")
)
sink()

library(readr)
write_csv(estimadores, "tabla.csv")



###Beta promedio tickes IPC MEX

betaprom <- mean(estimadores$beta)
betaprom
#sd beta tickers IPC

betasd <- sd(estimadores$beta)
betasd
####Graficar frontera eficiente de mercado
##### PORTAFOLIO SOLO CON LOS TICKERS: ALSEA, CHEDRAUI, GCC, GRUMAB, Q

tickers_portafolio <- c("ALSEA.MX","CHDRAUIB.MX","GRUMAB.MX", "GCC.MX", "Q.MX")

portafolio <- tq_get(tickers_portafolio, from = "2023-01-01", to = "2025-02-15", get = "stock.prices")
portafolio <- portafolio |>
  group_by(symbol) |>
  tq_transmute(select = close, mutate_fun = periodReturn, 
               period = "monthly",
               method = "arithmetic")

portafolio |>
  ggplot() + 
  geom_line(aes(x = date, y = monthly.returns, color = symbol)) + 
  geom_hline(yintercept = mean(portafolio$monthly.returns), color = "red") + 
  geom_hline(yintercept = 0, color = "black") + 
  labs(
    title = "Rendimientos mensuales del portafolio", 
    subtitle = "Elaboración propia", 
    caption = "Fuente: Yahoo Finance", 
    x = "Periodo", 
    y = "Rendimientos"
  ) + 
  scale_color_tq() + 
  theme_tq()

retorno_portafolio <- portafolio |>
  spread(symbol, monthly.returns)

re_portafolio <- colMeans(retorno_portafolio[,-1])

###MATRIZ VARIANZA COVARIANZA

cov_matrix_portafolio <- cov(retorno_portafolio[,-1])

# Convertir la matriz en formato largo para ggplot
cov_long <- melt(cov_matrix_portafolio)

# Graficar heatmap de la matriz de varianza-covarianza
ggplot(cov_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(cov_long$value, na.rm = TRUE)) +
  labs(title = "Matriz de Varianza-Covarianza de Retornos Mensuales del Portafolio",
       x = "Ticker",
       y = "Ticker",
       fill = "Cov") +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### CORRPLOT
cor_matriz <- cor(retorno_portafolio[,-1])
corrplot(cor_matriz,  # Tipo de visualización
         tl.col = "black",   # Color de los tickers (puedes cambiar "black" por otro)
         tl.cex = .8)


####Graficar portafolios

retorno_portafolio <- retorno_portafolio |>
  select(-date) |>
  as.matrix()

re_portafolio <- as.matrix(re_portafolio)
cov_matrix_portafolio



#####

set.seed(12)
n_portafolios <- 10000
pesos <- matrix(runif(n_portafolios * length(tickers_portafolio), 0, 1), ncol = length(tickers_portafolio))
pesos <- pesos / rowSums(pesos)  # Normalizar pesos para que sumen 1

# 7️⃣ Calcular rendimiento y riesgo de cada portafolio
rendimientos_esperados <- pesos %*% re_portafolio  # Ahora funciona sin error
volatilidad <- sqrt(rowSums((pesos %*% cov_matrix_portafolio) * pesos))

datos_frontera <- data.frame(Riesgo = volatilidad, Retorno = rendimientos_esperados)
frontera_eficiente <- datos_frontera %>%
  arrange(Riesgo) %>%
  filter(Retorno >= cummax(Retorno)) 

# 8️⃣ Identificar el portafolio de mínima varianza
indice_min_var <- which.min(volatilidad)
portafolio_min_var <- c(rendimientos_esperados[indice_min_var], volatilidad[indice_min_var])
# 9️⃣ Graficar la Frontera Eficiente
ggplot(data = data.frame(Riesgo = volatilidad, Retorno = rendimientos_esperados), aes(x = Riesgo, y = Retorno)) +
  geom_point(aes(color = Retorno / Riesgo), alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Frontera eficiente de inversión del portafolio",
       subtitle = "Elaboración propia",
       x = "Volatilidad (Riesgo)",
       y = "Retorno Esperado",
       caption = "Fuente: Yahoo Finance",
       color = "Ratio Retorno/Riesgo") +
  theme_tq() +
  geom_point(aes(x = portafolio_min_var[2], y = portafolio_min_var[1]), color = "green", size = 4, shape = 1) +
  annotate("text", x = portafolio_min_var[2]+.002, y = portafolio_min_var[1], label = "Mínima Varianza", vjust = -1)
####Otras gráfi



#####
#####
ipc_data <- tq_get(tickers, from = "2023-01-01", to = "2025-02-15", get = "stock.prices")


ipc_data <- ipc_data |>
  group_by(symbol) |>
  tq_transmute(select = close, 
               mutate_fun = periodReturn, 
               period = "monthly",
               method = "arithmetic")


matriz_rend <- ipc_data %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  select(-date) %>%
  na.omit() %>%
  as.matrix()

cov_matrix <- cov(matriz_rend)
media_retorno <- as.matrix(colMeans(matriz_rend))

# Convertir a matriz columna

# 6️⃣ Simulación de portafolios aleatorios
set.seed(12)
n_portafolios <- 550000
pesos <- matrix(runif(n_portafolios * length(tickers), 0, 1), ncol = length(tickers))
pesos <- pesos / rowSums(pesos)  # Normalizar pesos para que sumen 1

# 7️⃣ Calcular rendimiento y riesgo de cada portafolio
rendimientos_esperados <- pesos %*% media_retorno  # Ahora funciona sin error
volatilidad <- sqrt(rowSums((pesos %*% cov_matrix) * pesos))

datos_frontera <- data.frame(Riesgo = volatilidad, Retorno = rendimientos_esperados)
frontera_eficiente <- datos_frontera %>%
  arrange(Riesgo) %>%
  filter(Retorno >= cummax(Retorno)) 


# 8️⃣ Identificar el portafolio de mínima varianza
indice_min_var <- which.min(volatilidad)
portafolio_min_var <- c(rendimientos_esperados[indice_min_var], volatilidad[indice_min_var])
# 9️⃣ Graficar la Frontera Eficiente
ggplot(data = data.frame(Riesgo = volatilidad, Retorno = rendimientos_esperados), aes(x = Riesgo, y = Retorno)) +
  geom_point(aes(color = Retorno / Riesgo), alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Frontera eficiente de posibilidades de inversión",
       subtitle = "Elaboración propia",
       x = "Volatilidad (Riesgo)",
       y = "Retorno Esperado",
       caption = "Fuente: Yahoo Finance",
       color = "Ratio Retorno/Riesgo") +
  theme_tq() +
  geom_point(aes(x = portafolio_min_var[2], y = portafolio_min_var[1]), color = "green", size = 4, shape = 1) +
  annotate("text", x = portafolio_min_var[2]+.002, y = portafolio_min_var[1], label = "Mínima Varianza", vjust = -1)
####Otras gráficas

######## CON CURSO UDEMY

# Descargar precios ajustados
precios <- tq_get(tickers_portafolio, from = "2023-01-01", to = "2025-02-15", get = "stock.prices")

# Calcular rendimientos logarítmicos
retornos <- precios %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", type = "arithmetic") %>%
  spread(symbol, monthly.returns)

# Asegurar que la matriz tenga nombres correctos y convertir a timeSeries
retornos <- na.omit(retornos)  # Eliminar valores NA
rownames(retornos) <- retornos$date  # Asignar fechas como nombres de fila
matriz_retornos <- as.matrix(retornos[,-1])  # Convertir en matriz numérica
colnames(matriz_retornos) <- tickers_portafolio  # Asegurar nombres de columna

# Convertir la matriz a timeSeries (formato requerido por fPortfolio)
time_series_retornos <- timeSeries(matriz_retornos, charvec = retornos$date)

# Crear objeto portfolioData con los retornos en formato timeSeries
datos_portafolio <- portfolioData(time_series_retornos)

# Definir especificaciones del portafolio (con tasa libre de riesgo de 1.1% anual)
especificaciones <- portfolioSpec()
setRiskFreeRate(especificaciones) <- 0.009  # Convertir a tasa mensual

# Construir la frontera eficiente
frontera_eficiente <- portfolioFrontier(datos_portafolio, especificaciones, constraints = "LongOnly")

# Graficar la frontera eficiente
plot(frontera_eficiente, risk = "Cov")


### Sesion 6: Pesos de los Portafolios de Varianza MÌnima y de Tangencia
portafolio.var.min = minvariancePortfolio(
  datos_portafolio, `setRiskFreeRate<-`(portfolioSpec(), 0.009), 
  constraints = "LongOnly")

pesos.var.min = getWeights(portafolio.var.min) 
View(pesos.var.min)
barplot(pesos.var.min, main = "Pesos del portafolio de mínima varianza", col = "lightblue")
pie(pesos.var.min, main = "Distribución de pesos del portafolio de mínima varianza")


portafolio.tangencia = tangencyPortfolio(
  datos_portafolio, `setRiskFreeRate<-`(portfolioSpec(), 0.009), 
  constraints = "LongOnly")
pesos.port.tangencia = getWeights(portafolio.tangencia)
View(pesos.port.tangencia)
barplot(pesos.port.tangencia, main = "Pesos del portafolio de tangencia", col = "lightblue")
pie(pesos.port.tangencia, main = "Distribución de pesos del portafolio de tangencia")


###Rendimiento del portafolio tangente
rendimiento_portafolio_tangencia <- pesos.port.tangencia %*% re_portafolio
rendimiento_portafolio_tangencia
##Rendimiento del portafolio mínima varianza 
rendimiento_portafolio_min_var <- pesos.var.min %*% re_portafolio
rendimiento_portafolio_min_var

#Riesgo portafolio tangencia
cov_matrix_portafolio <- as.matrix(cov_matrix_portafolio)
var_tangencia <- t(pesos.port.tangencia) %*% cov_matrix_portafolio %*% pesos.port.tangencia
riesgo_portafolio_tangencia <- sqrt(var_tangencia)
riesgo_portafolio_tangencia
#Riesgo portafolio min var
cov_matrix_portafolio <- as.matrix(cov_matrix_portafolio)
var_min <- t(pesos.var.min) %*% cov_matrix_portafolio %*% pesos.var.min
riesgo_portafolio_varmin <- sqrt(var_min)
riesgo_portafolio_varmin

#Sharpe Ratio
sharpe_ratio_portafolio_tangencia <- (rendimiento_portafolio_tangencia-.009)/riesgo_portafolio_tangencia
sharpe_ratio_portafolio_tangencia

sharpe_ratio_portafolio_minvar <- (rendimiento_portafolio_min_var-.009)/riesgo_portafolio_varmin
sharpe_ratio_portafolio_minvar
#####
#####
##### Evolución portafolio de tangencia

####Análiis estadístico del portafolio
portafolio

pesos.port.tangencia <- as.matrix(pesos.port.tangencia)
dim(pesos.port.tangencia)

retorno_portafolio <- portafolio |>
  spread(symbol, monthly.returns) |>
  select(-date) |>
  as.matrix()


dim(retorno_portafolio)

retornos_mensuales <- retorno_portafolio %*% pesos.port.tangencia
retornos_mensuales

row.names(retornos_mensuales) <- retornos$date


ts.plot(retornos_mensuales, col = "blue", lwd = 2, 
        main = "Evolución de los Retornos Mensuales del Portafolio de Tangencia",
        sub = "Datos basados en el rendimiento del portafolio",
        xlab = "Tiempo (Meses)", ylab = "Retornos")


hist(retornos_mensuales, breaks = 7, col = "lightblue", main = "Histograma de Retornos Mensuales", xlab = "Retorno", ylab = "Frecuencia")
max(retornos_mensuales)
min(retornos_mensuales)

set.seed(123)
n <- 100
valores <- rnorm(n * 10, mean = rendimiento_portafolio_tangencia, sd = riesgo_portafolio_tangencia)
valores_filtrados <- valores[valores >= min(retornos_mensuales) & valores <= max(retornos_mensuales)]

plot(valores_filtrados)

if (length(valores_filtrados) >= n) {
  arreglo_aleatorio <- sample(valores_filtrados, n)
} else {
  stop("No se generaron suficientes valores dentro del rango, ajusta la desviación estándar.")
}


hist(arreglo_aleatorio, breaks = 15, col = "lightblue", probability = TRUE,
     main = "Distribución Normal Ajustada", xlab = "Valores", ylab = "Densidad")

# Generar la curva de la distribución normal
x_vals <- seq(min(arreglo_aleatorio), max(arreglo_aleatorio), length.out = 100)
y_vals <- dnorm(x_vals, mean = rendimiento_portafolio_tangencia, sd = riesgo_portafolio_tangencia)

# Agregar la línea de densidad normal
lines(x_vals, y_vals, col = "red", lwd = 2)
#Probabilidad de tener rendimientos de 2.5% o mayor
probabilidad <- 1 - pnorm(0.025, mean = rendimiento_portafolio_tangencia, sd = riesgo_portafolio_tangencia)
probabilidad
print(paste("Probabilidad de que el rendimiento sea mayor a 0.025:", round(probabilidad, 4)))

probabilidad_perdidas <- pnorm(0, mean = rendimiento_portafolio_tangencia, sd = riesgo_portafolio_tangencia)
print(paste("Probabilidad de que haya pérdidas:", round(probabilidad_perdidas, 4)))
probabilidad_perdidas
####Simulación Monte Carlo

inversion_inicial <- 1000000
pesos.port.tangencia
inversion_pesos_portafolio <- inversion_inicial * pesos.port.tangencia
dim(inversion_pesos_portafolio)


portafolio_precios <- tq_get(tickers_portafolio, get = "stock.prices", from = "2023-01-01", to = "2025-02-15")
portafolio_precios <- portafolio_precios |>
  select(symbol, date, close)

portafolio_precios <- portafolio_precios |>
  spread(symbol, close)

##OBTENER VALOR DEL PORTAFOLIO

portafolio_precios$Valor_Portafolio <- as.matrix(portafolio_precios[,-1]) %*% pesos.port.tangencia

portafolio_precios <- portafolio_precios |>
  tq_mutate(select = Valor_Portafolio, mutate_fun = periodReturn,
               period = "daily",
               method = "arithmetic")

portafolio_precios <- as.timeSeries(portafolio_precios)


par(mfrow = c(2,1))  # Dividir la ventana gráfica en 2 filas

ts.plot(portafolio_precios$Valor_Portafolio, col = "darkblue", lwd = 1, 
        main = "Evolución Precio del Portafolio de Tangencia",
        sub = "Datos basados en el rendimiento del portafolio",
        xlab = "Tiempo (Días)", ylab = "Precio")

ts.plot(portafolio_precios$daily.returns, col = "red", lwd = 1,
        main = "Rendimientos Diarios del Portafolio",
        xlab = "Tiempo (Días)", ylab = "Rendimiento",
        ylim = c(min(portafolio_precios$daily.returns, na.rm = TRUE), 
                 max(portafolio_precios$daily.returns, na.rm = TRUE)))

# Restaurar la configuración de gráficos a 1 solo gráfico
par(mfrow = c(1,1))

# Configurar la ventana gráfica
par(mar = c(5, 5, 4, 5))  # Ajustar márgenes para la segunda escala en el eje derecho

# Graficar el precio del portafolio (eje izquierdo)
plot(portafolio_precios$Valor_Portafolio, type = "l", col = "darkblue", lwd = 2,
     main = "Evolución Precio del Portafolio y Rendimientos Diarios",
     xlab = "Tiempo (Días)", ylab = "Precio del Portafolio")

# Agregar un nuevo gráfico sin borrar el anterior
par(new = TRUE)

# Graficar los rendimientos diarios (eje derecho)
plot(portafolio_precios$daily.returns, type = "l", col = "red", lwd = .5, axes = FALSE,
     xlab = "", ylab = "", ylim = c(min(portafolio_precios$daily.returns, na.rm = TRUE), 
                                    max(portafolio_precios$daily.returns, na.rm = TRUE)))

# Agregar el eje derecho para los rendimientos
axis(side = 4)
mtext("Rendimientos Diarios", side = 4, line = 3, col = "red")

# Agregar una leyenda
legend("bottomright", legend = c("Precio del Portafolio", "Rendimientos Diarios"),
       col = c("darkblue", "red"), lty = 1, lwd = 2)


# 🔹 1. Parámetros de la simulación
S0 <- portafolio_precios$Valor_Portafolio[1]      # Precio inicial del portafolio
mu <- rendimiento_portafolio_tangencia    # Retorno esperado anual (5%)
sigma <- riesgo_portafolio_tangencia   # Volatilidad anual (20%)
T <- 1         # Horizonte de tiempo en años
dt <- 1/252    # Paso de tiempo (1 día de trading)
n_sim <- 1000  # Número de simulaciones
n_steps <- T / dt  # Número de pasos en la simulación

# 🔹 2. Generación de escenarios
set.seed(123) # Para reproducibilidad
Z <- matrix(rnorm(n_steps * n_sim), ncol = n_sim)  # Simulaciones de ruido aleatorio
S <- matrix(0, nrow = n_steps, ncol = n_sim)  # Matriz de precios de la acción

# 🔹 3. Simulación de Montecarlo
S[1, ] <- S0  # Inicializamos con el precio actual

for (t in 2:n_steps) {
  S[t, ] <- S[t-1, ] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[t-1, ])
}

# 🔹 4. Convertir en DataFrame para visualización
df <- as.data.frame(S)
df$Día <- 1:n_steps
df_long <- df %>%
  pivot_longer(-Día, names_to = "Simulación", values_to = "Precio")

# 🔹 5. Graficar los resultados
ggplot(df_long, aes(x = Día, y = Precio, group = Simulación)) +
  geom_line(alpha = 0.1, color = "blue") +  # Líneas semitransparentes
  geom_line(data = df_long %>% filter(Simulación == "V1"), aes(x = Día, y = Precio), color = "red", size = 1) + # Una simulación destacada
  labs(title = "Simulación Montecarlo de Precios del Portafolio de Tangencia",
       subtitle = paste0(n_sim, " escenarios, horizonte de ", T, " año"),
       x = "Días de Trading",
       y = "Precio") +
  theme_tq()
## Pronostico arima

library(forecast)
library(broom)


modelo_arima <- auto.arima(portafolio_precios$Valor_Portafolio)
pronostico <- forecast(modelo_arima, h=30) # Predecir 10 días
plot(pronostico, col = "darkblue", lwd = 1, main = "Pronóstico del Valor del Portafolio a 30 días", 
     xlab = "Periodo (Días)", 
     ylab = "Precio")
summary(pronostico)
glance(modelo_arima)
kable(tidy(modelo_arima), caption = "Resultados del modelo ARIMA")
tidy(modelo_arima)

autoplot(modelo_arima)

limites <- data.frame(
  Fecha = time(pronostico$mean),
  Pronostico = pronostico$mean,
  Limite_Inferior_80 = pronostico$lower[,1],
  Limite_Superior_80 = pronostico$upper[,1],
  Limite_Inferior_95 = pronostico$lower[,2],
  Limite_Superior_95 = pronostico$upper[,2]
)

# Mostrar la tabla con los límites del pronóstico
print(limites)
primeros_pronosticos <- data.frame(head(limites))
primeros_pronosticos
##PRONOSTICO FECHA HOY

portafolio_precios_h <- tq_get(tickers_portafolio, get = "stock.prices", from = "2025-02-17", to = "2025-02-23")
portafolio_precios_h <- portafolio_precios_h |>
  select(symbol, date, close)

portafolio_precios_h <- portafolio_precios_h |>
  spread(symbol, close)

portafolio_precios_h$Valor_portafolio <- as.matrix(portafolio_precios_h[,-1]) %*% pesos.port.tangencia
portafolio_precios_h


###Análisis técnico 
###MACD STOCKS INDIVIDUALES
stocks <- tq_get(tickers_portafolio, get = "stock.prices", from = "2023-01-01", to = "2025-02-15")
stocks <- stocks |>
  group_by(symbol) |>
  tq_mutate(select = close, 
            mutate_fun = MACD,  # ¡Aquí no se usan paréntesis!
            nFast = 12, nSlow = 26, nSig = 9, maType = SMA) |>
  mutate(diff = macd - signal) |>  # Agregar diferencia (histograma)
  select(-(open:volume))  

#graficamos
stocks |> 
  filter(date >= "2024-06-01") |>
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) + 
  geom_line(aes(y = macd, col = symbol)) + 
  geom_line(aes(y = signal), color = "blue", linetype = 2) + 
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) + 
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") + 
  labs(
    title = "Moving Average Convergence Divergence ",
    y = "MACD"
  ) + 
  theme_tq() + 
  scale_color_tq() 

###MACD PORTAFOLIO

portafolio_precios_macd <- tq_get(tickers_portafolio, get = "stock.prices", from = "2023-01-01", to = "2025-02-15")
portafolio_precios_macd <- portafolio_precios_macd |>
  select(symbol, date, close)

portafolio_precios_macd <- portafolio_precios_macd |>
  spread(symbol, close)

##OBTENER VALOR DEL PORTAFOLIO

portafolio_precios_macd$Valor_Portafolio <- as.matrix(portafolio_precios_macd[,-1]) %*% pesos.port.tangencia

portafolio_precios_macd <- portafolio_precios_macd |>
  tq_mutate(select = Valor_Portafolio, mutate_fun = MACD,
            nFast = 12,
            nSlow = 26, 
            nSig = 9,
            maType = SMA) |>
  mutate(diff = macd - signal)

####CANDLESTICKS MENSUALES DEL PORTAFOLIO


data <- tq_get(tickers_portafolio, from = "2023-01-01", to = "2025-02-15")

# 2️⃣ Convertir datos diarios a datos mensuales usando summarize()
data_monthly <- data %>%
  mutate(year_month = floor_date(date, "month")) %>%
  group_by(symbol, year_month) %>%
  summarize(
    open = first(open),
    high = max(high),
    low = min(low),
    close = last(close),
    .groups = "drop"
  ) %>%
  rename(date = year_month)  # Renombrar para mantener consistencia

# 3️⃣ Función para graficar candlesticks con ggplot2
candlestick_plot <- function(stock_data, stock_name) {
  ggplot(stock_data, aes(x = date)) +
    geom_segment(aes(xend = date, y = low, yend = high), color = "black") +  # Líneas de mechas
    geom_rect(aes(xmin = date - 10, xmax = date + 10, ymin = pmin(open, close), ymax = pmax(open, close),
                  fill = close > open), color = "black") +  # Velas
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) + # Colores: verde subida, rojo bajada
    labs(title = paste("Candlestick Mensual -", stock_name), x = "Fecha", y = "Precio") +
    theme_tq() + 
    theme(legend.position = "none")
}

# 4️⃣ Generar gráficos para cada acción
plot_ALSEA <- candlestick_plot(data_monthly %>% filter(symbol == "ALSEA.MX"), "Alsea")
plot_GCC <- candlestick_plot(data_monthly %>% filter(symbol == "GCC.MX"), "GCC")
plot_Q <- candlestick_plot(data_monthly %>% filter(symbol == "Q.MX"), "Q")
plot_CHDRAUIB <- candlestick_plot(data_monthly %>% filter(symbol == "CHDRAUIB.MX"), "Chedraui")
plot_GRUMAB <- candlestick_plot(data_monthly %>% filter(symbol == "GRUMAB.MX"), "Grumab")

library(gridExtra)

grid.arrange(plot_ALSEA, plot_GCC, plot_Q, plot_CHDRAUIB, plot_GRUMAB, ncol = 2)
# 5️⃣ Mostrar los tres gráficos en un mismo panel

#graficamos
portafolio_precios_macd |> 
  filter(date >= "2023-06-01") |>
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0) + 
  geom_line(aes(y = macd), color = "red") + 
  geom_line(aes(y = signal), color = "blue", linetype = 2) + 
  geom_bar(aes(y = diff), stat = "identity") +
  labs(
    title = "Moving Average Convergence Divergence (MACD) del 
Portafolio de Tangencia",
    y = "MACD",
    x = "Fecha", 
    subtitle = "Elaboración propia",
    caption = "Fuente: Yahoo Finance"
  ) + 
  theme_tq() + 
  scale_color_tq() 

###Análisis fundamental

# Cargar librerías
library(httr)
library(jsonlite)

# Definir el token de la API (reemplázalo con el tuyo)
token <- "40277bc5a772b8c6d119c6f1e091f5"

# Definir el ticker de la acción mexicana
ticker <- "GCC"  # Cambia esto según la empresa que necesites

# Construir la URL de la API para obtener datos fundamentales
url <- paste0("https://api.databursatil.com/v1/financieros?token=", token, "&emisora=", ticker, "&periodo=4T_2024&financieros=posicion,resultado_trimestre")



"https://api.databursatil.com/v1/financieros?token=40277bc5a772b8c6d119c6f1e091f5&emisora=GCC&periodo=4T_2024&financieros=posicion,resultado_trimestre"
# Hacer la solicitud GET a la API
response <- GET(url)

# Verificar si la solicitud fue exitosa
if (status_code(response) == 200) {
  
  # Convertir la respuesta a formato JSON
  data <- fromJSON(content(response, "text"))
  
  # Extraer el P/E Ratio (si está disponible)
  pe_ratio <- data %>% filter(indicador == "PER")
  
  # Mostrar el resultado
  print(pe_ratio)
  
} else {
  cat("Error en la solicitud: Código", status_code(response), "\n")
}
#####Rendimiento esperado  de los inversionista

estimadores_portafolio <- estimadores |>
  filter(symbol %in% c("GCC.MX", "Q.MX", "GRUMAB.MX", "CHDRAUIB.MX", "ALSEA.MX"))

estimadores_portafolio <- estimadores_portafolio |>
  mutate(
    rendimiento_esperado = .009 + beta*(rm-.009)
  )

estimadores_portafolio

json_text <- content(response, as = "text", encoding = "UTF-8")
datos <- fromJSON(json_text)
cat(prettify(json_text))
df <- as.data.frame(datos)
View(df)  # Abre una ventana interactiva en RStudio
