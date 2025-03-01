tickers_sin_mx <- c("RA", "OMAB", "ALSEA", "BBAJIOO", "VESTA", "KOFUBL", "CHDRAUIB",
             "ASURB", "CUERVO", "GRUMAB", "GCC", "BIMBOA", "AC", "TLEVISACPO",
             "LABB", "GMEXICOB", "PINFRA", "GAPB", "KIMBERA", "CEMEXCPO", "AMXB",
             "Q", "BOLSAA", "GCARSOA1", "GFNORTEO", "MEGACPO", "ORBIA", "GENTERA")


ipc_data
returns <- ipc_data |>
  spread(symbol, monthly.returns)

returns <- na.omit(returns)  
rownames(returns) <- returns$date  # Asignar fechas como nombres de fila
matriz_retornos <- as.matrix(returns[,-1])  # Convertir en matriz numérica
colnames(matriz_retornos) <- tickers_sin_mx 

time_series_retornos <- timeSeries(matriz_retornos, charvec = returns$date)

# Crear objeto portfolioData con los retornos en formato timeSeries
datos_portafolio <- portfolioData(time_series_retornos)

especificaciones <- portfolioSpec()
setRiskFreeRate(especificaciones) <- 0.009  # Convertir a tasa mensual

# Construir la frontera eficiente
frontera_eficiente <- portfolioFrontier(datos_portafolio, especificaciones, constraints = "LongOnly")

# Graficar la frontera eficiente
plot(frontera_eficiente, risk = "Cov")
####
#### Evolución del portafolio de tangencia



###
###
###
# Instalar y cargar paquetes necesarios
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(tidyverse)
library(ggplot2)

# 🔹 1. Parámetros de la simulación
S0 <- 100      # Precio inicial de la acción
mu <- 0.05     # Retorno esperado anual (5%)
sigma <- 0.2   # Volatilidad anual (20%)
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
  labs(title = "Simulación Montecarlo de Precios de una Acción",
       subtitle = paste0(n_sim, " escenarios, horizonte de ", T, " año"),
       x = "Días de Trading",
       y = "Precio de la Acción") +
  theme_minimal()
