library(forecast)
library(ggplot2)
library(dplyr)

# --- Modelle nur bis Beobachtung 170 ---
train <- ts_data[1:170, ]

# ARIMA-Modelle mit unterschiedlichen Parametrisierungen
fit_arima_011 <- Arima(train$X_t, order = c(0,1,1))
fit_arima_110 <- Arima(train$X_t, order = c(1,1,0))
fit_arima_111 <- Arima(train$X_t, order = c(1,2,1))

# --- Vorhersagen über gesamte Zeitreihe ---
ts_data$fit_arima_011 <- c(fitted(fit_arima_011), forecast(fit_arima_011, h = nrow(ts_data)-170)$mean)
ts_data$fit_arima_110 <- c(fitted(fit_arima_110), forecast(fit_arima_110, h = nrow(ts_data)-170)$mean)
ts_data$fit_arima_111 <- c(fitted(fit_arima_111), forecast(fit_arima_111, h = nrow(ts_data)-170)$mean)

# Bereichsindikator
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")

# --- Plot ---
ggplot(ts_data, aes(x = date)) +
  geom_line(aes(y = X_t, color = factor("Zeitreihe", levels = c("Zeitreihe", "Trend-Konjunktur", "ARIMA(1,0,0)", "ARIMA(1,1,0)", "ARIMA(1,2,1)"))), alpha = 0.6, lwd = .6 ) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), alpha = .8, lwd = .9) +
  
  geom_line(data = subset(ts_data, bereich=="Modell"), aes(y = fit_arima_011, color="ARIMA(0,1,1)"), lwd=.9) +
  geom_line(data = subset(ts_data, bereich=="Vorhersage"), aes(y = fit_arima_011, color="ARIMA(0,1,1)"), linetype="dashed", lwd=.9) +
  
  geom_line(data = subset(ts_data, bereich=="Modell"), aes(y = fit_arima_110, color="ARIMA(1,1,0)"), lwd=.9) +
  geom_line(data = subset(ts_data, bereich=="Vorhersage"), aes(y = fit_arima_110, color="ARIMA(1,1,0)"), linetype="dashed", lwd=.9) +
  
  geom_line(data = subset(ts_data, bereich=="Modell"), aes(y = fit_arima_111, color="ARIMA(1,2,1)"), lwd=.9) +
  geom_line(data = subset(ts_data, bereich=="Vorhersage"), aes(y = fit_arima_111, color="ARIMA(1,2,1)"), linetype="dashed", lwd=.9) +
  
  scale_color_manual(values = c(
    "Zeitreihe"="black",
    "Trend-Konjunktur"="blue3",
    "ARIMA(0,1,1)"="red3",
    "ARIMA(1,1,0)"="green4",
    "ARIMA(1,2,1)"="purple"
  )) +
  theme_minimal() +
  coord_cartesian(ylim = c(100,112)) +
  geom_vline(xintercept = ts_data$date[170], linetype="dotted") +
  labs(title = "Schätzung und Prognose der Trend-Konjunktur-Komponente",
       subtitle = "Modellierung und Vorhersage der TKK mit OLS abhängig von der ARIMA-Spezifikation",
       y = "Wert", x = "Datum", color = "Legende")

