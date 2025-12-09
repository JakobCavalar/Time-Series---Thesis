library(forecast)
library(ggplot2)
library(dplyr)

train <- ts_data[1:170, ]

model_loess_01 <- loess(X_t ~ time, data = train, span = 0.1)
model_loess_03 <- loess(X_t ~ time, data = train, span = 0.3)
model_loess_07 <- loess(X_t ~ time, data = train, span = 0.7)

ts_data$loess_01 <- predict(model_loess_01, newdata = ts_data)
ts_data$loess_03 <- predict(model_loess_03, newdata = ts_data)
ts_data$loess_07 <- predict(model_loess_07, newdata = ts_data)

# --- 2) Lineares Modell für Prognose ab 171 ---
model_lin <- lm(X_t ~ time, data = ts_data[1:170, ])
ts_data$pred_lin <- predict(model_lin, newdata = ts_data)

# --- 3) Kombinierte Zeitreihe: Original bis 170, ab 171 linear prognostiziert ---
ts_data$X_comb <- ifelse(ts_data$time <= 170, ts_data$X_t, ts_data$pred_lin)

# --- 4) LOESS auf der kombinierten Serie (Prognoseverlängerung) ---
ts_data$loess_01_p <- loess(X_comb ~ time, span = 0.1, data = ts_data) |> predict(ts_data)
ts_data$loess_03_p <- loess(X_comb ~ time, span = 0.3, data = ts_data) |> predict(ts_data)
ts_data$loess_07_p <- loess(X_comb ~ time, span = 0.7, data = ts_data) |> predict(ts_data)

# Bereichsindikator
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")


# --- 5) Plot ---
ggplot(ts_data, aes(x = date)) +
  
  geom_line(aes(y = X_t, color = factor("Zeitreihe",
                                        levels = c("Zeitreihe", "Trend-Konjunktur",
                                                   "LWR (Span = 0.1)", "LWR (Span = 0.3)", "LWR (Span = 0.7)"))),
            alpha = .6, lwd = .6) +
  
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), alpha = .8, lwd = .9) +
  
  # Modellbereich (durchgezogen)
  geom_line(data=subset(ts_data, bereich=="Modell"),
            aes(y=loess_01, color="LWR (Span = 0.1)"), lwd=.9) +
  geom_line(data=subset(ts_data, bereich=="Modell"),
            aes(y=loess_03, color="LWR (Span = 0.3)"), lwd=.9) +
  geom_line(data=subset(ts_data, bereich=="Modell"),
            aes(y=loess_07, color="LWR (Span = 0.7)"), lwd=.9) +
  
  # Prognosebereich (gestrichelt)
  geom_line(data=subset(ts_data, bereich=="Vorhersage"),
            aes(y=loess_01_p, color="LWR (Span = 0.1)"), linetype="dashed", lwd=.9) +
  geom_line(data=subset(ts_data, bereich=="Vorhersage"),
            aes(y=loess_03_p, color="LWR (Span = 0.3)"), linetype="dashed", lwd=.9) +
  geom_line(data=subset(ts_data, bereich=="Vorhersage"),
            aes(y=loess_07_p, color="LWR (Span = 0.7)"), linetype="dashed", lwd=.9) +
  
  scale_color_manual(values = c(
    "Zeitreihe"="black",
    "Trend-Konjunktur"="blue3",
    "LWR (Span = 0.1)"="red3",
    "LWR (Span = 0.3)"="green4",
    "LWR (Span = 0.7)"="purple"
  )) +
  
  theme_minimal() +
  coord_cartesian(ylim = c(100,112)) +
  geom_vline(xintercept = ts_data$date[170], linetype="dotted") +
  
  labs(title = "Schätzung und Prognose der Trend-Konjunktur-Komponente",
       subtitle = "Modellierung und Vorhersage der TKK mit LWR abhängig von der Glättungsstärke",
       y = "Wert", x = "Datum", color = "Legende")

