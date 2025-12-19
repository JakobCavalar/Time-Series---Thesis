library(forecast)
library(ggplot2)
library(deseats)
library(RJDemetra)
library(cowplot)

Daten <- read.xlsx("Daten.xlsx")
ts <-  ts(Daten$VerbraucherpreisindexDE, frequency = 12, start = 1991)

# Baseline
plot(ts)
plot(decompose(ts, type = "additive"))

# Analyse mit BV4.1 und X-13-ARIMA
bv4.1 <- BV4.1(ts, type = "monthly")
Trend <- bv4.1@decomp[,"Trend"]
Seasonality <- bv4.1@decomp[,"Seasonality"]
Noise <- bv4.1@decomp[,"Rest"]

X11 <- x13(ts, spec = "RSA0")
Trend_X11 <- X11[["final"]][["series"]][,"t"]
Seasonality_X11 <- X11[["final"]][["series"]][,"s"]
Noise_X11 <- X11[["final"]][["series"]][,"i"]

# Plotting
plot1 <- ggplot() +
  geom_line(aes(x = 1:length(ts), y = ts, color = "Original"), size = 2, alpha = 1) +
  geom_line(aes(x = 1:length(Trend_X11), y = Trend_X11, color = "BV4.1 Trend"), size = 1, alpha = 0.7) +
  geom_line(aes(x = 1:length(Trend), y = Trend, color = "X-13-ARIMA Trend"), size = 1, alpha = 0.7) +
  labs(x = NULL, y = "TKK") +
  scale_color_manual(values = c("Original" = "gray", "BV4.1 Trend" = "magenta1",
                                "X-13-ARIMA Trend" = "dodgerblue"),
                     breaks = c("Original", "BV4.1 Trend", "X-13-ARIMA Trend")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 12),
        axis.text.x = element_blank()) +
  theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"))

plot2 <- ggplot() +
  geom_line(aes(x = 1:length(Seasonality_X11), y = Seasonality_X11, color = "BV4.1 Saison"),
            size = 1, alpha = 0.7) +
  geom_line(aes(x = 1:length(Seasonality), y = Seasonality, color = "X-13-ARIMA Saison"),
            size = 1, alpha = 0.7) +
  labs(x = NULL, y = "SK") +
  scale_color_manual(values = c("BV4.1 Saison" = "magenta1", "X-13-ARIMA Saison" = "dodgerblue"),
                     breaks = c("BV4.1 Saison", "X-13-ARIMA Saison")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 12),
        axis.text.x = element_blank()) +
  theme(legend.position = "none")

plot3 <- ggplot() +
  geom_line(aes(x = 1:length(Noise_X11), y = Noise_X11, color = "X-13-ARIMA Residuen"),
            size = 1, alpha = 0.7) +
  geom_line(aes(x = 1:length(Noise), y = Noise, color = "BV4.1 Residuen"), size = 1, alpha = 0.7) +
  labs(x = "Beobachtungen", y = "stoch. K.") +
  scale_color_manual(values = c("X-13-ARIMA Residuen" = "dodgerblue", "BV4.1 Residuen" = "magenta1"),
                     breaks = c("X-13-ARIMA Residuen", "BV4.1 Residuen")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  theme(legend.position = "none")

plot_grid(plot1, plot2, plot3, ncol = 1, rel_heights = c(2, 1, 1))







