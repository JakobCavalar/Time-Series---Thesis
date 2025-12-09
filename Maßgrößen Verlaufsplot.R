library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

x11_verlauf <- data.frame(szenario = c(1:9),
                          rmsetrend = c(0.38592,0.40589,0.48331,
                                        0.74222,0.74825,0.79863,
                                        0.75173,0.75477,0.80451),
                          rmsesaison = c(0.45475,0.45661,0.48599,
                                         0.43687,0.43729,0.46888,
                                         0.37034,0.37054,0.40678),
                          sqr = c(0.00091,0.00186,0.00257,
                                  0.00181,0.00223,0.00282,
                                  0.00281,0.00322,0.00392),
                          treff = c(0.77896,0.64975,0.63289,
                                    0.82338,0.66404,0.60520 ,
                                    0.87206,0.68705,0.60235))

BV_verlauf <- data.frame(szenario = c(1:9), 
                         rmsetrend = c(0.32837,0.33056,0.42489,
                                       0.65145,0.65260 ,0.72500 ,
                                       0.65259,0.65398,0.72804),
                         rmsesaison = c(0.59497,0.59551,0.64397,
                                        0.57393,0.57449,0.62419,
                                        0.56857,0.56912,0.61908),
                          sqr = c(0.00341,0.00349,0.00420 ,
                                  0.00303,0.00310 ,0.00380 ,
                                  0.00345,0.00353,0.00426),
                          treff = c(0.77896,0.64233,0.70651,
                                    0.80447,0.65115,0.64204,
                                    0.85986,0.67514,0.62288))
x11_long <- x11_verlauf %>%
  pivot_longer(cols = -szenario, names_to = "kennzahl", values_to = "wert")

bv_long <- BV_verlauf %>%
  pivot_longer(cols = -szenario, names_to = "kennzahl", values_to = "wert")

combined_limits <- bind_rows(x11_long, bv_long) %>%
  group_by(kennzahl) %>%
  summarise(
    ymin = min(wert, na.rm = TRUE) - 0.05 * abs(min(wert, na.rm = TRUE)),
    ymax = max(wert, na.rm = TRUE) + 0.05 * abs(max(wert, na.rm = TRUE))
  )

combined_limits[1:2,2] <- min(combined_limits[1:2,2])
combined_limits[1:2,3] <- max(combined_limits[1:2,3])

reihenfolge <- c("rmsetrend", "rmsesaison", "sqr", "treff")
labels <- c("RMSE Trend", "RMSE Saison", "SQR", "Treffsicherheit")

verlauf_plot <- function(df_long, title, colors) {
  
  df_long$kennzahl <- factor(df_long$kennzahl,
                             levels = reihenfolge,
                             labels = labels)
  
  limits_df <- combined_limits %>%
    mutate(kennzahl = factor(kennzahl, levels = reihenfolge, labels = labels))
  
  df_long <- left_join(df_long, limits_df, by = "kennzahl")
  
  ggplot(df_long, aes(x = szenario, y = wert, color = kennzahl)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ kennzahl, scales = "free_y", nrow = 2) +
    scale_x_continuous(breaks = 1:9) +
    scale_color_manual(values = colors) +
    labs(title = title, x = "Szenario", y = "Wert der Maßzahlen") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ) +
    geom_blank(aes(y = ymin)) +
    geom_blank(aes(y = ymax))
}

farben <- c("RMSE Trend" = "dodgerblue", "RMSE Saison" = "goldenrod1",
            "SQR" = "maroon1", "Treffsicherheit" = "aquamarine2")

plot_x11 <- verlauf_plot(x11_long, "X-13-ARIMA Verlauf der Kennzahlen", farben)
plot_bv  <- verlauf_plot(bv_long,  "BV4.1 Verlauf der Kennzahlen", farben)


# In einem Plot das Ganze
make_plot <- function(df_long, title) {
  ggplot(df_long, aes(x = szenario, y = wert, color = kennzahl)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(limits = global_y_limits) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = title, x = "Szenario", y = "Wert", color = "Kennzahl") +
    theme_minimal(base_size = 14)
}

plot_x11 <- make_plot(x11_long, "X-11 Verlauf aller Kennzahlen")
plot_bv  <- make_plot(bv_long,  "BV4.1 Verlauf aller Kennzahlen")
