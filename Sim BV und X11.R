pacman::p_load(deseats, ggplot2, RJDemetra, seasonal, matrixStats, openxlsx, dplyr, patchwork, foreach, doParallel)

# Funktionen - vorher durchlaufen lassen ------------
Summe_quadrierter_Revisisonen <- function(ts, Bereich_Untergrenze, Bereich_Obergrenze,
                                          Methode = x13, frequenz, Output = "SQR"){
  
  if (Bereich_Untergrenze < 52) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("x13", "BV4.1")
  Methoden_Name <- deparse(substitute(Methode))
  
  if (!Methoden_Name %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden sein: x13 oder BV4.1")
  }
  
  if (frequenz == 12) {
    type <- "monthly"
  } else if (frequenz == 4){
    type <- "quarterly"
  } else {
    stop("Zeitreihe muss quartals(4) oder monats(12) Frequenz haben")
  }
  
  
  Bereich <- seq(Bereich_Untergrenze,Bereich_Obergrenze,1)
  Revision_outer <- numeric(length(Bereich))
  
  for (i in Bereich){
    Revision_inner <- numeric(12)
    
    
    for (k in 1:12){
      if (Methoden_Name == "x13") {
        r <- Methode(window(ts, end = c(1 + ((i+k-1) - 1) %/% frequenz, ((i+k-1) - 1) %% frequenz + 1)), spec = "RSA0")
        b <- Methode(window(ts, end = c(1 + ((i+k-2) - 1) %/% frequenz, ((i+k-2) - 1) %% frequenz + 1)), spec = "RSA0")
        Revision_inner[k] <- (r[["final"]][["series"]][,"t"][(i-1)] - b[["final"]][["series"]][,"t"][(i-1)])^2
      } else {
        r <- Methode(window(ts, end = c(1 + ((i+k-1) - 1) %/% frequenz, ((i+k-1) - 1) %% frequenz + 1)), type = type)
        b <- Methode(window(ts, end = c(1 + ((i+k-2) - 1) %/% frequenz, ((i+k-2) - 1) %% frequenz + 1)), type = type)
        Revision_inner[k] <- (r@decomp[,"Trend"][(i-1)] - b@decomp[,"Trend"][(i-1)])^2
      }
    }
    Summe_k <- sum(Revision_inner)
    if (Methoden_Name == "x13") {
      l <- Methode(window(ts, end = c(1 + ((i+ 11) - 1) %/% frequenz, ((i+ 11) - 1) %% frequenz + 1)), spec = "RSA0")
      Entwicklung_last <- (l[["final"]][["series"]][,"t"][(i-1)]) ^2
    } else {
      l <- Methode(window(ts, end = c(1 + ((i+11) - 1) %/% frequenz, ((i+11) - 1) %% frequenz + 1)), type = type)
      Entwicklung_last <- (l@decomp[,"Trend"][(i-1)]) ^2
    }
    
    Revision_outer[i-(Bereich[1]-1)] <- (Summe_k / Entwicklung_last)*100
    print(i)
  }
  
  SQR <- mean(Revision_outer)
  assign(Output, SQR, envir = .GlobalEnv)
}
Revisions_Plot <- function(Daten, Methode = x13, Untergrenze, Obergrenze, Sprünge = 2,
                           frequenz, Plot_out = "Rev_plot"){
  
  
  if (Obergrenze < Untergrenze) {
    stop("Fehler: Die Obergrenze darf nicht kleiner als die Untergrenze sein.")
  }
  
  if ((Obergrenze - Untergrenze) / Sprünge > 13) {
    stop("Fehler: Es können nicht mehr als 15 Linien geplottet werden.
          Bitte Grenzen oder Sprünge anpassen.")
  }
  
  erlaubte_Methoden <- c("x13", "BV4.1")
  Methoden_Name <- deparse(substitute(Methode))
  
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  

  Werte <- c(seq(from = Untergrenze, to = Obergrenze, by = Sprünge))
  
  ts <- ts(Daten[,"X_t"], frequency = frequenz)
  
  if (frequenz == 12) {
    type <- "monthly"
    if (Untergrenze < 52) {
      stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
    }
  } else if (frequenz == 4){
    type <- "quarterly"
    if (Untergrenze < 17) {
      stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
    }
  } else {
    stop("Zeitreihe muss quartals(4) oder monats(12) Frequenz haben")
  }
  
  Frame_list <- lapply(Werte, function(n) {
    if (Methoden_Name == "x13") {
      Modell <- Methode(window(ts, end = c(1 + ((n) - 1) %/% frequenz, ((n) - 1) %% frequenz + 1)), spec = "RSA0")
      Trend_konjunktur <- Modell[["final"]][["series"]][,"t"]
    } else {
      Modell <- Methode(window(ts, end = c(1 + ((n) - 1) %/% frequenz, ((n) - 1) %% frequenz + 1)), type = type)
      Trend_konjunktur <- Modell@decomp[,"Trend"]
    }
    
    data.frame(
      TKgeschätzt = Trend_konjunktur,
      Datum = Daten[["date"]][1:n],
      run = rep(n, n)
    )
  })
  
  Frame_combined <- bind_rows(Frame_list)
  Frame <- data.frame(
    Reihe = Daten[["X_t"]][1:(Obergrenze+5)],
    TK = Daten[["trend"]][1:(Obergrenze+5)],
    Datum = Daten[["date"]][1:(Obergrenze+5)],
    run = Obergrenze + 6
  )
  
  Plot <- ggplot() +
    geom_line(data = Frame_combined, aes(x = Datum, y = TKgeschätzt, group = run, color = as.factor(run)),
              alpha = 0.9, lwd = 1.3) +
    geom_line(data = Frame, aes(x = Datum, y = Reihe, color = "Zeitreihe"), lwd = 1.3, alpha = 0.7) +
    geom_line(data = Frame, aes(x = Datum, y = TK, color = "TKK"), lwd = 1.3, alpha = 0.7) +
    scale_color_manual(
      name = "Anzahl Beobachtungen",
      values = c(
        "Zeitreihe" = "black",
        "TKK" = "blue2",
        setNames(c("red3","green4","purple3","magenta","cyan","darkorange","chartreuse","turquoise",
                   "purple","orchid1","aquamarine", "yellow","hotpink" ,
                   "lightgreen" , "salmon", "indianred", "deepskyblue", "darkolivegreen1"),
                 as.character(sort(unique(Frame_combined$run))))
      )
    ) +
    theme_minimal() +
    ylab("Wert der Zeitreihe")+
    ggtitle(paste0("Revision bei ", deparse(substitute(Methode))))+
    labs(subtitle = "Schätzung der Trend-Konjunktur-Komponente in Abhängigkeit der Anzahl an Beobachtungen", 
         caption = "") #+
  #coord_cartesian(xlim = as.Date(c("2022-01-01", "2025-01-01")), ylim = c(13000,22000))
  c
}
Unsicherheits_Plot <- function(Komponente1 = Trend, Komponente2 = Trend_X11,
                               Data = ts_data, Komponente = "trend", Titel = "",
                               Plot_out = "1"){
  
  # Berechnung der gewünschten Statistiken
  compute_summary <- function(mat) {
    data.frame(
      time = 1:nrow(mat),
      mean = rowMeans(mat, na.rm = TRUE),
      q025 = apply(mat, 1, quantile, probs = 0.025, na.rm = TRUE),
      q25  = apply(mat, 1, quantile, probs = 0.25,  na.rm = TRUE),
      q75  = apply(mat, 1, quantile, probs = 0.75,  na.rm = TRUE),
      q975 = apply(mat, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  }
  
  # Zusammenfassung beider Komponenten
  df1 <- compute_summary(as.matrix(Komponente1))
  df2 <- compute_summary(as.matrix(Komponente2))
  
  # Wahre Komponente
  wahr <- data.frame(time = 1:nrow(Komponente1), trend = ts_data[,Komponente])
  
  # Gemeinsame y-Achsen-Grenzen 
  global_ymin <- min(df1$q025, df2$q025, wahr$trend, na.rm = TRUE)
  global_ymax <- max(df1$q975, df2$q975, wahr$trend, na.rm = TRUE)
  
  # Plotobjekt für eine Komponente
  make_plot <- function(df, title, subtitle = "Lila: geschätzter Trend (mit 50% und 95% KI); Blau: wahrer Trend") {
    ggplot() +
      
      geom_ribbon(aes(x = time, ymin = q025, ymax = q975), data = df,
                  fill = "magenta1", alpha = 0.15) +
      
      geom_ribbon(aes(x = time, ymin = q25, ymax = q75), data = df,
                  fill = "magenta2", alpha = 0.35) +
      
      geom_line(aes(x = time, y = mean), data = df, color = "magenta3", linewidth = 1.2, alpha = 1.0) +
      
      geom_line(aes(x = time, y = trend), data = wahr, color = "deepskyblue", linewidth = 1.2, alpha = 0.75) +
      labs(title = title,
           y = Komponente,
           x = "Time",
           subtitle = subtitle) +
      theme_minimal(base_size = 13) +
      scale_y_continuous(limits = c(global_ymin, global_ymax))
  }
  
  # Plots
  Komponente1 <- make_plot(df1, paste0("Unsicherheiten bei der Schätzung ",Titel," mit BV4.1"))
  Komponente2 <- make_plot(df2, paste0("Unsicherheiten bei der Schätzung ",Titel," mit X11"))
  
#  Komp1 <- make_plot(df1, title = NULL, subtitle = NULL) + theme(
#    axis.title.y = element_blank(),
#    axis.text.y = element_blank(),
#    axis.ticks.y = element_blank(),
#    axis.title.x = element_blank(),
#    axis.text.x = element_blank())
#  Komp2 <- make_plot(df2, title = NULL, subtitle = NULL) + theme(
#    axis.title.y = element_blank(),
#    axis.text.y = element_blank(),
#    axis.ticks.y = element_blank(),
#    axis.title.x = element_blank(),
#    axis.text.x = element_blank())
#  Together <- Komp1 + Komp2 + plot_annotation(title = NULL) & 
#    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Ausgabe
  #assign(Plot_out, Together, envir = .GlobalEnv)
  return(list(Komponente1 = Komponente1, Komponente2 = Komponente2
              #, Plot = Together
  ))
}
simulate_time_series <- function(n = 200, 
                                 trend_slope = 0.15,
                                 trend_det = 1, saison_det = 1, saison = 0,
                                 season_period = 12, season_amplitude1 = 5, 
                                 season_amplitude2 = 5, noise_sd = 0.5, 
                                 outlier_prob = 0.012, outlier_magnitude = 12,
                                 shift_prob = 0.008, shift_magnitude = 5,
                                 seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                                 trend_offset = 0, trend_konj = 0) 
{
  set.seed(seed)
  # Zeitachse
  time <- 1:n
  date <- seq.Date(from = as.Date("2010-01-01"), length.out = n, by = "month")
  
  # 1. Trend-Konjunktur-Komponente
  
  
  if (polynomgrad <= 1) {
    trend <- 100 + trend_slope * time
  } else if (polynomgrad == 2) {
    trend <- 100 + trend_slope * time +
      0.0005 * trend_slope * time^2 
  } else if (polynomgrad == 3) {
    trend <- 100 + trend_slope * time +
      0.0005 * trend_slope * time^2 +
      0.000002 * trend_slope * time^3
  } else if (polynomgrad >= 4) {
    trend <- 100 + trend_slope * time +
      0.0005 * trend_slope * time^2 +
      0.000002 * trend_slope * time^3 +
      0.00000004 * trend_slope * time^4
  }
  
  if (trend_det == 1) {
    trend <- trend
  } else {
    trend <- trend + rnorm(n, mean=0, sd=0.2)
  }
  
  if (trend_offset == 0) {
    trend <- trend
  } else {
    trend[(n-2):n] <- trend[(n-2):n] - trend_offset
  }
  
  if (trend_konj == 0) {
    trend <- trend
  } else {
    trend <- trend + sin(2 * pi * time / 40)
  }
  
  # 2. Saisonale Komponente
  if (saison == 0) {
    seasonality <- season_amplitude1 * sin(2 * pi * time / season_period) +
      season_amplitude2 * cos(2 * pi * time /season_period)  
  }else {
    seasonality <- season_amplitude1 * sin(2 * pi * time / season_period) +
      season_amplitude2 * cos(2 * pi * time / season_period) +
      (season_amplitude1/3)* sin(4 * pi * time / season_period) +
      (season_amplitude2/6) * cos(4 * pi * time / season_period) +
      (season_amplitude1/4) * sin(6 * pi * time / season_period) +
      (season_amplitude2/2) * cos(6 * pi * time / season_period)
    seasonality <- seasonality - mean(seasonality)
  }
  
  if (saison_det == 1) {
    seasonality <- seasonality
  } else {
    seasonality <- seasonality + arima.sim(list(ar = 0.3), n = n, sd = 0.3)
  }
  
  # 3. Stochastische Komponente
  #print("AR and MA must be either number (or vector with numbers adding to) between 0,1")
  noise <- arima.sim(list(ar = AR, ma = MA), n, innov = rnorm(n, mean = 0, sd =  noise_sd))
  
  # 4. Ausreißer
  outliers <- rbinom(n, 1, outlier_prob) * runif(n, -outlier_magnitude, (1/3)*outlier_magnitude)
  outliers[abs(outliers) < 5.5] <- 0
  outliers[38] <- -20
  for (i in 1:n) {
    if (outliers[i] > 7.5 | outliers[i] < -7.5){
      outliers[i+1] = (1/1.18) * outliers[i]
      outliers[i+2] = (1/1.2) * outliers[i+1]
      outliers[i+3] = (1/1.3) * outliers[i+2]
      outliers[i+4] = (1/1.5) * outliers[i+3]
      outliers[i+5] = (1/3) * outliers[i+4]
      outliers[i+6] = (1/4) * outliers[i+5]
      outliers[i+7] = (1/5) * outliers[i+6]
    } else {
      outliers[i] = outliers[i]
    }
    outliers <- outliers[1:n]
  }
  outliers_indicator <- numeric(n)
  for (i in 2:n){
    if (abs(outliers[i]) > abs(outliers[i-1])){
      outliers_indicator[i] <-  1
    } else{
      outliers_indicator[i] <-  0
    }
  }
  
  # 5. Permanent Shift
  shifts <- rep(0, n)
  if (shift_prob > 0) {
    shift_events <- rbinom(n, 1, shift_prob) * runif(n, -shift_magnitude, shift_magnitude)
    for (i in 2:n) {
      shifts[i] <- shifts[i-1] + shift_events[i]  # Kumulativer Shift
    }
  }
  shift_indicator <- c(0, as.numeric(diff(shifts) != 0))
  
  # 6. Zusammensetzen der Zeitreihe
  X_t <- trend  + noise + seasonality #+ outliers + shifts
  
  # 7. Data Frame erstellen
  data <- data.frame(
    time = time,
    date = date,
    X_t = X_t,
    trend = trend,
    seasonality = seasonality,
    noise = noise,
    outliers = outliers,
    outliers_ind = outliers_indicator,
    shifts = shifts,
    shift_ind = shift_indicator
  )
  data$controll <- data$X_t - (data$trend + data$seasonality + data$noise +
                                 data$shifts + data$outliers)
  return(data)
}
Treffsicherheit_j<- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = x13,
                             frequenz = 12,  Verschärfung = 0.5, Output = "TJ"){
  
  
  erlaubte_Methoden <- c("x13", "BV4.1")
  Methoden_Name <- deparse(substitute(Methode))
  
  if (!Methoden_Name %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden sein: x13 oder BV4.1")
  }
  
  if (frequenz == 12) {
    type <- "monthly"
  } else if (frequenz == 4){
    type <- "quarterly"
  } else {
    stop("Zeitreihe muss quartals(4) oder monats(12) Frequenz haben")
  }
  
  Reihe1 <- c(seq(from = Bereich_Untergrenze, to = Bereich_Obergrenze, by = 1))
  Entwicklung_Wahr <- numeric(length(Reihe1))
  Entwicklung_Schätzung_J <- numeric(length(Reihe1))
  S <- numeric(length(Reihe1))
  
  ts <- ts(Daten[,"X_t"], frequency = frequenz)
  
  for (i in Reihe1){
    
    idx <- i - (Reihe1[1] - 1)
    if (Methoden_Name == "x13") {
      Modell_Treff <- Methode(window(ts, end = c(1 + (i - 1) %/% frequenz, (i - 1) %% frequenz + 1)), spec = "RSA0")
      Entwicklung_Schätzung_J[idx] <- Modell_Treff[["final"]][["series"]][,"t"][i] -
        Modell_Treff[["final"]][["series"]][,"t"][i-1]
    } else {
      Modell_Treff <- Methode(window(ts, end = c(1 + (i - 1) %/% frequenz, (i - 1) %% frequenz + 1)), type = type)
      Entwicklung_Schätzung_J[idx] <- Modell_Treff@decomp[,"Trend"][i] -
        Modell_Treff@decomp[,"Trend"][i-1]
    }
    Entwicklung_Wahr[idx] <- Daten[["X_t"]][i] - Daten[["X_t"]][i-1]
    
    if ((Entwicklung_Wahr[idx] >= 0 & Entwicklung_Schätzung_J[idx] >= 0) |
        (Entwicklung_Wahr[idx] < 0 & Entwicklung_Schätzung_J[idx] < 0)){
      S[idx] <- 0.05 * mean(Daten[["X_t"]])
    } else {
      S[idx] <- (0.05 * mean(Daten[["X_t"]])) * (1 - Verschärfung *
                ((1 - ((Entwicklung_Schätzung_J[idx] - Entwicklung_Wahr[idx]) /
                (0.05 * mean(Daten[["X_t"]])))^2)^2))
    }
  }
  Trend_Konjunktur_Entwicklung_J <- (1-((Entwicklung_Schätzung_J-Entwicklung_Wahr)/S)^2)^2
  #Index <- seq(1:length(Entwicklung_Schätzung_J))
  #Frame1 <- data.frame(Index = Index, TKE = Trend_Konjunktur_Entwicklung_J)
  #Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
  #Beste_Schätzung_Konjunktur_Entwicklung_J<- max(Frame1$TKE)
  Treffsicherheit_J <- mean(Trend_Konjunktur_Entwicklung_J)
  assign(Output, Treffsicherheit_J, envir = .GlobalEnv)
}

# Simulation ---------
Simulation <- function(iterations = 2, obs = 84, frequency = 12, trend_slope = 0.02,
                       trend_amplitude = 0, season_amplitude1 = 0.5,
                       season_amplitude2 = 0.5, noise_sd = 1, outlier_prob = 0, 
                       outlier_magnitude = 0, shift_prob = 0,
                       shift_magnitude = 0, polynomgrad = 1,AR = 0, MA = 0,
                       trend_offset = 0, trend_konj = 0){

frequenz <- frequency
if (frequenz == 12) {
  type <- "monthly"
  }
 else if (frequenz == 4){
  type <- "quarterly"
  }
 else {
  stop("Zeitreihe muss quartals(4) oder monats(12) Frequenz haben")
 }
print(type)

Trend <- matrix(nrow = obs, ncol = iterations)
Seasonality <- matrix(nrow = obs, ncol = iterations)
Noise <- matrix(nrow = obs, ncol = iterations)
SQR <- rep(0, iterations)
TJ <- rep(0, iterations)

Trend_X11 <- matrix(nrow = obs, ncol = iterations)
Seasonality_X11 <- matrix(nrow = obs, ncol = iterations)
Noise_X11 <- matrix(nrow = obs, ncol = iterations)
SQR_X11 <- rep(0, iterations)
TJ_X11 <- rep(0, iterations)

for (t in 1:iterations) {
  
  ts_data <-  simulate_time_series(n = obs, trend_slope = trend_slope,
                                   season_period = frequency, season_amplitude1 = season_amplitude1,
                                   season_amplitude2 = season_amplitude2,
                                   noise_sd = noise_sd, outlier_prob = outlier_prob, 
                                   outlier_magnitude = outlier_magnitude, shift_prob = shift_prob,
                                   shift_magnitude = shift_magnitude, seed = (t-1),
                                   polynomgrad = polynomgrad, AR = AR, MA = MA,
                                   trend_offset = trend_offset, trend_konj = trend_konj)
  ts <- ts(ts_data[,"X_t"], frequency = frequenz)
  
  bv4.1 <- BV4.1(ts, type = type)
  Trend[,t] <- bv4.1@decomp[,"Trend"]
  Seasonality[,t] <- bv4.1@decomp[,"Seasonality"]
  Noise[,t] <- bv4.1@decomp[,"Rest"]
  Summe_quadrierter_Revisisonen(ts = ts, Bereich_Untergrenze = (obs - 30),
                               Bereich_Obergrenze = obs, Methode = BV4.1,
                               frequenz = frequency, Output = "SQR_BV")
  SQR[t] <- SQR_BV
  Treffsicherheit_j(Daten = ts_data, Bereich_Untergrenze = (obs - 30),
                    Bereich_Obergrenze = obs, Methode = BV4.1,
                    frequenz = frequency,  Verschärfung = 0.5, Output = "TJ1")
  TJ[t] <- TJ1
  
  X11 <- x13(ts, spec = "RSA0")
  Trend_X11[,t] <- X11[["final"]][["series"]][,"t"]
  Seasonality_X11[,t] <- X11[["final"]][["series"]][,"s"]
  Noise_X11[,t] <- X11[["final"]][["series"]][,"i"]
  Summe_quadrierter_Revisisonen(ts = ts, Bereich_Untergrenze = (obs - 30),
                                Bereich_Obergrenze = obs, Methode = x13,
                                frequenz = frequency, Output = "SQR_X")
  SQR_X11[t] <- SQR_X
  Treffsicherheit_j(Daten = ts_data, Bereich_Untergrenze = (obs - 30),
                    Bereich_Obergrenze = obs, Methode = x13,
                    frequenz = frequency,  Verschärfung = 0.5, Output = "TJX")
  TJ_X11[t] <- TJX
  
  print(t)
  assign("ts_data", ts_data, envir = .GlobalEnv)
}

Ergebnisse <- data.frame(
  Kennzahl = c(
    "MSE Trend BV4.1", "MSE Saison BV4.1", "MSE Fehler BV4.1",
    "Var Trend BV4.1 (Mitte)", "Var Saison BV4.1 (Mitte)", "Var Fehler BV4.1 (Mitte)",
    "Var Trend BV4.1 (gesamt)", "Var Saison BV4.1 (gesamt)", "Var Fehler BV4.1 (gesamt)",
    "Bias Trend BV4.1", "Bias Saison BV4.1", "Bias Noise BV4.1", "SQR BV4.1", "Treffsicherheit BV4.1",
    
    "MSE Trend X11", "MSE Saison X11", "MSE Fehler X11",
    "Var Trend X11 (Mitte)", "Var Saison X11 (Mitte)", "Var Fehler X11 (Mitte)",
    "Var Trend X11 (gesamt)", "Var Saison X11 (gesamt)", "Var Fehler X11 (gesamt)",
    "Bias Trend X11", "Bias Saison X11", "Bias Noise X11", "SQR X11", "Treffsicherheit X11"
  ),
  
  Wert = c(
    mean((Trend - ts_data$trend)^2),
    mean((Seasonality - ts_data$seasonality)^2),
    mean((Noise - as.numeric(ts_data$noise))^2),
    mean(rowVars(as.matrix(Trend))[30:(obs-30)]),
    mean(rowVars(as.matrix(Seasonality))[30:(obs-30)]),
    mean(rowVars(as.matrix(Noise))[30:(obs-30)]),
    mean(rowVars(as.matrix(Trend))),
    mean(rowVars(as.matrix(Seasonality))),
    mean(rowVars(as.matrix(Noise))),
    mean(rowMeans(as.matrix(Trend)) - ts_data$trend),
    mean(rowMeans(as.matrix(Seasonality)) - ts_data$seasonality),
    mean(rowMeans(as.matrix(Noise)) - ts_data$noise),
    mean(SQR),
    mean(TJ),
    
    mean((Trend_X11 - ts_data$trend)^2),
    mean((Seasonality_X11 - ts_data$seasonality)^2),
    mean((Noise_X11 - as.numeric(ts_data$noise))^2),
    mean(rowVars(as.matrix(Trend_X11))[30:(obs-30)]),
    mean(rowVars(as.matrix(Seasonality_X11))[30:(obs-30)]),
    mean(rowVars(as.matrix(Noise_X11))[30:(obs-30)]),
    mean(rowVars(as.matrix(Trend_X11))),
    mean(rowVars(as.matrix(Seasonality_X11))),
    mean(rowVars(as.matrix(Noise_X11))),
    mean(rowMeans(as.matrix(Trend_X11)) - ts_data$trend),
    mean(rowMeans(as.matrix(Seasonality_X11)) - ts_data$seasonality),
    mean(rowMeans(as.matrix(Noise_X11)) - ts_data$noise),
    mean(SQR_X11),
    mean(TJ_X11)
  )
)

print(Ergebnisse)
write.xlsx(Ergebnisse, file = "C:/Jakob/Uni Bamberg/Masterarbeit/Sim Res/Simulation_Ergebnisse_S1.xlsx")


Trend_Plot <- Unsicherheits_Plot(Trend, Trend_X11, ts_data, "trend", "des Trends", "Trend_P")
Saison_Plot <- Unsicherheits_Plot(Seasonality, Seasonality_X11, ts_data, "seasonality", "der Saison-Komponente",
                                  Plot_out = "Season_P")
assign("Trend_Plot", Trend_Plot, envir = .GlobalEnv)
assign("Saison_Plot", Saison_Plot, envir = .GlobalEnv)
}

start_time <- Sys.time() 
Simulation(iterations = 2, obs = 84, frequency = 12, trend_slope = 0.02,
           trend_amplitude = 0, season_amplitude1 = 0.5,
           season_amplitude2 = 0.5, noise_sd = 1, outlier_prob = 0, 
           outlier_magnitude = 0, shift_prob = 0,
           shift_magnitude = 0, polynomgrad = 1,AR = 0, MA = 0,
           trend_offset = 0, trend_konj = 0)
end_time <- Sys.time()  
time_taken <- end_time - start_time
print(time_taken)

# Alter Kram -----
Summe_quadrierter_Revisisonen(Daten = ts, Bereich_Untergrenze = 52,
                              Bereich_Obergrenze = 62, Methode = x13, frequenz = 12, Output = "SQR_X")

Summe_quadrierter_Revisisonen(Daten = ts, Bereich_Untergrenze = 52,
                              Bereich_Obergrenze = 62, Methode = BV4.1, frequenz = 12, Output = "SQR_BV")

Revisions_Plot(Daten = ts_data, Methode = x13, Untergrenze = 55, Obergrenze = 75,
               Sprünge = 4, frequenz = 12, Plot_out = "Rev_Plot_X11")


Revisions_Plot(Daten = ts_data, Methode = BV4.1, Untergrenze = 55, Obergrenze = 75,
               Sprünge = 4, frequenz = 12, Plot_out = "Rev_Plot_BV")

Treffsicherheit_j(Daten = Beschäftigte, Bereich_Untergrenze = 62, Bereich_Obergrenze = 73, Methode = X13ARIMA,
                  Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
                  Anteil = 1, Verschärfung = 0.5, Output = "Treff_jx13")

Treffsicherheit_g(Daten = Beschäftigte, Bereich_Untergrenze = 62, Bereich_Obergrenze = 73, Methode = X13ARIMA,
                  Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
                  Anteil = 1,Verschärfung = 0.5, Output = "Treff_gx13")

Revisions_Plot(Daten = Beschäftigte, Methode = X13ARIMA, Untergrenze = 73, Obergrenze = 84, Sprünge = 4,
               Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend",
               Frequenz = 12)
