.libPaths(c("/mnt/data/libs/ba4sv7/4.4"))
args <- commandArgs(trailingOnly = TRUE)
process_id <- as.numeric(args[1])
set.seed(process_id)
outpath <- sprintf("results/simulation_%03d", process_id)

library(deseats)
library(ggplot2)
library(RJDemetra)
library(seasonal)
library(matrixStats)
library(dplyr)
library(rugarch)


# Funktionen - vorher durchlaufen lassen ------------
Summe_quadrierter_Revisisonen <- function(ts, Bereich_Untergrenze, Bereich_Obergrenze,
                                          Methode = x13, frequenz, Output = "SQR"){
  
  if (Bereich_Untergrenze < 52) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht groesser als die Obergrenze sein.")
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

simulate_time_series <- function(n = 200, 
                                 trend_slope = 0.15,
                                 trend_det = 1, saison_det = 1, saison = 0,
                                 season_period = 12, season_amplitude1 = 5, 
                                 season_amplitude2 = 5, noise_sd = 0.5, 
                                 outlier_prob = 0.012, outlier_magnitude = 12,
                                 shift_prob = 0.008, shift_magnitude = 5,
                                 seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                                 trend_offset = 0, trend_konj = 0, Noise = 0){
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
    trend[(n/2):n] <- trend[(n/2):n] - trend_offset
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
    amps <- c(season_amplitude1 * 0.5437707, season_amplitude2 * 0.6514111,
              0.7249299, 0.9828433, 0.3857289,
              0.3926493, 0.8374689, 0.8653374, 0.2908062, 0.9706496, 0.3178583, 0.3149016)
    phases <- c(5.8133911, 3.1857986, 0.9729576, 2.1884463, 4.1457778, 1.9589236,
                2.2090009, 0.9289420, 4.1398501, 1.1628289, 5.9965347, 5.6413485)
    seasonality <- sapply(1:saison, function(k)
      amps[k] * sin(2 * pi * k * time / season_period + phases[k])
    ) |> rowSums()
    
    seasonality <- seasonality - mean(seasonality)
  }
  
  if (saison_det == 1) {
    seasonality <- seasonality
  } else {
    seasonality <- seasonality + arima.sim(list(ar = 0.3), n = n, sd = 0.3)
  }
  
  # 3. Stochastische Komponente
  #print("AR and MA must be either number (or vector with numbers adding to) between 0,1")
  if (Noise == "GARCH") {
    spec_e <- ugarchspec(
      variance.model = list(
        model = "sGARCH",
        garchOrder = c(1, 2)
      ),
      mean.model = list(
        armaOrder = c(0, 0),
        include.mean = FALSE
      ),
      fixed.pars = list(
        omega = 0.1,
        alpha1 = 0.4,
        beta1 = 0.4,
        beta2 = 0.1
      )
    )
    garch_path <- ugarchpath(spec_e, n.sim = n)
    eps <- fitted(garch_path)
    noise <- arima.sim(list(ar = AR, ma = MA), n = n, innov = eps)
  } else if (Noise == "ARMA") {
    noise <- arima.sim(list(ar = AR, ma = MA), n, innov = rnorm(n, mean = 0, sd =  noise_sd))
  } else {
    noise <- rnorm(n, mean = 0, sd =  noise_sd)
  }
  
  
  # 4. Ausreisser
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
                             frequenz = 12,  Verschaerfung = 0.5, Output = "TJ"){
  
  
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
  Entwicklung_Schaetzung_J <- numeric(length(Reihe1))
  S <- numeric(length(Reihe1))
  
  ts <- ts(Daten[,"X_t"], frequency = frequenz)
  
  for (i in Reihe1){
    
    idx <- i - (Reihe1[1] - 1)
    if (Methoden_Name == "x13") {
      Modell_Treff <- Methode(window(ts, end = c(1 + (i - 1) %/% frequenz, (i - 1) %% frequenz + 1)), spec = "RSA0")
      Entwicklung_Schaetzung_J[idx] <- Modell_Treff[["final"]][["series"]][,"t"][i] -
        Modell_Treff[["final"]][["series"]][,"t"][i-1]
    } else {
      Modell_Treff <- Methode(window(ts, end = c(1 + (i - 1) %/% frequenz, (i - 1) %% frequenz + 1)), type = type)
      Entwicklung_Schaetzung_J[idx] <- Modell_Treff@decomp[,"Trend"][i] -
        Modell_Treff@decomp[,"Trend"][i-1]
    }
    Entwicklung_Wahr[idx] <- Daten[["X_t"]][i] - Daten[["X_t"]][i-1]
    
    if ((Entwicklung_Wahr[idx] >= 0 & Entwicklung_Schaetzung_J[idx] >= 0) |
        (Entwicklung_Wahr[idx] < 0 & Entwicklung_Schaetzung_J[idx] < 0)){
      S[idx] <- 0.05 * mean(Daten[["X_t"]])
    } else {
      S[idx] <- (0.05 * mean(Daten[["X_t"]])) * (1 - Verschaerfung *
                                                   ((1 - ((Entwicklung_Schaetzung_J[idx] - Entwicklung_Wahr[idx]) /
                                                            (0.05 * mean(Daten[["X_t"]])))^2)^2))
    }
  }
  Trend_Konjunktur_Entwicklung_J <- (1-((Entwicklung_Schaetzung_J-Entwicklung_Wahr)/S)^2)^2
  #Index <- seq(1:length(Entwicklung_Schaetzung_J))
  #Frame1 <- data.frame(Index = Index, TKE = Trend_Konjunktur_Entwicklung_J)
  #Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
  #Beste_Schaetzung_Konjunktur_Entwicklung_J<- max(Frame1$TKE)
  Treffsicherheit_J <- mean(Trend_Konjunktur_Entwicklung_J)
  assign(Output, Treffsicherheit_J, envir = .GlobalEnv)
}

Simulation <- function(obs = 84, frequency = 12, trend_slope = 0.1,
                       trend_det = 1, saison_det = 1, saison = 0,
                       season_period = 12, season_amplitude1 = 1, 
                       season_amplitude2 = 1, noise_sd = 1, 
                       outlier_prob = 0, outlier_magnitude = 0,
                       shift_prob = 0, shift_magnitude = 0,
                       seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                       trend_offset = 0, trend_konj = 0, Noise = 0){
  
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
  
  ts_data <-  simulate_time_series(n = obs, trend_slope = trend_slope, trend_det = trend_det,
                                   saison_det = saison_det, saison = saison,
                                   season_period = frequency, season_amplitude1 = season_amplitude1,
                                   season_amplitude2 = season_amplitude2,
                                   noise_sd = noise_sd, outlier_prob = outlier_prob, 
                                   outlier_magnitude = outlier_magnitude, shift_prob = shift_prob,
                                   shift_magnitude = shift_magnitude, seed = process_id,
                                   polynomgrad = polynomgrad, AR = AR, MA = MA,
                                   trend_offset = trend_offset, trend_konj = trend_konj, Noise = Noise)
  ts <- ts(ts_data[,"X_t"], frequency = frequenz)
  
  bv4.1 <- BV4.1(ts, type = type)
  Trend <- bv4.1@decomp[,"Trend"]
  Seasonality <- bv4.1@decomp[,"Seasonality"]
  Noise <- bv4.1@decomp[,"Rest"]
  Summe_quadrierter_Revisisonen(ts = ts, Bereich_Untergrenze = (obs - 30),
                                Bereich_Obergrenze = obs, Methode = BV4.1,
                                frequenz = frequency, Output = "SQR_BV")
  SQR <- SQR_BV
  Treffsicherheit_j(Daten = ts_data, Bereich_Untergrenze = (obs - 30),
                    Bereich_Obergrenze = obs, Methode = BV4.1,
                    frequenz = frequency,  Verschaerfung = 0.5, Output = "TJ1")
  TJ <- TJ1
  
  X11 <- x13(ts, spec = "RSA0")
  Trend_X11 <- X11[["final"]][["series"]][,"t"]
  Seasonality_X11 <- X11[["final"]][["series"]][,"s"]
  Noise_X11 <- X11[["final"]][["series"]][,"i"]
  Summe_quadrierter_Revisisonen(ts = ts, Bereich_Untergrenze = (obs - 30),
                                Bereich_Obergrenze = obs, Methode = x13,
                                frequenz = frequency, Output = "SQR_X")
  SQR_X11 <- SQR_X
  Treffsicherheit_j(Daten = ts_data, Bereich_Untergrenze = (obs - 30),
                    Bereich_Obergrenze = obs, Methode = x13,
                    frequenz = frequency,  Verschaerfung = 0.5, Output = "TJX")
  TJ_X11 <- TJX
  
  return(list(Trend = Trend, Seasonality = Seasonality, Noise = Noise,
              SQR = SQR, TJ = TJ,
              Trend_X11 = Trend_X11, Seasonality_X11 = Seasonality_X11,
              Noise_X11 = Noise_X11, SQR_X11 = SQR_X11, TJ_X11 = TJ_X11))
  
} 

# Simulation ---------
res <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                  trend_det = 1, saison_det = 1, saison = 0,
                  season_period = 12, season_amplitude1 = 1, 
                  season_amplitude2 = 1, noise_sd = 1, 
                  outlier_prob = 0, outlier_magnitude = 0,
                  shift_prob = 0, shift_magnitude = 0,
                  seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                  trend_offset = 0, trend_konj = 0, Noise = 0)


saveRDS(res, paste0(outpath, ".rds"))

#______________________________________________________________________________#
#______________________________________________________________________________#
# 9 Szenarios
S1 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 0,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                 trend_offset = 0, trend_konj = 0, Noise = 0)

S2 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 6,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                 trend_offset = 0, trend_konj = 1, Noise = 0)
S3 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 12,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = 0, MA = 0,
                 trend_offset = 4, trend_konj = 0, Noise = 0)
S4 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 0,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 0, trend_konj = 0, Noise = "ARMA")
S5 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 6,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 0, trend_konj = 1, Noise = "ARMA")
S6 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 12,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 4, trend_konj = 0, Noise = "ARMA")
S7 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 0,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 0, trend_konj = 0, Noise = "GARCH")
S8 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 6,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 0, trend_konj = 1, Noise = "GARCH")
S9 <- Simulation(obs = 84, frequency = 12, trend_slope = 0.1,
                 trend_det = 1, saison_det = 1, saison = 12,
                 season_period = 12, season_amplitude1 = 1, 
                 season_amplitude2 = 1, noise_sd = 1, 
                 outlier_prob = 0, outlier_magnitude = 0,
                 shift_prob = 0, shift_magnitude = 0,
                 seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                 trend_offset = 4, trend_konj = 0, Noise = "GARCH")




