library(openxlsx)

Anzahl_MC_real <- 5000

Anzahl_MC <- Anzahl_MC_real -1

sample_MC <- lapply(0:Anzahl_MC, function(i) {
  
  filename <- sprintf("simulation_%03d.rds", i)
  
  filepath <- file.path("C:/Jakob/Uni Bamberg/Masterarbeit/Zeitreihen Einarbeitung/R - Simulation/Results Server S7", filename)
  
  if (file.exists(filepath)) {
    
    Sim <- readRDS(filepath)
    
    Sim$id <- i  +1
    
    return(Sim)
    
  } else {
    
    message(sprintf("Datei fehlt: %s", filepath))
    
    return(NULL)
    
  }
  
})


vector_names <- c("Noise_real", "Trend", "Seasonality", "Noise",
                  "Trend_X11", "Seasonality_X11", "Noise_X11")

scalar_names <- c("SQR", "TJ", "SQR_X11", "TJ_X11")

n_sim <- length(sample_MC)

vector_results <- list()   
scalar_results <- list()   

for (v in vector_names) {
  
  mat <- do.call(cbind, lapply(sample_MC, function(sim) {
    as.numeric(sim[[v]])
  }))
  
  vector_results[[v]] <- as.data.frame(mat)
  
  colnames(vector_results[[v]]) <- paste0("sim_", seq_len(n_sim))
}

for (s in scalar_names) {
  
  scalar_results[[s]] <- sapply(sample_MC, function(sim) sim[[s]])
}

#vector_results  
#scalar_results   

Noise_real <- vector_results[["Noise_real"]]
Trend <- vector_results[["Trend"]]
Seasonality <- vector_results[["Seasonality"]]
Noise_BV <- vector_results[["Noise"]]
Trend_X11 <- vector_results[["Trend_X11"]]
Seasonality_X11 <- vector_results[["Seasonality_X11"]]
Noise_X11 <- vector_results[["Noise_X11"]]
SQR <- scalar_results[["SQR"]]
TJ <- scalar_results[["TJ"]]
SQR_X11 <- scalar_results[["SQR_X11"]]
TJ_X11 <- scalar_results[["TJ_X11"]]

# bei Fehlern
sort(TJ, decreasing = T)
sort(TJ_X11, decreasing = T)
# TJ <- ifelse(TJ >= 1, median(TJ), TJ)
# TJ_X11 <- ifelse(TJ_X11 >= 1, median(TJ_X11), TJ_X11)

############################################################################
warning("Hier die Zeitreihe aus dem jeweiligen Szenario !!!!!!!!!!!!!!!!!!")
############################################################################
obs <- 84
ts_data <- simulate_time_series(n = obs, 
                                trend_slope = 0.1,
                                trend_det = 1, saison_det = 1, saison = 0,
                                season_period = 12, season_amplitude1 = 1, 
                                season_amplitude2 = 1, noise_sd = 1, 
                                outlier_prob = 0, outlier_magnitude = 0,
                                shift_prob = 0, shift_magnitude = 0,
                                seed = 1, polynomgrad = 1 ,AR = c(0.4,0.2), MA = -0.1,
                                trend_offset = 0, trend_konj = 0, Noise = "GARCH")

### Quality Measures
Trend_real <- matrix(ts_data$trend, nrow = length(ts_data$trend), ncol = dim(Trend)[2])
Trend_BV4.1 <- as.matrix(Trend)
Trend_X11 <- as.matrix(Trend_X11)
Seasonality_real <- matrix(ts_data$seasonality, nrow = length(ts_data$seasonality),
                           ncol = dim(Seasonality)[2])
Seasonality_BV4.1 <- as.matrix(Seasonality)
Seasonality_X11 <- as.matrix(Seasonality_X11)
Noise_real <- as.matrix(Noise_real)
Noise_BV4.1 <- as.matrix(Noise_BV)
Noise_X11 <- as.matrix(Noise_X11)

Trend_BV4.1_res <- QualityMeasure(True_mean = Trend_real, Est_mean = Trend_BV4.1, MSETF = F)
Trend_X11_res <- QualityMeasure(True_mean = Trend_real, Est_mean = Trend_X11, MSETF = F)
Seasonality_BV4.1_res <- QualityMeasure(True_mean = Seasonality_real, Est_mean = Seasonality_BV4.1, MSETF = F) 
Seasonality_X11_res <- QualityMeasure(True_mean = Seasonality_real, Est_mean = Seasonality_X11, MSETF = F) 
Noise_BV4.1_res <- QualityMeasure(True_mean = Noise_real, Est_mean = Noise_BV4.1, MSETF = F)
Noise_X11_res <- QualityMeasure(True_mean = Noise_real, Est_mean = Noise_X11, MSETF = F)



Ergebnisse <- data.frame(
  Kennzahl = c(
    "RMSE Trend BV4.1", "RMSE Saison BV4.1", "RMSE Fehler BV4.1",
    "Var Trend BV4.1 (Mitte)", "Var Saison BV4.1 (Mitte)", "Var Fehler BV4.1 (Mitte)",
    "Var Trend BV4.1 (gesamt)", "Var Saison BV4.1 (gesamt)", "Var Fehler BV4.1 (gesamt)",
    "Bias Trend BV4.1", "Bias Saison BV4.1", "Bias Noise BV4.1", "SQR BV4.1", "Treffsicherheit BV4.1 (median)",
    "Treffsicherheit BV4.1 (mean)",
    
    "RMSE Trend X11", "RMSE Saison X11", "RMSE Fehler X11",
    "Var Trend X11 (Mitte)", "Var Saison X11 (Mitte)", "Var Fehler X11 (Mitte)",
    "Var Trend X11 (gesamt)", "Var Saison X11 (gesamt)", "Var Fehler X11 (gesamt)",
    "Bias Trend X11", "Bias Saison X11", "Bias Noise X11", "SQR X11", "Treffsicherheit X11 (median)", 
    "Treffsicherheit X11 (mean)"
  ),
  
  Wert = c(
    mean(Trend_BV4.1_res[["RMSE"]]),
    mean(Seasonality_BV4.1_res[["RMSE"]]),
    mean(Noise_BV4.1_res[["RMSE"]]),
    mean(rowVars(as.matrix(Trend))[30:(obs-30)]),
    mean(rowVars(as.matrix(Seasonality))[30:(obs-30)]),
    mean(rowVars(as.matrix(Noise))[30:(obs-30)]),
    mean(rowVars(Trend_BV4.1)),
    mean(rowVars(Seasonality_BV4.1)),
    mean(rowVars(Noise_BV4.1)),
    mean(Trend_BV4.1_res[["Bias"]]),
    mean(Seasonality_BV4.1_res[["Bias"]]),
    mean(Noise_BV4.1_res[["Bias"]]),
    mean(SQR),
    median(TJ),
    mean(TJ),
    
    mean(Trend_X11_res[["RMSE"]]),
    mean(Seasonality_X11_res[["RMSE"]]),
    mean(Noise_X11_res[["RMSE"]]),
    mean(rowVars(as.matrix(Trend_X11))[30:(obs-30)]),
    mean(rowVars(as.matrix(Seasonality_X11))[30:(obs-30)]),
    mean(rowVars(as.matrix(Noise_X11))[30:(obs-30)]),
    mean(rowVars(Trend_X11)),
    mean(rowVars(Seasonality_X11)),
    mean(rowVars(Noise_X11)),
    mean(Trend_X11_res[["Bias"]]),
    mean(Seasonality_X11_res[["Bias"]]),
    mean(Noise_X11_res[["Bias"]]),
    mean(SQR_X11),
    median(TJ_X11),
    mean(TJ_X11)
  )
)

print(Ergebnisse)
#########################################################
warning("aktuelles Szenario eintragen !!!!!!!!!!!!!!!!!!")
#########################################################
write.xlsx(Ergebnisse, file = "C:/Jakob/Uni Bamberg/Masterarbeit/Zeitreihen Einarbeitung/R - Simulation/Sim Res/Simulation_Ergebnisse_S7_long.xlsx")  

Unsicherheits_Plot(Trend_BV4.1, Trend_X11, ts_data, "trend", "des Trends", "Trend_P")
Unsicherheits_Plot(Seasonality_BV4.1, Seasonality_X11, ts_data, "seasonality", "der Saison-Komponente",
                                  Plot_out = "Season_P")

