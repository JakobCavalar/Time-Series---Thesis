install.packages("MASS")
library(MASS)

# q = 4 für 9-Term-Henderson-Filter ...
q <- 4
j_vals <- -q:q

# Schritt 1: g_j
gj <- function(j, q) {
  ( (q+1)^2 - j^2 ) * ( (q+2)^2 - j^2 ) * ( (q+3)^2 - j^2 )
}

g <- sapply(j_vals, gj, q = q)

# Schritt 2: Summen 
gj2 <- g * j_vals^2
gj4 <- g * j_vals^4

sum_g  <- sum(g)
sum_gj2 <- sum(gj2)
sum_gj4 <- sum(gj4)

# Löse das Gleichungssystem:
# a * sum_g + b * sum_gj2 = 1
# a * sum_gj2 + b * sum_gj4 = 0

# Cramer's Rule:
D  <- sum_g * sum_gj4 - sum_gj2^2
Da <- 1 * sum_gj4 - sum_gj2 * 0
Db <- sum_g * 0 - sum_gj2 * 1

a <- Da / D
b <- Db / D

# Schritt 3: h_j = g_j * (a + b * j^2)
h <- g * (a + b * j_vals^2)

# Filtergewichte als Brüche
cat("Henderson-Gewichte (h_j) als Brüche:\n")
for (i in 1:length(j_vals)) {
  cat(sprintf("h_%d = %s\n", j_vals[i], fractions(h[i])))
}

# Bedingungen prüfen
if(sum(h) == 1){
  print("Bedingung 1 Erfüllt")
}

if(sum(h*j_vals) == 0){
  print("Bedingung 2 Erfüllt")
}

if(round(sum(h*(j_vals^2)), 15) == 0){
  print("Bedingung 3 Erfüllt")
}
