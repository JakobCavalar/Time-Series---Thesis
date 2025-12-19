# Time-Series---Thesis


This Repository is storing the Code I have used to create simulations plots and other stuff for my master´s thesis: Ein Vergleich der Verfahren X-13-ARIMA-SEATS und BV4.1 zur Analyse von Zeitreihen.

The Code was just copied and pasted into this repository, so dont expect everything to run without making minor adjustments. Especially to the simulations itself since it has been done on a univerity server and therefore needed to be adapted for this. Therefore I have included a file that I did not use but does the same simulation on your local device.

The Script: VerbraucherpreisindexDE is using BV4.1 and X-13-ARIMA to decompose the german consumer price index into trend, seasonal and random components

The Scripts: ARIMA, LWR, Polynomgrade Plot, Signalextraktion and Bandbreite just show the different Methods using changing parameterization to estimate the Trend-component

The Script - Einlesen BV4.1 helps if you work with the software programm BV4.1 and not the deseats package in R. It can automatically extract results of the BV4.1 xlsx result files

The Script - Berechnen von Hendersonweights helps you calculate the Henderson weights for a Henderson-filter with given Bandwith q

The Script - Server Ergebnisse abrufen helped me extract the results from our university server

The Script - Sim BV und X11 contains most of the important function such as the data generating process, the simulation function an some quality measures. Some of the functions are in old versions.

The Script - Maßgrößen Verlaufsplot plots the chnage of the Qualitymeasures over different scenarios

Lastly, the Script - Server Code has the most recent version of the important functions and starts the simulation on the universities server. Below the parameterization of all the 9 scenarios can be found.
