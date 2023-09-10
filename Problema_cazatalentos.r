set.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}

# Cazatalentos 1 : 24,22% elijo a la mejor con Po:0,80
mejor <- 0.8
peloton <- seq(31, 79, by = 1) / 100
ultima <- 0.30
jugadoras <- c(mejor, peloton, peloton, ultima)
length(jugadoras)

primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 100) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(mejor * (1 - mejor) / 100)

# ---------------------------------------------------
# Cazatalentos 2 : 10,31% elijo a la mejor con Po:0,80
mejor <- 0.8
peloton_1 <- rep(79, 6) / 100
peloton_2 <- rep(78, 5) / 100
peloton_3 <- rep(77, 4) / 100
peloton_4 <- rep(76, 3) / 100
peloton_5 <- rep(75, 2) / 100
peloton_6 <- rep(74, 1) / 100
peloton_7 <- rep(73, 178) / 100
jugadoras <- c(mejor, peloton_1, peloton_2, peloton_3,
               peloton_4, peloton_5, peloton_6, peloton_7)
length(jugadoras)

primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 100) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(mejor * (1 - mejor) / 100)

# ---------------------------------------------------
# Cazatalentos 3 : 82,24% elijo a la mejor con Po:0,80
mejor <- 0.8
peor <- 0.75
jugadoras <- c(mejor, peor)
length(jugadoras)

primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 100) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(mejor * (1 - mejor) / 100)

# ---------------------------------------------------
# Cazatalentos 4 : 34,77% elijo a la mejor con Po:0,80
mejor <- 0.9
peloton_1 <- rep(80, 20) / 100
peloton_2 <- rep(79, 20) / 100
peloton_3 <- rep(78, 20) / 100
peloton_4 <- rep(77, 20) / 100
peloton_5 <- rep(76, 19) / 100
jugadoras <- c(mejor, peloton_1, peloton_2, peloton_3,
               peloton_4, peloton_5)

length(jugadoras)


primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 10) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(mejor * (1 - mejor) / 10)

# ---------------------------------------------------
# Cazatalentos 5 : 5,72% elijo a la jugadora A con Po:0,74

jugadora_a <- (85 + 69 + 70) / 300 # 74,67%
jugadora_b <- (84 + 74 + 76) / 300
jugadora_c <- (84 + 74 + 75) / 300
jugadora_d <- (82 + 70 + 73) / 300
jugadora_e <- (81 + 75 + 74) / 300

jugadoras <- c(jugadora_a, jugadora_b, jugadora_c,
               jugadora_d, jugadora_e)

length(jugadoras)


primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 300) # 300 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(jugadora_a * (1 - jugadora_a) / 300)

# ---------------------------------------------------
# Cazatalentos 6 : Con 100 tiros elijo 1 jugadora con Po:0,80

# ---------------------------------------------------
# Cazatalentos 7 : 40,84% elijo a la mejor con Po:0,80 ?
# Supongo que la mejor en la primera ronda obtuvo 80 aciertos
jugadora_a <- (80 + 80) / 200
jugadora_b <- (79 + 79) / 200
jugadora_c <- (79 + 78) / 200
jugadora_d <- (79 + 77) / 200
jugadora_e <- (79 + 72) / 200

jugadoras <- c(jugadora_a, jugadora_b, jugadora_c,
               jugadora_d, jugadora_e)

length(jugadoras)

primera_ganadora <- 0
n_repeticiones <- 10000
for (i in 1:n_repeticiones) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 200)

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print(primera_ganadora / n_repeticiones * 100)
mejor_se <- sqrt(jugadora_a * (1 - jugadora_a) / 300)

# ---------------------------------------------------
# Cazatalentos 8 : Con 1085 tiros elijo 1 jugadora con P:0,80
jugadora_a <- (85 + 790) / 1085
mejor_se <- sqrt(jugadora_a * (1 - jugadora_a) / 1085)


# ---------------------------------------------------
# Cazatalentos 9 : Con 1000 tiros elijo 1 jugadora con P:0,70

jugadora_a <- (68 + 74 + 78 + 70 + 68 + 63 + 80 + 68 + 67 + 65) / 1000
mejor_se <- sqrt(jugadora_a * (1 - jugadora_a) / 1000)




# Cómo las ordenaria??
# Tengo dos fuentes de incertidumbre: 
#    1. el problema de las multiples comparaciones
#    2. la incertidumbre sobre la verdadera proporción de enceste.

# Sobre el problema 1)
# Prob de elegir a la mejor:
# Czt_1: 24.06%
# Czt_2: 10.29%
# Czt_3: 82.23%
# Czt_4: 34.09% Asumo 20 de 0.8, 20 de 0.79, etc...
# Czt_5: 5.59% La elegida no seria la mejor en los 300 tiros
# Czt_6: 100%
# Czt_7: 40,84% Asumo que en la primera ronda la ganadora encesto 80, el resto 79

# Sobre el problema 2)
# Czt_1: p:0.8 se: 0.03     #100 jugadoras,  100 tiros
# Czt_2: p:0.8 se: 0.03     #200 jugadoras,  100 tiros
# Czt_3: p:0.8 se: 0.03     #  2 jugadoras,  100 tiros
# Czt_4: p:0.8 se: 0.09     #100 jugadoras,   10 tiros
# Czt_5: p:0.747 se: 0.025  #  5 jugadoras,  300 tiros (Pero elijo la mejor sobre los primeros 100)
# Czt_6: p:0.8 se: 0.03     #  1 jugadoras,  100 tiros
# Czt_7: p:0.8 se: 0.03     #  5 jugadoras,  200 tiros (Conozco 2da ronda. Se q ganó ambas rondas)
# Czt_8: p:0.81 se: 0.01    #  1 jugadoras, 1085 tiros
# Czt_9: p:0.70 se: 0.014   #  1 jugadoras, 1000 tiros

# Czt_8 > (Czt_7 > Czt_6 > Czt_3 > Czt_1 > Czt_2) > Czt_4 > Czt_5 > Czt_9
