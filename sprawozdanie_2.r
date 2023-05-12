library(dplyr)
library(vcdExtra)
library(DescTools)


# ZAD 1
df <- read.csv('personel.csv', header = FALSE, sep = ';')
colnames(df) <- c('D', 'S', 'A1', 'A2', 'W1', 'W2', 'P', 'Wiek', 'Wyk')
df
alpha <- 0.05

D <- df$D
S <- df$S
A1 <- df$A1
A2 <- df$A2
W1 <- df$W1
W2 <- df$W2
P <- df$P
Wiek <- df$Wiek
Wyk <- df$Wyk


# zad 2
x2 <- structable(P ~ S, df) %>% matrix(nrow = 2, ncol = 2)
test_zad2 <- fisher.test(x2, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad2
test_zad2$p.value


# zad 3

##a
x3a <- structable(S ~ Wiek, df) %>% matrix(nrow = 4, ncol = 2)
test_zad3a <- fisher.test(x3a, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad3a
##b
x3b <- structable(S ~ Wyk, df) %>% matrix(nrow = 3, ncol = 2)
test_zad3b <- fisher.test(x3b, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad3b


# zad 4

##a
x4a <- structable(W1 ~ S, df) %>% matrix(nrow = 2, ncol = 4)
test_zad4a <- fisher.test(x4a, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4a
##b
x4b <- structable(W1 ~ Wyk, df) %>% matrix(nrow = 3, ncol = 4)
test_zad4b <- fisher.test(x4b, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4b
##c
x4c <- structable(W1 ~ P, df) %>% matrix(nrow = 2, ncol = 4)
test_zad4c <- fisher.test(x4c, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4c
##d
x4d <- structable(W1 ~ Wiek, df) %>% matrix(nrow = 4, ncol = 4)
test_zad4d <- fisher.test(x4d, hybrid = TRUE, 
                          conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4d


###########

# zad 5

# Zapozna? si? z funkcj? 'chisq.test
# oraz z funkcj? 'assocstats' 
# biblioteki vcd.


# zad 6
alpha <- 0.01
x6 <- structable(W1 ~ S, df) %>% matrix(nrow = 2, ncol = 4)
### test chi-kwadrat Pearsona
test_zad6_1 <- chisq.test(x6, rescale.p = TRUE, simulate.p.value = TRUE)
### test chi-kwadrat ilorazu wiarogodno?ci
test_zad6_2 <- assocstats(x6)


# zad 7 (multinorm)

power7 <- function(p, MC = 1000){
  n <- c(50, 100, 1000)
  
  fish_n <- c()
  chi_n <- c()
  like_n <- c()
  for(j in n){
    fish_i <- c()
    chi_i <- c()
    like_i <- c()
    
    for(i in 1:MC){
      bool <- TRUE
      while(bool){
        m <- matrix(rmultinom(1, j, p), nrow = 2)
        x2 <- chisq.test(m)$p.value
        if(is.nan(x2) == FALSE){
          bool <- FALSE
        }
      }
      x1 <- fisher.test(m)$p.value < 0.05
      x2 <- chisq.test(m)$p.value < 0.05
      x3 <- assocstats(m)[2]$chisq_tests[5] < 0.05
      fish_i <- append(fish_i, x1)
      chi_i <- append(chi_i, x2)
      like_i <- append(like_i, x3)
    }
    
    size_fish <- sum(fish_i[fish_i ==TRUE]) / MC
    size_chi <- sum(chi_i[chi_i == TRUE]) / MC
    size_like <- sum(like_i[like_i == TRUE]) / MC
    
    fish_n <- append(fish_n, size_fish)
    chi_n <- append(chi_n, size_chi)
    like_n <- append(like_n, size_like)
  }
  df <- data.frame(fish_n, chi_n, like_n, row.names = c(50, 100, 1000))
  return(df)
}

##a
p_a <- c(1/20, 9/20, 1/20, 9/20)
power7(p_a)

##b
p_b <- c(1/40, 19/40, 3/40, 17/40)
power7(p_b)

# zad 8

# W każdej z poniższych funkcji argument tbl odpowiada za tabelę liczności
#WAŻNE -> w zadaniu 8 miar zmiennych porządkowych używamy tylko w podpunkcie b


# ZMIENNE NOMINALNE

manual_gk_tau <- function(tbl){ #Goodman-Kruskal tau
  n <- sum(tbl)
  rows <- dim(tbl)[1]
  cols <- dim(tbl)[2]
  tau1 <- 0
  for(row in 1:rows){
    n_i_ <- sum(tbl[row,])
    n_ij2 <- sum(tbl[row,]^2)
    tau1 <- tau1 + n_ij2 / (n*n_i_)
  }
  
  tau2 <- 0
  for(col in 1:cols){
    n__j <- sum(tbl[,col])
    tau2 <- tau2 + sum((n__j / n)^2)
    
    tau <- (tau1 - tau2) / (1 - tau2)
  }
  return(tau)
}

manual_vc <- function(tbl){ #V-Crammer
  n <- sum(tbl)
  rows <- dim(tbl)[1]
  cols <- dim(tbl)[2]
  X <- chisq.test(tbl)$statistic
  v <- sqrt(X / (n*min(rows - 1, cols - 1)))
  return(v)
}

manual_tc <- function(tbl){ #T-Czurpova
  n <- sum(tbl)
  rows <- dim(tbl)[1]
  cols <- dim(tbl)[2]
  X <- chisq.test(tbl)$statistic
  t <- sqrt(X / (n*sqrt((rows - 1)*(cols - 1))))
  return(t)
}

manual_phi <- function(tbl){ #phi
  n <- sum(tbl)
  X <- chisq.test(tbl)$statistic
  phi <- sqrt(X/n)
  return(phi)
}

# ZMIENNE PORZĄDKOWE

manual_gk_gamma <- function(tbl){ #Goodman-Kruskal gamma
  params <- ConDisPairs(tbl)
  C <- params$C
  D <- params$D
  gamma <- (C - D) / (C + D)
  return(gamma)
}

manual_kendall_tau_b <- function(tbl){ #tau-b Kendall
  params <- ConDisPairs(tbl)
  C <- params$C
  D <- params$D
  n <- sum(tbl)
  rows <- dim(tbl)[1]
  T1 <- 0
  for(row in 1:rows){
    ni <- sum(tbl[row,])
    T1 <- T1 + ni*(ni - 1) / 2
  }
  cols <- dim(tbl)[2]
  T2 <- 0
  for(col in 1:cols){
    nj <- sum(tbl[,col])
    T2 <- T2 + nj*(nj - 1) / 2
  }
  tau <- (C - D) / sqrt((n*(n - 1)/2 - T1)*(n*(n - 1)/2 - T2))
  return(tau)
}

manual_sommers_d <- function(tbl){ #d Sommersa
  params <- ConDisPairs(tbl)
  C <- params$C
  D <- params$D
  n <- sum(tbl)
  rows <- dim(tbl)[1]
  T1 <- 0
  for(row in 1:rows){
    ni <- sum(tbl[row,])
    T1 <- T1 + ni*(ni - 1) / 2
  }
  d <- (C - D) / (n*(n - 1)/2 - T1)
  return(d)
}


tt <- GoodmanKruskalTau(x4a, direction = 'column')

