library(dplyr)
library(vcdExtra)

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

##a


##b


# zad 8

gk_manual <- function(df, X1, X2){
  X1 <- data.frame(as.factor(df$X1))
  X2 <- data.frame(as.factor(df$X2))
  tbl <- structable(X1 ~ X2, df)
  rows <- dim(tbl)[1]
  cols <- dim(tbl)[2]
  tbl <- matrix(nrow = rows, ncol = cols)
  n <- sum(tbl)
  
  tau1 <- 0
  for(row in 1:rows){
    n_i_ <- sum(tbl[row,])
    n_ij2 <- sum(tbl[row,]^2)
    tau1 <- tau1 + n_ij2 / n*n_i_
  }
  
  tau2 <- 0
  for(col in 1:cols){
    n__j <- sum(tbl[,col])
    tau2 <- tau2 + sum((n__j / n)^2)
    
    tau <- (tau1 - tau2) / (1 - tau2)
  }
  
  return(tbl)
}