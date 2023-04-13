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
test_zad2 <- fisher.test(P, S, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad2
test_zad2$p.value

# zad 3

##a
test_zad3a <- fisher.test(S, Wiek, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad3a
##b
test_zad3b <- fisher.test(S, Wyk, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad3b

# zad 4

##a
test_zad4a <- fisher.test(W1, S, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4a
##b
test_zad4b <- fisher.test(W1, Wyk, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4b
##c
test_zad4c <- fisher.test(W1, P, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4c
##d
test_zad4d <- fisher.test(W1, Wiek, conf.level = 1 - alpha, simulate.p.value = TRUE)
test_zad4d

TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")