##CZĘŚĆ 1##

library(dplyr)
library(vcdExtra)

# ZAD 1
df <- read.csv('personel.csv', header = FALSE, sep = ';')
colnames(df) <- c('D', 'S', 'A1', 'A2', 'W1', 'W2', 'P', 'Wiek', 'Wyk')
df

### A1
df_A1 <- df %>% group_by(A1) %>% count()
df_D_A1 <- df %>% group_by(D, A1) %>% tally()
df_P_A1 <- df %>% group_by(P, A1) %>% tally()
df_Wiek_A1 <- df %>% group_by(Wiek, A1) %>% tally()
df_Wyk_A1 <- df %>% group_by(Wyk, A1) %>% tally()

### W1
df_D_W1 <- df %>% group_by(D, W1) %>% tally()
df_P_W1 <- df %>% group_by(P, W1) %>% tally()
df_Wiek_W1 <- df %>% group_by(Wiek, W1) %>% tally()
df_Wyk_W1 <- df %>% group_by(Wyk, W1) %>% tally()

# ZAD 2
structable(P ~ W1, df)
structable(S ~ W1, df)
structable(D ~ A1, df)

# ZAD 3
library(ggplot2)

df_W1 <- df %>% group_by(W1) %>% count()
df_W1$perc <- apply(df_W1, 1, FUN = function(x) x[2]/200)
df_W2 <- df %>% group_by(W2) %>% count()
df_W2$perc <- apply(df_W2, 1, FUN = function(x) x[2]/200)

### Wykresy słupkowe
bar_W1 <- ggplot(df, aes(x = factor(W1), fill = factor(W1))) +
  geom_bar(stat = 'count') +
  theme_minimal() +
  labs(title = 'Wykres słupkowy W1', x = 'W1', 
       y = 'Ilość głosów', fill = 'W1')
bar_W1

bar_W2 <- ggplot(df, aes(x = factor(W2), fill = factor(W2))) +
  geom_bar(stat = 'count') +
  theme_minimal() +
  labs(title = 'Wykres słupkowy W2', x = 'W2', 
       y = 'Ilość głosów', fill = 'W2')
bar_W2

### Wykresy kołowe
pie_W1 <- ggplot(df_W1, aes(x = '', y = perc, fill = factor(W1))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  geom_text(aes(x = 1.3, label = paste0(perc, '%')),
            position = position_stack(vjust = 0.5),
            size = 2) +
  labs(fill = 'W1', title = 'Wykres kołowy W1')
pie_W1

pie_W2 <- ggplot(df_W2, aes(x = '', y = perc, fill = factor(W2))) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  geom_text(aes(x = 1.3, label = paste0(perc, '%')),
            position = position_stack(vjust = 0.5),
            size = 2) +
  labs(fill = 'W1', title = 'Wykres kołowy W1')
pie_W2

# ZAD 4
library(ggmosaic)
library(tidyr)

### D i A1
mosaic1 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(D, A1), fill = D)) +
  theme_minimal() +
  labs(title = 'Ocena atmosfery pracy') +
  theme(legend.position = 'none',
        axis.title = element_blank())

mosaic1

### D i W1
mosaic2 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(D, W1), fill = D)) +
  labs(title = 'Zadowolenie z wynagrodzenia') +
  theme(legend.position = 'none', axis.title = element_blank())

mosaic2

### S i P
mosaic3 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(P, S), fill = P)) +
  labs(title = 'Stanowiska kierownicze wśród kobiet i mężczyzn') +
  theme(legend.position = 'none', axis.title = element_blank())

mosaic3

### P i W1
mosaic4 <- ggplot(data = df) +
  geom_mosaic(aes(x = product(P, W1), fill = P)) +
  labs(title = 'Zadowolenie z wynagrodzenia wśród kobiet i mężczyzn') +
  theme(legend.position = 'none', axis.title = element_blank())

mosaic4

##CZĘŚĆ 2##

#ZAD 5
library(stats)
ret <- function(data){ #losowanie ze zwracaniem (proste niezależne)
  smp <- sample(nrow(data), size = 0.1*nrow(data), TRUE)
  return(smp)
}

noret <- function(data){ #losowanie bez zwracania (proste zależne)
  smp <- sample(nrow(data), size = 0.1*nrow(data), FALSE)
  return(smp)
}

#ZAD 6
library(likert)

#fltr - nazwa kolumny (filtrujemy po jakiej podgrupie sprawdzamy
#grp - nazwa podgrupy (np. wśród płci wybieramy tylko kobiety)
#dt - badany okres (wybieramy między A1, a A2)

atmosphere <- function(fltr, grp, dt){
  fltr <- deparse(substitute(fltr))
  grp <- deparse(substitute(grp))
  dt <- deparse(substitute(dt))
  df1 <- filter(df, df[[fltr]] == grp)
  df2 <- data.frame(as.factor(df1[[dt]]))
  colnames(df2) <- ''
  return(df2)
}
##pierwszy okres ankiet - A1
##likert - cała grupa
all_employees <- data.frame(as.factor(df[['A1']]))
colnames(all_employees) <- 'all'
all_employees_likert <- likert(all_employees)
all_employees_summary <- summary(all_employees_likert)
bar_all_employees <- plot(all_employees_likert,
                          type = 'bar')
bar_all_employees
den_all_employees <- likert.density.plot(all_employees_likert, 
                                facet = TRUE, bw = 0.5)
den_all_employees

##likert - kobiety i mężczyźni
women <- atmosphere(P, K, A1)
colnames(women) <- 'women'
men <- atmosphere(P, M, A1)
colnames(men) <- 'men'
both <- merge(men, women, 
           by = 0, all = TRUE)[-1]
both_likert <- likert(both)
both_summary <- summary(both_likert)
bar_both <- plot(both_likert, type = 'bar')
bar_both
den_both <- likert.density.plot(both_likert, 
                                facet = TRUE, bw = 0.5)
den_both

##likert - wszystkie działy
inv <- atmosphere(D, Z, A1)
colnames(inv) <- 'inv'
prod <- atmosphere(D, P, A1)
colnames(prod) <- 'prod'
sales <- atmosphere(D, S, A1)
colnames(sales) <- 'sales'
serv <- atmosphere(D, O, A1)
colnames(serv) <- 'serv'
departments <- merge(inv, prod, by = 0,
                     all = TRUE)[-1]
departments <- merge(departments, sales,
                     by = 0,
                     all = TRUE)[-1]
departments <- merge(departments, serv,
                     by = 0,
                     all = TRUE)[-1]
departments[1:4] <- lapply(departments[1:4], factor,
                      levels = -2:2)
departments_likert <- likert(departments[, c(1:4)])
departments_summary <- summary(departments_likert)
bar_departments <- plot(departments_likert, type = 'bar')
bar_departments
den_departments <- likert.density.plot(departments_likert,
                        facet = TRUE, bw = 0.5)
den_departments


##drugi okres ankiet - A2
##likert - cała grupa
all_employees2 <- data.frame(as.factor(df[['A2']]))
colnames(all_employees2) <- 'all'
all_employees2_likert <- likert(all_employees2)
all_employees2_summary <- summary(all_employees2_likert)
bar_all_employees2 <- plot(all_employees2_likert,
                          type = 'bar')
bar_all_employees2
den_all_employees2 <- likert.density.plot(all_employees2_likert, 
                                         facet = TRUE, bw = 0.5)
den_all_employees2

##likert - kobiety i mężczyźni
women2 <- atmosphere(P, K, A2)
colnames(women2) <- 'women'
men2 <- atmosphere(P, M, A2)
colnames(men2) <- 'men'
both2 <- merge(men2, women2, 
              by = 0, all = TRUE)[-1]
both2_likert <- likert(both2)
both2_summary <- summary(both2_likert)
bar_both2 <- plot(both2_likert, type = 'bar')
bar_both2
den_both2 <- likert.density.plot(both2_likert, 
                                facet = TRUE, bw = 0.5)
den_both2

##likert - wszystkie działy
inv2 <- atmosphere(D, Z, A2)
colnames(inv2) <- 'inv'
prod2 <- atmosphere(D, P, A2)
colnames(prod2) <- 'prod'
sales2 <- atmosphere(D, S, A2)
colnames(sales2) <- 'sales'
serv2 <- atmosphere(D, O, A2)
colnames(serv2) <- 'serv'
departments2 <- merge(inv2, prod2, by = 0,
                     all = TRUE)[-1]
departments2 <- merge(departments2, sales2,
                     by = 0,
                     all = TRUE)[-1]
departments2 <- merge(departments2, serv2,
                     by = 0,
                     all = TRUE)[-1]
departments2[1:4] <- lapply(departments2[1:4], factor,
                           levels = -2:2)
departments2_likert <- likert(departments2[, c(1:4)])
departments2_summary <- summary(departments2_likert)
bar_departments2 <- plot(departments2_likert, type = 'bar')
bar_departments2
den_departments2 <- likert.density.plot(departments2_likert,
                                       facet = TRUE, bw = 0.5)
den_departments2

#ZAD 7
library(binom)
alpha <- 0.05

##Gotowa funkcja z pakietu binom
clopper1 <- function(alpha, poll){
  tbl <- table(poll)
  if((1 %in% poll) == FALSE){
    positive <- tbl[['2']]
  }
  else if((2 %in% poll) == FALSE){
    positive <- tbl[['1']]
  }
  else if(((1 %in% poll) == FALSE) & 
          ((2 %in% poll) == FALSE)){
    positive <- 0
  }
  else{
    positive <- tbl[['1']] + tbl[['2']]
  }
  all <- sum(tbl)
  confint1 <- binom.confint(positive, all,
                            conf.level = 1 - alpha,
                            methods = 'exact')
  interval <- c(confint1$lower, confint1$upper)
  return(interval)
}

##Ręczne policzenie
clopper2 <- function(alpha, poll){
  tbl <- table(poll)
  if((1 %in% poll) == FALSE){
    positive <- tbl[['2']]
  }
  else if((2 %in% poll) == FALSE){
    positive <- tbl[['1']]
  }
  else if(((1 %in% poll) == FALSE) & 
          ((2 %in% poll) == FALSE)){
    positive <- 0
  }
  else{
  positive <- tbl[['1']] + tbl[['2']]
  }
  all <- sum(tbl)
  left <- qbeta(alpha/2, positive, all - positive + 1)
  right <- qbeta(1 - alpha/2, positive + 1, all - positive)
  return(c(left, right))
}

###Cała grupa
inter1 <- clopper1(alpha, df$W1)
inter1
inter1.2 <- clopper2(alpha, df$W1)
inter1.2

###Dział zaopatrzenia
inv_salary <- atmosphere(D, Z, W1)
inter2 <- clopper1(alpha, inv_salary)
inter2
inter2.2 <- clopper2(alpha, inv_salary)
inter2.2

###Dział produkcyjny
prod_salary <- atmosphere(D, P, W1)
inter3 <- clopper1(alpha, prod_salary)
inter3
inter3.2 <- clopper2(alpha, prod_salary)
inter3.2

###Dział sprzedaży
sales_salary <- atmosphere(D, S, W1)
inter4 <- clopper1(alpha, sales_salary)
inter4
inter4.2 <- clopper2(alpha, sales_salary)
inter4.2

###Dział obsługi kadrowo-płacowej
serv_salary <- atmosphere(D, O, W1)
inter5 <- clopper1(alpha, serv_salary)
inter5
inter5.2 <- clopper2(alpha, serv_salary)
inter5.2

###Pracownicy na zwykłych stanowiskach
emp_salary <- atmosphere(S, 0, W1)
inter6 <- clopper1(alpha, emp_salary)
inter6
inter6.2 <- clopper2(alpha, emp_salary)
inter6.2

###Pracownicy na stanowiskach kierowniczych
mgr_salary <- atmosphere(S, 1, W1)
inter7 <- clopper1(alpha, mgr_salary)
inter7
inter7.2 <- clopper2(alpha, mgr_salary)
inter7.2

##CZĘŚĆ 3##

## zad 8
binomial <- function(n, p, N){
  bin <- c()
  for(i in 1:N){
    U <- runif(n, min = 0, max = 1)
    inds <- sum(U < p)
    bin <- append(bin, inds)
  }
  return(bin)
}

comp_hist <- function(n, p, N, leg = TRUE){
  # funkcja rbinom
  df1 <- data.frame(x = c(rbinom(N, n, p), binomial(n, p, N)),
                    group = as.factor(c(rep(1, N), rep(2, N))))
  plt1 <- ggplot(df1, aes(x = x, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), 
                   alpha = 0.3, 
                   binwidth = 1,
                   position = 'identity') +
    labs(y = 'p', fill = 'Użyta \nfunkcja')
  if(leg){
    plt1 +
      scale_fill_discrete(labels = c("rbinom", 
                                     "ręcznie")) +
      theme(legend.key.size = unit(5, 'mm'),
            legend.text = element_text(size=5))
  }
  else{
    plt1 + theme(legend.position = 'none')
  }
}
comp_hist(500, 0.3, 500)
comp_hist(500, 0.3, 5000)

comp_cdf <- function(n, p, N, leg = TRUE){
  # funkcja rbinom
  df1 <- data.frame(x = c(rbinom(N, n, p), binomial(n, p, N)),
                    group = as.factor(c(rep(1, N), rep(2, N))))
  plt1 <- ggplot(df1, aes(x = x, col = group)) +
    stat_ecdf() +
    labs(y = 'p', col = 'Użyta \nfunkcja')
  if(leg){
    plt1 +
      scale_color_discrete(labels = c("rbinom", 
                                     "ręcznie")) +
      theme(legend.key.size = unit(5, 'mm'),
            legend.text = element_text(size=5))
  }
  else{
    plt1 + theme(legend.position = 'none')
  }
}
comp_cdf(500, 0.3, 500)
comp_cdf(500, 0.3, 5000)
## zad 9 (ma Zosia)

n <- 30 
p <- seq(0.1, 0.9, 0.1)
alpha <- 0.05
MC <- 10

Results <- data.frame(n = integer(),
                      p = numeric(),
                      method = character(),
                      coverage_prob = numeric(),
                      ci_length = numeric())


for (mc in MC){
  results <- data.frame(n = integer(),
                        p = numeric(),
                        method = character(),
                        coverage_prob = numeric(),
                        ci_length = numeric())
  
  for (i in 1:length(n)) {
    for (j in 1:length(p)) {
      
      x <- rbinom(n[i], 1, p[j])
      
      ci_cp <- binom.confint(sum(x), n[i], conf.level = 1 - alpha, method = "exact")
      ci_wald <- binom.confint(sum(x), n[i], conf.level = 1 - alpha, method = "asymptotic")
      ci_ac <- binom.confint(sum(x), n[i], conf.level = 1 - alpha, method = "agresti-coull")
      
      results <- rbind(results,
                       data.frame(n = n[i],
                                  p = p[j],
                                  method = "Clopper-Pearson",
                                  coverage_prob = ifelse(ci_cp[1] <= p[j] & p[j] <= ci_cp[2], 1, 0),
                                  ci_length = ci_cp[,6] - ci_cp[,5]))
      
      results <- rbind(results,
                       data.frame(n = n[i],
                                  p = p[j],
                                  method = "Wald",
                                  coverage_prob = ifelse(ci_wald[1] <= p[j] & p[j] <= ci_wald[2], 1, 0),
                                  ci_length = ci_wald[,6] - ci_wald[,5]))
      
      results <- rbind(results,
                       data.frame(n = n[i],
                                  p = p[j],
                                  method = "Agresti-Coull",
                                  coverage_prob = ifelse(ci_ac[1] <= p[j] & p[j] <= ci_ac[2], 1, 0),
                                  ci_length = ci_ac[,6] - ci_ac[,5]))
      
    }
    Results <- rbind(Results, results)
  }
}

print(results)
print(Results)

ggplot(results, aes(x = p, y = ci_length, color = method)) + 
  geom_line() + 
  facet_wrap(~ n, scales = "free_x") + 
  labs(x = "p", y = "CI length") + 
  theme_minimal()

##CZĘŚĆ 4##

## zad 10, 11

# a
x_a <- df$P
x_a_success <- length(x_a[x_a == 'K'])
x_a_n <- length(x_a)
test_a <- binom.test(x_a_success, x_a_n, p = 0.5, conf.level = 0.95)

# b
x_b <- df %>% group_by(W1) %>% count() 
x_b_success <- (x_b[4, 'n'] + x_b[3, 'n'])$n
x_b_n <- length(df$W1)
test_b <- binom.test(x_b_success, x_b_n, p = 0.8, 
                     conf.level = 0.95,
                     alternative = 'greater')

# c

# d

# e
