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
  return(df2)
}

##kobiety
women <- atmosphere(P, K, A1)
women1 <- likert(women)
summary(women1)
plot(women1, type = 'bar')
plot(women1, type = 'density')

##mężczyźni
men <- atmosphere(P, M, A1)
men1 <- likert(men)
summary(men1)
plot(men1, type = 'bar')
plot(men1, type = 'density')

##dział zaopatrzenia
inv <- atmosphere(D, Z, A1)
inv1 <- likert(inv)
summary(inv1)
plot(inv1, type = 'bar')
plot(inv1, type = 'density')

##dział produkcyjny
prod <- atmosphere(D, P, A1)
prod1 <- likert(prod)
summary(prod1)
plot(prod1, type = 'bar')
plot(prod1, type = 'density')

##dział sprzedaży
sales <- atmosphere(D, S, A1)
sales1 <- likert(sales)
summary(sales1)
plot(sales1, type = 'bar')
plot(sales1, type = 'density')

##dział obsługi kadrowo-płacowej
serv <- atmosphere(D, O, A1)
serv1 <- likert(serv)
summary(serv1)
plot(serv1, type = 'bar')
plot(serv1, type = 'density')


#ZAD 7
library(binom)
alpha <- 0.05

##Gotowa funkcja z pakietu binom
clopper1 <- function(alpha, poll){
  tbl <- table(poll)
  positive <- tbl[['1']] + tbl[['2']]
  all <- sum(tbl)
  confint1 <- binom.confint(positive, all,
                            conf.level = 1 - alpha,
                            methods = 'exact')
  interval <- c(confint1$lower, confint1$upper)
  return(interval)
}

##Ręczne policzenie
clopper2 <- function(poll){
  colnames(poll) <- 'vote'
  vote <- poll$vote
  vote <- replace(vote, vote == 2, 1)
  vote <- replace(vote, vote == -1, 0)
  vote <- replace(vote, vote == -2, 0)
  vote <- replace(vote, vote == 11, 1)
  poll2 <- data.frame(vote)
  colnames(poll2) <- 'vote'
  return(poll2)
}

###Cała grupa
inter1 <- clopper1(alpha, df$A1)
inter1

###Dział zaopatrzenia
inter2 <- clopper1(alpha, inv)
inter2

###Dział produkcyjny
inter3 <- clopper1(alpha, prod)
inter3

###Dział sprzedaży
inter4 <- clopper1(alpha, sales)
inter4

###Dział obsługi kadrowo-płacowej
inter5 <- clopper1(alpha, serv)
inter5

##CZĘŚĆ 3##

##CZĘŚĆ 4##

