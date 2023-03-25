# zadanie 9

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
            