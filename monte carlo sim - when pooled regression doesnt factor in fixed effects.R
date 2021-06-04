test <- data.table(
  run = seq(1, 500),
  mean = 777,
  pooled_nofixed = 777,
  pooled_fixed = 777
)

runif2 <- function(x) {
  runif(x, min = -1, max = 1)
}

for (i in test$run) {
  bob <- data.frame(a = runif(1000)) %>% as.data.table()
  bob[, b := rep(c("a", "b", "c", "d"), 250)]
  
  bob[b == "a", c := a * runif2(1) + runif2(1)]
  bob[b == "b", c := a * runif2(1) + runif2(1)]
  bob[b == "c", c := a * runif2(1) + runif2(1)]
  bob[b == "d", c := a * runif2(1) + runif2(1)]
  
  bob[b == "a", c := c + rnorm(250, mean = runif2(1), sd = abs(runif2(1)))]
  bob[b == "b", c := c + rnorm(250, mean = runif2(1), sd = abs(runif2(1)))]
  bob[b == "c", c := c + rnorm(250, mean = runif2(1), sd = abs(runif2(1)))]
  bob[b == "d", c := c + rnorm(250, mean = runif2(1), sd = abs(runif2(1)))]
  
  pooled_nofixed_ <- summary(lm(c ~ a, data = bob))$r.squared
  pooled_fixed_ <- summary(lm(c ~ a + b, data = bob))$r.squared
  pooled_fixed_other_ <- summary(lm(c ~ a + b + b:a, data = bob))$r.squared
  
  sal <-
    lapply(unique(bob$b), function(x)
      summary(lm(c ~ a, data = bob[b == x]))$r.squared)
  
  indiv_mean <- sal %>% unlist %>% mean
  
  test[run == i, mean := indiv_mean]
  test[run == i, pooled_nofixed := pooled_nofixed_]
  test[run == i, pooled_fixed := pooled_fixed_]
  test[run == i, pooled_fixed_other := pooled_fixed_other_]
}


plot <-
  ggplot(test, aes(x = mean, y = pooled_nofixed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm") +
  labs(title = "no fixed effects", 
       x = "mean of R2 individual regressions", 
       y = "R2 pooled regression")

ggsave(
  "pooled R squared without fixed effects.png",
  plot,
  width = 5,
  height = 5
)

plot <-
  ggplot(test, aes(x = mean, y = pooled_fixed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm") +
  labs(title = "fixed effects", 
       x = "mean of R2 individual regressions", 
       y = "R2 pooled regression")

ggsave("pooled R squared with fixed effects.png", plot, width =  5, height = 5)


plot <-
  ggplot(test, aes(x = mean, y = pooled_fixed_other)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_smooth(method = "lm") +
  labs(title = "fixed effects and interaction", 
       x = "mean of R2 individual regressions", 
       y = "R2 pooled regression")

ggsave("pooled R squared with fixed effects and interaction.png", plot, width =  5, height = 5)
