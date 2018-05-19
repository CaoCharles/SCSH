# HW02

library("ggplot2")
library("dplyr")
library("reshape2")

options(scipen = 999)
set.seed(1)

comp1.vals <- data_frame(comp = "A", vals = rnorm(50, mean = 1, sd = 0.5))
comp2.vals <- data_frame(comp = "B", vals = rnorm(50, mean = 1.5, sd = 0.5))

vals.df <- bind_rows(comp1.vals, comp2.vals)

vals.df %>%
  ggplot(aes(x = vals, y = "A", color = factor(comp))) +
  geom_point(alpha = 0.4) +
  scale_color_discrete(name = "Source of Data") +
  xlab("Values") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")

#
vals.df %>%
  group_by(comp) %>%
  summarize(mean_vals = mean(vals), sd_vals = sd(vals))

vals.df %>%
  ggplot(aes(x = vals, y = 0)) +
  geom_point(alpha = 0.4) +
  xlab("Values") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# K MEANS
wait <- faithful$waiting

wait.kmeans <- kmeans(wait, 2)
wait.kmeans.cluster <- wait.kmeans$cluster

wait.df <- data_frame(x = wait, cluster = wait.kmeans.cluster)

wait.df %>%
  mutate(num = row_number()) %>%
  ggplot(aes(y = num, x = x, color = factor(cluster))) +
  geom_point() +
  ylab("Values") +
  ylab("Data Point Number") +
  scale_color_discrete(name = "Cluster") +
  ggtitle("K-means Clustering")

# 
wait.summary.df <- wait.df %>%
  group_by(cluster) %>%
  summarize(mu = mean(x), variance = var(x), std = sd(x), size = n())

wait.summary.df %>%
  select(cluster, mu, variance, std)
#
wait.summary.df <- wait.summary.df %>%
  mutate(alpha = size / sum(size))

wait.summary.df %>%
  select(cluster, size, alpha)
#
comp1.prod <- dnorm(66, wait.summary.df$mu[1], wait.summary.df$std[1])*wait.summary.df$alpha[1]
comp2.prod <- dnorm(66, wait.summary.df$mu[2], wait.summary.df$std[2])*wait.summary.df$alpha[2]

normalizer <- comp1.prod + comp2.prod
comp1.prod / normalizer

#
comp1.prod <- wait.summary.df$alpha[1] * dnorm(x = wait, mean = wait.summary.df$mu[1], sd = wait.summary.df$std[1]) 

comp2.prod <- wait.summary.df$alpha[2] * dnorm(x = wait, mean = wait.summary.df$mu[2], sd = wait.summary.df$std[2]) 

normalizer <- comp1.prod + comp2.prod

comp1.post <- comp1.prod / normalizer
comp2.post <- comp2.prod / normalizer

#
comp1.n <- sum(comp1.post)
comp2.n <- sum(comp2.post)

comp1.mu <- 1/comp1.n * sum(comp1.post * wait)
comp2.mu <- 1/comp2.n * sum(comp2.post * wait)

comp1.var <- sum(comp1.post * (wait - comp1.mu)^2) * 1/comp1.n
comp2.var <- sum(comp2.post * (wait - comp2.mu)^2) * 1/comp2.n

comp1.alpha <- comp1.n / length(wait)
comp2.alpha <- comp2.n / length(wait)

comp.params.df <- data.frame(comp = c("comp1", "comp2"),
                             comp.mu = c(comp1.mu, comp2.mu),
                             comp.var = c(comp1.var, comp2.var),
                             comp.alpha = c(comp1.alpha, comp2.alpha),
                             comp.cal = c("self", "self"))
#
# Already calculate component responsibilities for each data point from above
sum.of.comps <- comp1.prod + comp2.prod
sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
sum(sum.of.comps.ln)


# EM算法 --------------------------------------------------------------------

#' Expectation Step of the EM Algorithm
#'
#' Calculate the posterior probabilities (soft labels) that each component
#' has to each data point.
#'
#' @param sd.vector Vector containing the standard deviations of each component
#' @param sd.vector Vector containing the mean of each component
#' @param alpha.vector Vector containing the mixing weights  of each component
#' @return Named list containing the loglik and posterior.df
e_step <- function(x, mu.vector, sd.vector, alpha.vector) {
  comp1.prod <- dnorm(x, mu.vector[1], sd.vector[1]) * alpha.vector[1]
  comp2.prod <- dnorm(x, mu.vector[2], sd.vector[2]) * alpha.vector[2]
  sum.of.comps <- comp1.prod + comp2.prod
  comp1.post <- comp1.prod / sum.of.comps
  comp2.post <- comp2.prod / sum.of.comps
  
  sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
  sum.of.comps.ln.sum <- sum(sum.of.comps.ln)
  
  list("loglik" = sum.of.comps.ln.sum,
       "posterior.df" = cbind(comp1.post, comp2.post))
}

#' Maximization Step of the EM Algorithm
#'
#' Update the Component Parameters
#'
#' @param x Input data.
#' @param posterior.df Posterior probability data.frame.
#' @return Named list containing the mean (mu), variance (var), and mixing
#'   weights (alpha) for each component.
m_step <- function(x, posterior.df) {
  comp1.n <- sum(posterior.df[, 1])
  comp2.n <- sum(posterior.df[, 2])
  
  comp1.mu <- 1/comp1.n * sum(posterior.df[, 1] * x)
  comp2.mu <- 1/comp2.n * sum(posterior.df[, 2] * x)
  
  comp1.var <- sum(posterior.df[, 1] * (x - comp1.mu)^2) * 1/comp1.n
  comp2.var <- sum(posterior.df[, 2] * (x - comp2.mu)^2) * 1/comp2.n
  
  comp1.alpha <- comp1.n / length(x)
  comp2.alpha <- comp2.n / length(x)
  
  list("mu" = c(comp1.mu, comp2.mu),
       "var" = c(comp1.var, comp2.var),
       "alpha" = c(comp1.alpha, comp2.alpha))
}

#
for (i in 1:50) {
  if (i == 1) {
    # Initialization
    e.step <- e_step(wait, wait.summary.df[["mu"]], wait.summary.df[["std"]],
                     wait.summary.df[["alpha"]])
    m.step <- m_step(wait, e.step[["posterior.df"]])
    cur.loglik <- e.step[["loglik"]]
    loglik.vector <- e.step[["loglik"]]
  } else {
    # Repeat E and M steps till convergence
    e.step <- e_step(wait, m.step[["mu"]], sqrt(m.step[["var"]]), 
                     m.step[["alpha"]])
    m.step <- m_step(wait, e.step[["posterior.df"]])
    loglik.vector <- c(loglik.vector, e.step[["loglik"]])
    
    loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
    if(loglik.diff < 1e-6) {
      break
    } else {
      cur.loglik <- e.step[["loglik"]]
    }
  }
}
loglik.vector
cur.loglik
getwd()
