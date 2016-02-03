library(ggplot2)
set.seed(42)
lambda <- .2
sims <- 1000
n <- 40
# 1000 simulations
sim_data <- rexp(sims * n, lambda)
exp_data <- matrix(sim_data, sims, n)
means <- rowMeans(exp_data)
vars <- apply(exp_data, 1, var)
g1 <- ggplot() +
        scale_x_continuous(limits = c(0, 10)) +
        ggtitle("Picture 1: Sample Mean vs. Theoretical Mean") +
        xlab("Value") +
        ylab("Density") +
        geom_histogram(data = data.frame(x = sim_data),
                       aes(x = x, y = ..density..),
                       fill = "darkblue",
                       binwidth = .2,
                       color = "darkblue",
                       alpha = .2) +
        geom_density(data = data.frame(x = sim_data),
                     aes(x = x, y = ..density..),
                     fill = "transparent",
                     color = "darkblue",
                     size = 1.1) +
        geom_histogram(data = data.frame(x = means),
                       aes(x = x, y = ..density..),
                       binwidth = .2,
                       fill = "red",
                       color = "red",
                       alpha = .2) +
        geom_density(data = data.frame(x = means),
                     aes(x = x, y = ..density..),
                     fill = "transparent",
                     color = "red",
                     size = 1.1) +
        geom_vline(xintercept = mean(means),
                   size = 1,
                   color = "darkblue",
                   linetype = 2) +
        geom_vline(xintercept = 1 / lambda,
                   color = "red",
                   size = 1,
                   linetype = 2)
print(g1)

#sample variance approaching true variance
g2 <- ggplot(data = data.frame(y = cumsum(vars) / 1:sims),
            aes(y = y, x = 1:sims)) +
        scale_y_continuous(limits = c(20, 40)) +
        ggtitle("Picture 2: Sample Variance vs. Theoretical Variance") +
        xlab("Number of Tries") +
        ylab("Cumulative Variance") +
        geom_point(color = "red",
                   size = 2) +
        geom_hline(yintercept = 1 / lambda^2,
                   color = "darkblue",
                   size = 1.1)
print(g2)

#quantiles of means vs normal distribution quantiles
g3 <- ggplot(data = data.frame(x = means),
             aes(sample = x)) +
        ggtitle("Picture 3: Sample Distribution vs Normal Distribution") +
        xlab("Normal Quantiles") +
        ylab("Sample Quantiles") +
        scale_y_continuous(breaks = round(mean(means) + seq(-3, 3, by = 1) * sd(means), 4)) +
        stat_qq(color = "red") +
        geom_abline(slope = sd(means),
                    intercept = mean(means),
                    color = "darkblue",
                    size = 1.1)
print(g3)



