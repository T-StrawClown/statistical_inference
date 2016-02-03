library(ggplot2)
data("ToothGrowth")
# coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
#        xlab = "ToothGrowth data: length vs dose, given type of supplement")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
g1 <- ggplot(data = ToothGrowth, aes(x = dose, y = len, fill = dose)) +
        geom_boxplot(colour = c("red", "darkgreen", "darkblue")) +
        facet_grid(. ~ supp) +
        ggtitle("Picture 1: Tooth Growth Data by Type of Supplement") +
        ylab("Tooth Length") +
        xlab("Dose")
print(g1)

#bootstrapping
dose05 <- subset(ToothGrowth, dose == "0.5")
dose1 <- subset(ToothGrowth, dose == "1")
dose2 <- subset(ToothGrowth, dose == "2")

n05 <- dim(dose05)[1]
n1 <- dim(dose1)[1]
n2 <- dim(dose2)[1]
sims <- 1000
set.seed(42)
sim_data05 <- matrix(data = sample(dose05$len,
                                   n05 * sims,
                                   replace = TRUE),
                     nrow = sims,
                     ncol = n05)
means05 <- apply(sim_data05, 1, mean)

set.seed(42)
sim_data1 <- matrix(data = sample(dose1$len,
                                  n1 * sims,
                                  replace = TRUE),
                     nrow = sims,
                     ncol = n1)
means1 <- apply(sim_data1, 1, mean)

set.seed(42)
sim_data2 <- matrix(data = sample(dose2$len,
                                  n2 * sims,
                                  replace = TRUE),
                     nrow = sims,
                     ncol = n2)
means2 <- apply(sim_data2, 1, mean)

g2 <- ggplot() +
        ggtitle("Picture 2: Density by Dose") +
        xlab("Tooth Length") +
        ylab("Density") +
        geom_density(data = data.frame(x = dose05$len),
                     aes(x = x),
                     color = "red",
                     size = 1.1) +
        geom_vline(data = data.frame(x = dose05$len),
                   xintercept = mean(dose05$len),
                   linetype = 2,
                   color = "red",
                   size = 1.1) +
        geom_density(data = data.frame(x = dose1$len),
                     aes(x = x),
                     color = "darkgreen",
                     size = 1.1) +
        geom_vline(data = data.frame(x = dose1$len),
                   xintercept = mean(dose1$len),
                   linetype = 2,
                   color = "darkgreen",
                   size = 1.1) +
        geom_density(data = data.frame(x = dose2$len),
                     aes(x = x),
                     color = "darkblue",
                     size = 1.1) +
        geom_vline(data = data.frame(x = dose2$len),
                   xintercept = mean(dose2$len),
                   linetype = 2,
                   color = "darkblue",
                   size = 1.1) +
        geom_vline(xintercept = mean(dose05$len) + c(-1, 1) * qnorm(.975) * sd(dose05$len),
                   #xintercept = t.test(dose05$len)$conf,
                   linetype = 1,
                   color = "red",
                   width = 1.1) +
        geom_vline(xintercept = mean(dose1$len) + c(-1, 1) * qnorm(.975) * sd(dose1$len),
                   #xintercept = t.test(dose1$len)$conf,
                   linetype = 1,
                   color = "darkgreen",
                   width = 1.1) +
        geom_vline(xintercept = mean(dose2$len) + c(-1, 1) * qnorm(.975) * sd(dose2$len),
                   #xintercept = t.test(dose2$len)$conf,
                   linetype = 1,
                   color = "darkblue",
                   width = 1.1)
print(g2)

g3 <- ggplot() +
        ggtitle("Picture 2: Bootstraping 0.5 mg/day dose vs. 2 mg/day dose") +
        xlab("Mean of Sample Data") +
        ylab("Density") +
        geom_density(data = data.frame(x = means05),
                       aes(x = x),
                     color = "red",
                     size = 1.1) +
        geom_vline(data = data.frame(x = means05),
                   xintercept = mean(means05),
                   linetype = 2,
                   color = "red",
                   size = 1.1) +
#         geom_vline(xintercept = t.test(dose05$len)$conf,
#                    linetype = 3,
#                    color = "red",
#                    width = 1.1) +
        geom_vline(xintercept = mean(means05) + c(-1, 1) * qnorm(.975) * sd(means05),
                   linetype = 1,
                   color = "red",
                   width = 1.1) +
        geom_density(data = data.frame(x = means1),
                     aes(x = x),
                     color = "darkgreen",
                     size = 1.1) +
        geom_vline(data = data.frame(x = means1),
                   xintercept = mean(means1),
                   linetype = 2,
                   color = "darkgreen",
                   size = 1.1) +
#         geom_vline(xintercept = t.test(dose1$len)$conf,
#                    linetype = 3,
#                    color = "darkgreen",
#                    width = 1.1) +
        geom_vline(xintercept = mean(means1) + c(-1, 1) * qnorm(.975) * sd(means1),
                   linetype = 1,
                   color = "darkgreen",
                   width = 1.1) +
        geom_density(data = data.frame(x = means2),
                     aes(x = x),
                     color = "darkblue",
                     size = 1.1) +
        geom_vline(data = data.frame(x = means2),
                   xintercept = mean(means2),
                   linetype = 2,
                   color = "darkblue",
                   size = 1.1) +
#         geom_vline(xintercept = t.test(dose2$len)$conf,
#                    linetype = 3,
#                    color = "darkblue",
#                    width = 1.1)
        geom_vline(xintercept = mean(means2) + c(-1, 1) * qnorm(.975) * sd(means2),
                   linetype = 1,
                   color = "darkblue",
                   width = 1.1)
print(g3)




# g4 <- ggplot() +
#         ggtitle("Picture 2: Bootstraping 0.5 mg/day dose vs. 2 mg/day dose") +
#         xlab("Mean of Sample Data") +
#         ylab("Density") +
#         geom_histogram(data = data.frame(x = dose05$len),
#                      aes(x = x),
#                      color = "red",
#                      size = 1.1) +
#         geom_vline(data = data.frame(x = dose05$len),
#                    xintercept = mean(dose05$len),
#                    linetype = 2,
#                    color = "red",
#                    size = 1.1) +
#         geom_histogram(data = data.frame(x = dose1$len),
#                      aes(x = x),
#                      color = "green",
#                      size = 1.1) +
#         geom_vline(data = data.frame(x = dose1$len),
#                    xintercept = mean(dose1$len),
#                    linetype = 2,
#                    color = "green",
#                    size = 1.1) +
#         geom_histogram(data = data.frame(x = dose2$len),
#                      aes(x = x),
#                      color = "blue",
#                      size = 1.1) +
#         geom_vline(data = data.frame(x = dose2$len),
#                    xintercept = mean(dose2$len),
#                    linetype = 2,
#                    color = "blue",
#                    size = 1.1)
# print(g4)

# resampling
dose2$supp <- as.character(dose2$supp)
means_diff <- function(v_len, v_supp) mean(v_len[v_supp == "VC"]) - mean(v_len[v_supp == "OJ"])
resampled_means <- sapply(1:sims, function(i) means_diff(v_len = dose2$len, v_supp = sample(dose2$supp)))

g5 <- ggplot(data = data.frame(x = resampled_means),
             aes(x = x)) +
        ggtitle("Picture 4: Simulated Resampled Data") +
        xlab("Difference in Means") +
        ylab("Density") +
        geom_density(color = "black",
                     size = 1.1,
                     fill = "salmon") +
        geom_vline(xintercept = mean(resampled_means),
                   colour = "red",
                   linetype = 2,
                   size = 1.1) +
        geom_vline(xintercept = means_diff(v_len = dose2$len, v_supp = dose2$supp),
                   color = "darkblue",
                   linetype = 2,
                   size = 1.1) +
        geom_vline(xintercept = t.test(subset(dose2, supp == "VC")$len,
                                       subset(dose2, supp == "OJ")$len,
                                       paired = FALSE,
                                       var.equal = FALSE)$conf.int,
                   linetype = 1,
                   colour = "darkgreen",
                   size = 1.1)
print(g5)

ttest_diff2 <- t.test(subset(dose2, supp == "VC")$len,
                      subset(dose2, supp == "OJ")$len,
                      paired = FALSE,
                      var.equal = FALSE)
print(ttest_diff2)

dose1$supp <- as.character(dose1$supp)
ttest_diff1 <- t.test(subset(dose1, supp == "VC")$len,
                      subset(dose1, supp == "OJ")$len,
                      paired = FALSE,
                      var.equal = FALSE)
print(ttest_diff1)

n1 <- dim(subset(dose1, supp == "VC"))[1]
n2 <- dim(subset(dose1, supp == "OJ"))[1]
sp <- sqrt( ((n1 - 1) * sd(subset(dose1, supp == "VC")$len)^2 + (n2-1) * sd(subset(dose1, supp == "OJ")$len)^2) / (n1 + n2-2))
md <- mean(subset(dose1, supp == "OJ")$len) - mean(subset(dose1, supp == "VC")$len)
semd <- sp * sqrt(1 / n1 + 1/n2)
pttest_diff1 <- power.t.test(n = 10, delta = md, type = "two.sample", alt = "one.sided", sd = sp)
print(pttest_diff1)
