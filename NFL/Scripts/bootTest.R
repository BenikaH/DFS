# clear console and workspace
cat("\014")
rm(list = ls())

# pop <- rnorm(10000, 10, 3)
# nSamples <- 30
# sampleSize <- 10
# samples <- lapply(1:nSamples, function(i) sample(pop, size = sampleSize, replace = T))
# muBoot <- sapply(samples, mean)
# muHat <- mean(muBoot)
# muRange <- range(muBoot)
# se <- sd(muBoot)
# se2 <- sd(samples[[15]]) / sqrt(sampleSize)
# sigmaHat <- se*sqrt(sampleSize)
# sigmaHat2 <- se2*sqrt(sampleSize)

pop <- rnorm(10000, 10, 3)
nSamples <- 8
sampleSize <- 500
samples <- lapply(1:nSamples, function(i) sample(pop, size = sampleSize, replace = F))
means <- sapply(samples, mean)