# Load libraries
library(readxl)
library(ggplot2)
library(moments)
library(outliers)

# Read data
P2 <- read_excel("/Users/guozekai/Downloads/P2.xlsx", sheet = 1)

# Ensure numeric type
P2$`Serum Folate (μg/L)` <- as.numeric(P2$`Serum Folate (μg/L)`)

# Define bin width and breaks for histogram
bin_width <- 1
min_val <- floor(min(P2$`Serum Folate (μg/L)`, na.rm = TRUE)) - bin_width
max_val <- ceiling(max(P2$`Serum Folate (μg/L)`, na.rm = TRUE)) + bin_width
breaks <- seq(min_val, max_val, by = bin_width)

# Create histogram data
hist_data <- hist(P2$`Serum Folate (μg/L)`, breaks = breaks, plot = FALSE)
max_count <- max(hist_data$counts)

# Plot histogram
hist(P2$`Serum Folate (μg/L)`, 
     breaks = breaks, 
     col = "lightblue",
     main = "Serum Folate Distribution",
     xlab = "Serum Folate (μg/L)",
     ylab = "Frequency",
     ylim = c(0, max_count * 1.1),
     xlim = range(breaks))

# Add LOESS smoothed line
loess_fit <- loess(hist_data$counts ~ hist_data$mids, span = 0.3)
lines(hist_data$mids, predict(loess_fit), col = "red", lwd = 2)

# Add normal curve
x_vals <- seq(min(breaks), max(breaks), length.out = 100)
scaling_factor <- sum(hist_data$counts) * bin_width
y_vals <- dnorm(x_vals,
                mean = mean(P2$`Serum Folate (μg/L)`, na.rm = TRUE),
                sd = sd(P2$`Serum Folate (μg/L)`, na.rm = TRUE)) * scaling_factor
lines(x_vals, y_vals, col = "blue", lwd = 2)

# Frequency table
freq_table <- data.frame(
  Range = paste0("(", head(breaks, -1), ", ", tail(breaks, -1), "]"),
  Frequency = hist_data$counts
)
print(freq_table)

# Chi-square goodness-of-fit test
expected <- (c(NA, freq_table$Frequency[-length(freq_table$Frequency)]) +
               c(freq_table$Frequency[-1], NA)) / 2
expected <- expected[-c(1, length(expected))]
observed <- freq_table$Frequency[-c(1, length(freq_table$Frequency))]
chisq_test <- chisq.test(observed, p = expected / sum(expected), simulate.p.value = TRUE)
print(chisq_test)

# Normality tests
print(skewness(P2$`Serum Folate (μg/L)`, na.rm = TRUE))
print(agostino.test(P2$`Serum Folate (μg/L)`))
qqnorm(P2$`Serum Folate (μg/L)`); qqline(P2$`Serum Folate (μg/L)`, col = "red")
print(shapiro.test(P2$`Serum Folate (μg/L)`))

# Outlier test (Grubbs)
print(grubbs.test(P2$`Serum Folate (μg/L)`))

# Kolmogorov–Smirnov test
serum_folate <- P2$`Serum Folate (μg/L)`
ks_result <- ks.test(
  serum_folate,
  "pnorm",
  mean = mean(serum_folate),
  sd = sd(serum_folate)
)
print(ks_result)
