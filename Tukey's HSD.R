# Load required packages
library(car)
library(nlme)
library(dplyr)
library(readxl)

# Read the Excel file
turkey_s_HSD <- read_excel("/Users/guozekai/Downloads/turkey's HSD.xlsx")

# Rename columns (remove spaces)
names(turkey_s_HSD)[names(turkey_s_HSD) == "Smoker Status"] <- "SmokerStatus"
colnames(turkey_s_HSD)[colnames(turkey_s_HSD) == "Forced Expiratory Volume in 1 second (FEV1) (liters)"] <- "FEV1"

# Convert to factors
turkey_s_HSD$SmokerStatus <- factor(turkey_s_HSD$SmokerStatus)
turkey_s_HSD$Sex <- factor(turkey_s_HSD$Sex)


# Levene's test (variance homogeneity)
leveneTest(FEV1 ~ SmokerStatus * Sex, data = turkey_s_HSD)

# Shapiro-Wilk test (normality by group)
shapiro_results <- turkey_s_HSD %>%
  group_by(SmokerStatus, Sex) %>%
  summarise(p_value = shapiro.test(FEV1)$p.value)

print(shapiro_results)

# GLS model with group-specific variance
gls_model <- gls(
  FEV1 ~ SmokerStatus * Sex, 
  data = turkey_s_HSD,
  weights = varIdent(form = ~ 1 | SmokerStatus * Sex)
)

summary(gls_model)

# Two-way ANOVA
anova2 <- aov(FEV1 ~ SmokerStatus + Sex + SmokerStatus:Sex, data = turkey_s_HSD)

# Tukey HSD test for SmokerStatus
tukey_result <- TukeyHSD(anova2, "SmokerStatus")
plot(tukey_result)

# Format Tukey results
df_result <- as.data.frame(tukey_result$SmokerStatus)
df_result <- apply(df_result, 2, function(x) formatC(x, format = "e", digits = 6))
print(df_result)

