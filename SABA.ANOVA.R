library(xlsx)
library(lubridate)
library(dplyr)
library(tidyr)

# Read in and trimm data----------------------------------------------------
data <- read.xlsx("data/SABA_Tidy_Data.xlsx", 1, header=TRUE)
data <- data[,c(1:7)]

# Set n to the number of analytes-------------------------------------------
n <- na.omit(unique(data$Analyte))
n1 <- n[1]

# Filter for analyte -------------------------------------------------------
data2 <- data %>%
        filter(Analyte == n1)

# Set m = number of matrices -----------------------------------------------
m <- unique(data2$Matrix)
m1 <- m[3]

# Filter out suspect data and filter for matrix ----------------------------
data3 <- select(data2, Trial, Analyte, Matrix, Result, Comment) %>%
        filter(Comment == "NA")%>%
        filter(Matrix == m1)

data3$Trial <- as.factor(data3$Trial)

# Boxplot of data ---------------------------------------------------------
boxplot(Result~Trial, 
        data3,
        xlab = "Trial",
        ylab = "mg/kg",
        main = paste(n1, " (Matrix =", m1,")", sep=" "))

# Itemise trial sets to permit 'spread' function --------------------------
Rep <- sequence(table(data3$Trial))
data4 <- cbind(Rep, data3)
data4 <- data4[,c(3,4,2,1,5)]
data5 <- spread(data4, Trial, Result, fill="", convert = TRUE)
data5

# ANOVA ------------------------------------------------------------------
anova1 <- aov(Result~Trial, data = data3)

#Review results ----------------------------------------------------------
summary(anova1)

#Check for significant differences ---------------------------------------
TukeyHSD(anova1)

#Repeatability & Interim Precision ---------------------------------------
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

