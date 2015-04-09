library(xlsx)
library(lubridate)
library(dplyr)
library(tidyr)

data <- read.xlsx("data/SABA_Tidy_Data.xlsx", 1, header=TRUE)
data <- data[,c(1:7)]

n <- na.omit(unique(data$Analyte))

data2 <- data %>%
        filter(Analyte == n[1])

m <- unique(data2$Matrix)

data3 <- select(data2, Trial, Analyte, Matrix, Result) %>%
        filter(Matrix == m[1])%>%
        group_by(Trial)

Rep <- sequence(table(data3$Trial))

data4 <- cbind(Rep, data3)

data4 <- data4[,c(2,3,6)]
data4$Trial <- as.factor(data4$Trial)
data5 <- spread(data4, Trial, Result, fill="")
data5

anova1 <- aov(Result~Trial, data = data3)

#Review results
summary(anova1)

#Check for significant differences
TukeyHSD(anova1)

#Repeatability & Interim Precision
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR


#-------------------------------------
wb <- createWorkbook()
saveWorkbook(wb, 'output.xlsx')

lapply(names(myList), function(x) write.xlsx(myList[[x]], 'output.xlsx', sheetName=x, append=TRUE))

http://stackoverflow.com/questions/7891600/write-a-list-of-named-data-frames-to-an-xlsx-file