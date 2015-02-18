data.in <- read.csv("data/SABA_Linearity.csv", as.is=TRUE, header=TRUE)
library(dplyr)
library(ggplot2)
library(broom)

# Select analyte --------------------------------------------------------------------------
analyte <- "Sorbic Acid"

# Filter for that analyte -----------------------------------------------------------------
data2 <- select(data.in, everything())%>%
        filter(Analyte == analyte)

# Least squares & save info ---------------------------------------------------------------
fit <- lm(Area~Conc-1, data=data2)
summary(fit)
data.lm2 <- tidy(fit)
data.r2 <- summary(fit)$r.squared

write.csv(data.lm2, paste("output/", analyte," linear.csv",sep=""), row.names=FALSE)
write.csv(data.r2, paste("output/", analyte," r2.csv",sep=""), row.names=FALSE)

# Plot graph ------------------------------------------------------------------------------
plot <- ggplot(data2, aes(x = Conc, y = Area)) +
        geom_point(size=4, colour = "darkgreen") +
        geom_smooth(method=lm, se=TRUE) +
        labs(x="Concentration, mg/100mL", 
             y="Area", 
             title= paste(analyte, "Linearity Curve")) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

plot

# Save plot ------------------------------------------------------------------------------
png(filename = paste0("graphs/",analyte, "-Linearity Curve.png", sep=""),
    width = 1000, height = 550, units = "px", pointsize = 12)
plot(plot)
dev.off()