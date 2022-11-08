library(readr)
toxin_July <- read.csv("D:/Fall 2022/MEES799/Data/Saxitoxin_3.csv")
toxin_June <- read.csv("D:/Fall 2022/MEES799/Data/Saxitoxin_2.csv")

library(ggrepel)

#June

lmtoxin_June = lm(toxin_June$Wet_Biomass~toxin_June$Paralytic_Shellfish_Toxins_.ug.g.) #Create the linear regression
summary(lmtoxin_June) #Review the results

plot(toxin_June$Paralytic_Shellfish_Toxins_.ug.g.,
     toxin_June$Wet_Biomass, col = "green", main = "June Biomass and saxitoxin Regression",
     abline(lm(toxin_June$Wet_Biomass~toxin_June$Paralytic_Shellfish_Toxins_.ug.g.)),
     cex = 1.3, pch = 16, xlab = "Biomass Wet Weight",
     ylab = "saxitoxin (ug/g)")

June + annotate("text", x=7.5, y=55, label="r^2 = 0.2277") +
    annotate("text", x=7.5, y=50, label= "y = 3.019x+9.677")

#July

lmtoxin_July = lm(toxin_July$Wet_Biomass~toxin_July$Paralytic_Shellfish_Toxins_.ug.g.) #Create the linear regression
summary(lmtoxin_July) #Review the results

# points(toxin_July$Paralytic_Shellfish_Toxins_.ug.g.,
#        toxin_July$Wet_Biomass, col = "green", main = "July Biomass and toxintoxin Regression",
#        abline(lm(toxin_July$Wet_Biomass~toxin_July$Paralytic_Shellfish_Toxins_.ug.g.)),
#        cex = 1.3, pch = 16, xlab = "Biomass Wet Weight",
#        ylab = "toxintoxin (ug/g)")

plot(toxin_July$Paralytic_Shellfish_Toxins_.ug.g.,
       toxin_July$Wet_Biomass, col = "green", main = "July Biomass and toxintoxin Regression",
       abline(lm(toxin_July$Wet_Biomass~toxin_July$Paralytic_Shellfish_Toxins_.ug.g.)),
     cex = 1.3, pch = 16, xlab = "Biomass Wet Weight",
     ylab = "toxintoxin (ug/g)")

# combine june and july data
 #Review the results

library(dplyr)
toxin_summer <- bind_rows(toxin_June %>% mutate(month = "June"),
          toxin_July %>% mutate(month = "July"))

lmtoxin_summer = lm(toxin_summer$Paralytic_Shellfish_Toxins_.ug.g. ~ toxin_summer$Wet_Biomass) #Create the linear regression
summary(lmtoxin_summer)

plot <- plot(toxin_summer$Paralytic_Shellfish_Toxins_.ug.g.,
     toxin_summer$Wet_Biomass, col = "green", main = "Summer 2022 Biomass and toxintoxin Regression",
     abline(lm(toxin_summer$Wet_Biomass~toxin_summer$Paralytic_Shellfish_Toxins_.ug.g.)),
     cex = 1.3, pch = 16)

#ggplot

plot <- ggplot(data = toxin_summer,
       aes(x = `Wet_Biomass`,
           y = `Paralytic_Shellfish_Toxins_.ug.g.`, color = month
           )) + 
    geom_point() +
    geom_abline(intercept = coef(lmtoxin_summer)[1], 
                slope = coef(lmtoxin_summer)[2]) +
    ggtitle("Summer 2022 Wet Biomass and Saxitoxin Regression") +
    labs(x="Wet Biomass (g)", y = expression(paste("Paralytic Shellfish Toxins (",  
                                              mu, "g/g)")))
plot
    
#add texts

plot + annotate("text", x=7.5, y=55, label="r^2 = 0.2277") +
    annotate("text", x=7.5, y=50, label= "y = 3.019x+9.677")
    
#geom_smooth(method = "lm",
                #formula = Paralytic_Shellfish_Toxins_.ug.g. ~ Wet_Biomass)
