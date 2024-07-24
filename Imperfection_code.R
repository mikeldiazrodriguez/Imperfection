# Set working directory
setwd("~/Data/csv")

# Import data from csv files

sample1 <- read.csv("sample1.csv", header = TRUE, dec = ",", sep = ";")
sample2 <- read.csv("sample2.csv", header = TRUE, dec = ",", sep = ";")

# Calculate the difference in distances
distances <- sample1$distance

# Statistical summary
summary(distances)

# Check normality with Shapiro test
shapiro.test(sample1$distance)

# Divide the Plots window in 2 parts
par(mfrow = c(1, 2))

# Create histogram
hist(sample1$distance, main = "a. Histogram of non filtered distances",
     xlab = "Distances (m)",
     col = "#fcc5c0")

# Create boxplot
boxplot(sample1$distance, main = "b. Boxplot of non filtered distances", 
        ylab = "Distances (m)",
        col = "#fcc5c0")


# Exclude distance values = 0
filtered_sample <- subset(sample1, distance > 0)

# Calculate the difference in distances
distances2 <- filtered_sample$distance

# Statistical summary
summary(distances2)

# Check normality with Shapiro test
shapiro.test(filtered_sample$distance)

# Histogram
hist(filtered_sample$distance, main = "a. Histogram of filtered distances", 
     xlab = "Distances (m)",
     col = "#addd8e")

# Boxplot
boxplot(filtered_sample$distance, main = "b. Boxplot of filtered distances", 
        ylab = "Distances (m)",
        col = "#addd8e")


# Create variables for statistics
ALTA1 <- sample1$ALTA
ALTA2 <- sample2$ALTA

HYDROC1 <- sample1$HYDROC
HYDROC2 <- sample2$HYDROC

ASP1 <- sample1$ASP
ASP2 <- sample2$ASP


# Statistical summary
summary(ALTA1)
summary(ALTA2)

summary(HYDROC1)
summary(HYDROC2)

summary(ASP1)
summary(ASP2)


# Add a 'group' column to each sample to identify where the data comes from.
sample1$group <- "Not corrected"
sample2$group <- "Corrected"

# Combine the samples into a single dataset.
combined_sample <- rbind(sample1, sample2)

# Divide the Plots window in 3 parts
par(mfrow = c(1, 3))

# ALTA Boxplot
boxplot(ALTA ~ group, data = combined_sample, 
        main = "a. ALTA Boxplot", 
        ylab = "ALTA (masl)",
        col = c("#d8daeb", "#fee0b6"), # colours to differentiate the samples
        border = "black")

# HYDROC Boxplot
boxplot(HYDROC ~ group, data = combined_sample, 
        main = "b. HYDROC boxplot", 
        ylab = "HYDROC (mintues)",
        col = c("#d8daeb", "#fee0b6"), 
        border = "black")

# ASP Boxplot
boxplot(ASP ~ group, data = combined_sample, 
        main = "c. ASP Boxplot", 
        ylab = "ASP (degrees)",
        col = c("#d8daeb", "#fee0b6"), 
        border = "black")



# Statistical tests
t.test(ALTA ~ group, data = combined_sample)
wilcox.test(ALTA ~ group, data = combined_sample)

t.test(HYDROC ~ group, data = combined_sample)
wilcox.test(HYDROC ~ group, data = combined_sample)

t.test(ASP ~ group, data = combined_sample)
wilcox.test(ASP ~ group, data = combined_sample)


# Exclude sites with distance values = 0
sample1 <- sample1[sample1$distance != 0, ]
sample2 <- sample2[sample2$distance != 0, ]


# Boxplots to create each variable: ALTA, HYDROC and ASP
boxplot(sample1$ALTA, sample2$ALTA, 
        main="a. ALTA boxplot",
        ylab="Altitude (masl)",
        col = c("#80b1d3", "#ffffb3"),
        names=c("Not corrected", "Corrected"))

boxplot(sample1$HYDROC, sample2$HYDROC, 
        main="b. HYDROC boxplot",
        ylab="Cost to potential hydrology (minutes)",
        col = c("#80b1d3", "#ffffb3"),
        names=c("Not corrected", "Corrected"))

boxplot(sample1$ASP, sample2$ASP, 
        main="c. ASP boxplot",
        ylab="Aspect (degrees)",
        col = c("#80b1d3", "#ffffb3"),
        names=c("Not corrected", "Corrected"))


# Statistical tests
t.test(sample1$ALTA, sample2$ALTA)
wilcox.test(sample1$ALTA, sample2$ALTA)

t.test(sample1$HYDROC, sample2$HYDROC)
wilcox.test(sample1$HYDROC, sample2$HYDROC)

t.test(sample1$ASP, sample2$ASP)
wilcox.test(sample1$ASP, sample2$ASP)

# Create variables for statistics
ALTA1 <- sample1$ALTA
ALTA2 <- sample2$ALTA

HYDROC1 <- sample1$HYDROC
HYDROC2 <- sample2$HYDROC

ASP1 <- sample1$ASP
ASP2 <- sample2$ASP


# Statistical summary
summary(ALTA1)
summary(ALTA2)

summary(HYDROC1)
summary(HYDROC2)

summary(ASP1)
summary(ASP2)


