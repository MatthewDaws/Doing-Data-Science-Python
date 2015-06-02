# Sample from the Doing Data Science book

#data1 <- read.csv("../../../../Data/dds_datasets/dds_ch2_nyt/nyt1.csv")
data1 <- read.csv("Data/dds_datasets/dds_ch2_nyt/nyt1.csv")
data1$agecat <- cut(data1$Age, c(-Inf,0,18,24,34,44,54,64,Inf))

summary(data1)

library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~agecat, data=data1, FUN=siterange)

summaryBy(Gender + Signed_In + Impressions + Clicks ~ agecat, data=data1)

library("ggplot2")
ggplot(data1, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()

# Create click through rate
data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data=data1, FUN=siterange)
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions, colour=agecat))+geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions, colour=agecat))+geom_density()
ggplot(subset(data1, Clicks>0), aes(x=agecat, y=Clicks, fill=agecat))+geom_boxplot()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=agecat))+geom_density()

# Add some categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions>0] <- "Imps"
data1$scode[data1$Clicks>0] <- "Clicks"
data1$scode <- factor(data1$scode)

# This final bit of code produces a "long format" table listing all combinations
# of "Clicks, Imps, NoImps", Gender and the Age Category, and then the impressions
# count.
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat, data=data1, FUN=clen)
etable

