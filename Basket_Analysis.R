
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

df=moonkil_full
df <- df[complete.cases(df),]

df%>% mutate(description=as.factor(description))
df$S_Date <- as.Date(df$S_Date)
S_Invoice_No <- as.numeric(as.character(df$S_Invoice_No))
cbind(df,InvoiceNo)
glimpse(df)
transactionData <- ddply(df,c("S_Invoice_No"),function(df)paste(df$description,collapse = ","))

transactionData
transactionData$S_Invoice_No <- NULL
colnames(transactionData) <- c("items")
transactionData


write.csv(transactionData,"D:/msc research/R/market.csv", quote = TRUE, row.names = FALSE)
write.csv(transactionData,"D:/msc research/R/market_basket_transactions.csv", quote = FALSE, row.names = FALSE)

#tr <- read.transactions('D:/msc research/R/market.csv', format = 'basket', sep=',')
tr <- read.transactions('D:/msc research/R/market_basket_transactions.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:41])

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))
summary(shorter.association.rules)
inspect(shorter.association.rules[1:12])

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.

KOWPI.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="KOWPI"))
inspect(KOWPI.association.rules[1:3])

subRules<-KOWPI.association.rules[quality(KOWPI.association.rules)$confidence>0.4]
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")


saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

