#install.packages("naniar")
#library(dplyr)
#library(tidyr)
#library(tidyverse)
#library(purrr)
library(reshape2)
library(ggplot2)
library(naniar)
library(scales) 
#library(stringr)
#library(data.table)


## Obtain
# Load data
transaction<-read.csv('card_transdata.csv')

## Peek at data
head(transaction)

# Display the structure of dataset
str(transaction)
#glimpse(transaction)

# Display summary of dataset
summary(transaction$distance_from_home)
summary(transaction$distance_from_last_transaction)
summary(transaction$ratio_to_median_purchase_price)
summary(transaction)

# Display the columns of the dataset
colnames(transaction)


## Scrub
# Check missing data
any(is.na(transaction))

# Check duplicated data
#sum(duplicated(transaction))   # Take note it will get killed due to long run

# Plot Heatmap for empty data
#gg_miss_upset(transaction)      # this plot cannot be used if there is no missing data in dataset

# Create Correlation Matrix
corr_mat <- round(cor(transaction),2)  # 'cor()' calculated pairwise correlations between numeric columns
head(corr_mat)
melted_corr_mat <- melt(corr_mat)
ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() +
  geom_text(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggplot(data=melted_corr_mat, aes(x=Var1, y=Var2,fill=value,label=value)) + geom_tile()


# Categorical - univariate analysis - Fraud distribution
fraud_cases <- sum(transaction$fraud==1)
non_fraud_cases <- nrow(transaction)-fraud_cases
fraud_cases_perc <- fraud_cases/nrow(transaction)*100
non_fraud_cases_perc <- non_fraud_cases/nrow(transaction)*100
fraud_data <- data.frame(Type = c("Fraud","Non-Fraud"), Count = c(fraud_cases,non_fraud_cases), Percentage = c(fraud_cases_perc,non_fraud_cases_perc))
ggplot(fraud_data, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Distribution of Fraud Cases", x = "Type", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Trial on pie chart
#ggplot(fraud_data,aes(x = "",y = Count,fill=Type,label=Count)) + 
#  geom_bar(stat = "identity",width=1) +
#  geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) +
#  coord_polar("y", start=0) + 
#  theme_void()
#  labs(title = "Distribution of Fraud Cases") +
#  theme(legend.position = "bottom")

# Categorical - univariate analysis - Repeat retailer distribution
rep_retailer <- sum(transaction$repeat_retailer==1)
non_rep_retailer <- nrow(transaction)-rep_retailer
rep_retailer_perc <- (rep_retailer/nrow(transaction))*100
non_rep_retailer_perc <- (non_rep_retailer/nrow(transaction))*100
rep_retailer_data <- data.frame(Type = c("Repeat Retailer","Non-Repeat Retailer"), Count = c(rep_retailer,non_rep_retailer), Percentage=c(rep_retailer_perc,non_rep_retailer_perc))
rep_retailer_data$Type <- factor(rep_retailer_data$Type, levels = c("Repeat Retailer","Non-Repeat Retailer"))
ggplot(rep_retailer_data, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Distribution of Repeat Retailer transaction", x = "Type", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Categorical - univariate analysis - used_chip distribution
used_chip <- sum(transaction$used_chip==1)
non_used_chip <- nrow(transaction)-used_chip
used_chip_perc <- (used_chip/nrow(transaction)) * 100
non_used_chip_perc <- (non_used_chip/nrow(transaction)) * 100
used_chip_data <- data.frame(Type=c("Used Chip","Non-Used Chip"),Count=c(used_chip,non_used_chip),Percentage=c(used_chip_perc,non_used_chip_perc))
used_chip_data$Type <- factor(used_chip_data$Type, levels = c("Used Chip","Non-Used Chip"))
ggplot(used_chip_data, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Distribution of Used Chip Transaction",x="Type", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Categorical - univariate analysis - used_pin_number distribution
used_pin <- sum(transaction$used_pin_number==1)
non_used_pin <- nrow(transaction)-used_pin
used_pin_perc <- (used_pin/nrow(transaction)) * 100
non_used_pin_perc <- (non_used_pin/nrow(transaction)) * 100
used_pin_data <- data.frame(Type=c("Used Pin","Non-Used Pin"),Count=c(used_pin,non_used_pin),Percentage=c(used_pin_perc,non_used_pin_perc))
used_pin_data$Type <- factor(used_pin_data$Type, levels = c("Used Pin","Non-Used Pin"))
ggplot(used_pin_data, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Distribution of Used Pin transaction",x="Type", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Categorical - univariate analysis - online order distribution
online_order <- sum(transaction$online_order==1)
non_online_order <- nrow(transaction)-online_order
online_order_perc <- (online_order/nrow(transaction)) * 100
non_online_order_perc <- (non_online_order/nrow(transaction)) * 100
online_order_data <- data.frame(Type=c("Online Order","Non-Online Order"),Count=c(online_order,non_online_order),Percentage=c(online_order_perc,non_online_order_perc))
online_order_data$Type <- factor(online_order_data$Type, levels = c("Online Order","Non-Online Order"))
ggplot(online_order_data, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Distribution of Online Order transaction",x="Type", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)


# Univariate analysis - Distance from home
summary(transaction$distance_from_home)
# get_box_stats <- function(y, upper_limit = max(transaction$distance_from_home) * 1.15) {
#   return(data.frame(
#     y = 0.95 * upper_limit,
#     label = paste(
#       "1st Qu =", round(sum_trans[2], 2), "\n",
#       "3rd Qu =", round(sum_trans[5], 2), "\n",
#       "IQR =", round(IQR(y), 2), "\n",
#       "Mean =", round(mean(y), 2), "\n",
#       "Median =", round(median(y), 2), "\n",
#       "Min =", round(sum_trans[1], 2), "\n"
#     )
#   ))
# }
ggplot(transaction, aes(x = "", y = distance_from_home)) +
  geom_point(aes(y = distance_from_home), color = "black", alpha = 0.5) + 
  geom_boxplot(aes(y = distance_from_home), fill = "red", alpha = 0.5, width = 0.1) +
  labs(title = "Distribution of Distance from home",x = "", y = "Distance") + theme(plot.title = element_text(hjust = 0.5)) 
#stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9)

# Test outliers
raw_sum_distance<-transaction$ratio_to_median_purchase_price
sum_distance<-summary(transaction$ratio_to_median_purchase_price)
min_sum<-sum_distance[1]
q1_sum<-sum_distance[2]
med_sum<-sum_distance[3]
mean_sum<-sum_distance[4]
q3_sum<-sum_distance[5]
max_sum<-sum_distance[6]
# get IQR
iqr<-IQR(sum_distance)
# get threshold values for outliers
Tmin<-q1_sum-(1.5*iqr) 
Tmin
Tmax<-q3_sum+(1.5*iqr)
Tmax
length(raw_sum_distance[which(raw_sum_distance < Tmin | raw_sum_distance > Tmax)])

# Univariate analysis - distance_from_last_transaction
summary(transaction$distance_from_last_transaction)
ggplot(transaction, aes(x = "", y = distance_from_last_transaction)) +
  geom_point(aes(y = distance_from_last_transaction), color = "black", alpha = 0.5) + 
  geom_boxplot(aes(y = distance_from_last_transaction), fill = "red", alpha = 0.5, width = 0.1) +
  labs(title = "Distribution of Distance From Last Transaction", x = "", y = "Distance") + theme(plot.title = element_text(hjust = 0.5)) 

# Univariate analysis - ratio_to_median_purchase_price
summary(transaction$ratio_to_median_purchase_price)
ggplot(transaction, aes(x = "", y = ratio_to_median_purchase_price)) +
  geom_point(aes(y = ratio_to_median_purchase_price), color = "black", alpha = 0.5) + 
  geom_boxplot(aes(y = ratio_to_median_purchase_price), fill = "red", alpha = 0.5, width = 0.1) +
  labs(title = "Distribution of Ratio to Median Purchase Price", x = "", y = "Ratio to Median price") + theme(plot.title = element_text(hjust = 0.5)) 


# Revised: Bivariate analysis - Online order vs fraud
online_fraud <- nrow(subset(transaction,online_order==1 & fraud==1))
nononline_fraud <- nrow(subset(transaction,online_order==0 & fraud==1))
online_nonfraud <- nrow(subset(transaction,online_order==1 & fraud==0))
nononline_nonfraud <- nrow(subset(transaction,online_order==0 & fraud==0))
online_fraud_perc <- (online_fraud/nrow(subset(transaction,online_order==1))) * 100
online_nonfraud_perc <- 100-online_fraud_perc
nononline_fraud_perc <- (nononline_fraud/nrow(subset(transaction,online_order==0))) * 100
nononline_nonfraud_perc <- 100-nononline_fraud_perc
onlinefraud_data <- data.frame(Fraud=c("Fraud","Fraud","Not Fraud","Not Fraud"),Type=c("Online","Non-Online","Online","Non-Online"),Count=c(online_fraud,nononline_fraud,online_nonfraud,nononline_nonfraud),Percentage=c(online_fraud_perc,nononline_fraud_perc,online_nonfraud_perc,nononline_nonfraud_perc))
onlinefraud_data$Type <- factor(onlinefraud_data$Type, levels = c("Online","Non-Online"))
ggplot(onlinefraud_data, aes(x=Type, y=Count, fill=Fraud)) + 
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Bivariate Analysis - Online Order VS Fraud", x = "Online Order", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Revised: Bivariate analysis - used_pin_number vs fraud
usedpin_fraud <- nrow(subset(transaction,used_pin_number==1 & fraud==1))
nonusedpin_fraud <- nrow(subset(transaction,used_pin_number==0 & fraud==1))
usedpin_nonfraud <- nrow(subset(transaction,used_pin_number==1 & fraud==0))
nonusedpin_nonfraud <- nrow(subset(transaction,used_pin_number==0 & fraud==0))
usedpin_fraud_perc <- (usedpin_fraud/nrow(subset(transaction,used_pin_number==1))) * 100
usedpin_nonfraud_perc <- 100-usedpin_fraud_perc
nonusedpin_fraud_perc <- (nonusedpin_fraud/nrow(subset(transaction,used_pin_number==0))) * 100
nonusedpin_nonfraud_perc <- 100-nonusedpin_fraud_perc
usedpin_data <- data.frame(Fraud=c("Fraud","Fraud","Not Fraud","Not Fraud"),Type=c("Used Pin","Non-Used Pin","Used Pin","Non-Used Pin"),Count=c(usedpin_fraud,nonusedpin_fraud,usedpin_nonfraud,nonusedpin_nonfraud),Percentage=c(usedpin_fraud_perc,nonusedpin_fraud_perc,usedpin_nonfraud_perc,nonusedpin_nonfraud_perc))
usedpin_data$Type <- factor(usedpin_data$Type, levels = c("Used Pin","Non-Used Pin"))
ggplot(usedpin_data, aes(x=Type, y=Count, fill=Fraud)) + 
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Bivariate Analysis - Used Pin No. VS Fraud", x = "Used Pin", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Revised: Bivariate analysis - used_chip vs fraud
usedchip_fraud <- nrow(subset(transaction,used_chip==1 & fraud==1))
nonusedchip_fraud <- nrow(subset(transaction,used_chip==0 & fraud==1))
usedchip_nonfraud <- nrow(subset(transaction,used_chip==1 & fraud==0))
nonusedchip_nonfraud <- nrow(subset(transaction,used_chip==0 & fraud==0))
usedchip_fraud_perc <- (usedchip_fraud/nrow(subset(transaction,used_chip==1))) * 100
usedchip_nonfraud_perc <- 100-usedchip_fraud_perc
nonusedchip_fraud_perc <- (nonusedchip_fraud/nrow(subset(transaction,used_chip==0))) * 100
nonusedchip_nonfraud_perc <- 100-nonusedchip_fraud_perc
usedchip_data <- data.frame(Fraud=c("Fraud","Fraud","Not Fraud","Not Fraud"),Type=c("Used Chip","Non-Used Chip","Used Chip","Non-Used Chip"),Count=c(usedchip_fraud,nonusedchip_fraud,usedchip_nonfraud,nonusedchip_nonfraud),Percentage=c(usedchip_fraud_perc,nonusedchip_fraud_perc,usedchip_nonfraud_perc,nonusedchip_nonfraud_perc))
usedchip_data$Type <- factor(usedchip_data$Type, levels = c("Used Chip","Non-Used Chip"))
ggplot(usedchip_data, aes(x=Type, y=Count, fill=Fraud)) + 
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Bivariate Analysis - Used Chip VS Fraud", x = "Used Chip", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

# Revised: Bivariate analysis - repeat_retailer vs fraud
repeat_fraud <- nrow(subset(transaction,repeat_retailer==1 & fraud==1))
nonrepeat_fraud <- nrow(subset(transaction,repeat_retailer==0 & fraud==1))
repeat_nonfraud <- nrow(subset(transaction,repeat_retailer==1 & fraud==0))
nonrepeat_nonfraud <- nrow(subset(transaction,repeat_retailer==0 & fraud==0))
repeat_fraud_perc <- (repeat_fraud/nrow(subset(transaction,repeat_retailer==1))) * 100
repeat_nonfraud_perc <- 100-repeat_fraud_perc
nonrepeat_fraud_perc <- (nonrepeat_fraud/nrow(subset(transaction,repeat_retailer==0))) * 100
nonrepeat_nonfraud_perc <- 100-nonrepeat_fraud_perc
repeat_data <- data.frame(Fraud=c("Fraud","Fraud","Not Fraud","Not Fraud"),Type=c("Repeat Retailer","Non-Repeat Retailer","Repeat Retailer","Non-Repeat Retailer")
                            ,Count=c(repeat_fraud,nonrepeat_fraud,repeat_nonfraud,nonrepeat_nonfraud),Percentage=c(repeat_fraud_perc,nonrepeat_fraud_perc,repeat_nonfraud_perc,nonrepeat_nonfraud_perc))
repeat_data$Type <- factor(repeat_data$Type, levels = c("Repeat Retailer","Non-Repeat Retailer"))
ggplot(repeat_data, aes(x=Type, y=Count, fill=Fraud)) + 
  geom_bar(stat = "identity",position="dodge") +
  geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
  labs(title = "Bivariate Analysis - Repeat Retailer VS Fraud", x = "Repeat Retailer", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)
