library(readr)
#32/40.
#1a Data Wrangling


dim(loan_default_data_set)
nrow(loan_default_data_set)
ncol(loan_default_data_set)
 
# The dimensions are 21 by 2000
# There are 20000 rows and 21 columns 

#1b 

head(loan_default_data_set,0)

#1c

#This is a numeric data set 
#(-2)

#1d

colSums(is.na(loan_default_data_set))

# pct_card_over_50_uti is missing 1958 out of 20000 = 9.97 percent
# rep_income is missing 1559 out of 20000 = 7.795 percent 
# rep_education is missing 1 out of 20000 = 0.005 percent

#1e

# We should take out the mising values so we are still able to run functions properly.

# What are the dimensions of the data ? (-2)

#1f

# I would fit a unsupervised learning model because there is no way to predict based on specific variables
#(-2)
#1g

newdataset <- na.omit(loan_default_data_set)
colSums((is.na(newdataset)))

#2a Data summary statistics

install.packages("dplyr")
library("dplyr")

summary(newdataset)[-4]
#Come back

#2b

mean(newdataset$num_card_inq_24_month)
median(newdataset$num_card_inq_24_month)
frequency(newdataset$num_card_inq_24_month)

newnewdataset = read.csv("C:/Users/rodge/OneDrive/Desktop/DATA 180 -Intoduction to Data Science/DATA-180-Introduction-to-Data-Science--Section-2/data/loan_default_data_set.csv")

mode = function(){
  return(sort(-table(newnewdataset$num_card_inq_24_month))[1])
}

mode()

#Right Skewed

mean(newnewdataset$tot_amount_currently_past_due)
median(newnewdataset$tot_amount_currently_past_due)
frequency(newnewdataset$tot_amount_currently_past_due)
mode = function(){
  return(sort(-table(newnewdataset$tot_amount_currently_past_due))[1])
}

mode()

#Right Skewed

mean(newdataset$credit_age)
median(newdataset$credit_age)
mode = function(){
  return(sort(-table(newnewdataset$credit_age))[1])
}

mode()

#Bell Shaped

#2c

hist(newnewdataset$num_card_inq_24_month, cex.lab=1.2, cex.axis=1.2, col = "blue")
hist(newnewdataset$tot_amount_currently_past_due, cex.lab=1.2, cex.axis=1.2, col = "red")
hist(newnewdataset$credit_age, cex.lab=1.2, cex.axis=1.2, col = "green")

# Yes, these histograms do reflect the mean, median, mode, and the skewness found.

#2d

# Convert it with a vector or factor()

#3a

barplot(sort(table(newdataset$Def_ind), decreasing=T), ylab ="Frequency",xlab="Def_ind", col="blue", cex.names=1.2, cex.axis=1.2 ,cex.lab=1.2)
# This barplot is an exmaple of non numeric data being made into numeric

#3b

barplot(sort(table(newdataset$rep_education), decreasing=T), ylab ="Frequency",xlab="Def_ind", col="green", cex.names=1.2, cex.axis=1.2 ,cex.lab=1.2)
# This barplot shows the number of individuals who have finished college, high school, graduate, or other.

#3c

hist(newnewdataset$rep_income, cex.lab=1.2, cex.axis=1.2, col = "red")

#3d

boxplot(newnewdataset$tot_balance, horizontal = T, col="lightblue", xlab="Balance", cex.axis=1.2)
fivenum(newnewdataset$tot_balance)

med = median(newdataset$tot_balance)
abs_dev = abs(newdataset$tot_balance-med)
mad = 1.4826 * median(abs_dev)

Tmin = med-(3*mad)
Tmax = med+(3*mad)

newdataset$tot_balance[which(newdataset$tot_balance<Tmin | newdataset$tot_balance>Tmax)]
# The only outlier is 0 (-2)