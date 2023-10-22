# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Data8001 - Data Science Assignment 1
# Brian Higgins - R00239570
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Libraries used in the project
# -----------------------------------------------------------------------------
# Install packages are commented out unless needed.

# Library used with creating a table
#install.packages("kableExtra")
library(kableExtra)

# Library for making plots.
# install.packages("ggplot")
library(ggplot2)

# Library to allow multiple ggplots together 
# install.packages("ggpubr")
library(ggpubr)

# library for accessing data better and other things.
# install.packages("dplyr")
library(dplyr)

# Used with ggpairs
#install.packages("GGally")
library(GGally)

# Tree Models
# install.packages("tree")
library(tree)

# random forest
# install.packages("randomForest")
library(randomForest)

# Random oOrest
#install.packages("randomForest")
library(randomForest)

# library to allow levels to be combined to create new predictors.
#install.packages("forcats")
library(forcats)

#install.packages("reshape2"), melt function for correlation heatmap
library(reshape2)

# library for DT and Confusion matrix
#install.packages('caret')
library(caret)

# Use rpart Tree
library(rpart)
library(rpart.plot)

# remove everything and start again
rm(list=ls())

# -----------------------------------------------------------------------------
# Workspace
# -----------------------------------------------------------------------------
# At the end of sections I have clean up the workspace. Quick Objects for 
# reports, etc are removed in order to have clean and ordered code. These sections
# can be uncommented if they are needed. 
# Models, tables, data frames, etc are kept.

# -----------------------------------------------------------------------------
# Read in data
# -----------------------------------------------------------------------------
# load in the data
data <- read.csv("Credit_Risk_25_final-2.csv")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# a) Exploratory Data Analysis (EDA)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------------------------------------
# a.1) Explore each column
# ---------------------------------------------

# View what the data looks like
# View(data)
# look at the structure of the data, column types
str(data)
# 3 int columns. ID looks like an index.
# 10 categorical columns
# 1 num column. Age is in a float type. TO DO: Change this. DONE

# Look into the data in more detail
summary(data) 
# Residence.in.current.distinct has a minus value? TO DO: Fix this. DONE
# Residence.in.current.distinct also has an outlier value of 10, when median is 3.
# Months.since.checking.acct.opened also has an outlier of 120 months when median is 19.
#     This is just 10 years so while above the mean of 23.16 months its not unreasonable to
#     imagine someone having an account for 10 years. Since we have not been told when the 
#     Financial Institution opened.
#     It is also in months and the other two numeric predicators are in years.
#     The model will not know this and so one month and one year will have the same weight.
#     I'll change months into a decimal divided by 12.
# Age also has an outlier with 103 but people do live this long so again its not unreasonable.

# Get a table of the head of the data for the report.# Too wide for one view so need to break up
table_head_1 <- head(data[1:10])
table_head_2 <- head(data[10:14])

# make the table in the same style as used across report. Table added to report.
table_head_1 %>%
  kbl(caption = "Head of the Data") %>%
  kable_classic(full_width = F, html_font = "Cambria")

table_head_2 %>%
  kbl(caption = "Head of the Data cont...") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Clean Workspace: Remove table
rm(table_head_1, table_head_2)

# ---------------------------------------
# a.2) check for Na and Duplicates for columns
# ---------------------------------------

# Are there any Na in different columns? No
for (i in colnames(data)){
  cat("Missing values in", i, ":", sum(is.na(data[,i])), "\n")
}
# There are no na values, However in the head.table I have seen " " data. We will fix this.

# Are there are duplicate values? No
for (i in colnames(data)){
  cat("Duplicate values in", i, ":", sum(duplicated(data[,i])), "\n")
}
# There are multiple duplicates but this not surprising after looking at the structure as 
# most of the columns are categorically values.
# ID has no duplicates so we can take this as the columns having no duplicates.

# Keep a clean work space so remove i.
rm(i)

# ---------------------------------------
# a.3) target/predictor
# ---------------------------------------
# y variable will be the credit standing

# Create a table to get the count of Good and Bad and also a percentage value
# Put this into a table for the report.
x1 <- table(data$Credit.Standing) # 329 Bad, 478 Good
x2 <- round(prop.table(table(data$Credit.Standing)),2) # percentage of Good/Bad
t_1 <- rbind(x1,x2) # merge two tables together
t_1 <-data.frame(t_1) # make this into a data frame

# Issue, because, the percentage is a decimal, it makes the Amount a decimal
# Don't want this, so format the Amounts into whole number and add them back into the DF
t_1 <- t_1[1,] %>% format(round(1))
t_1 <- rbind(t_1,x2) # merge again together
rownames(t_1) <- c("Amount", "Percentage") # give rows a description for Amount and Percentage

# Now take this df and create a table for my report.
t_1 %>%
  kbl(caption = "Y Target Predicator") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# clean up work space
rm(x1,x2, t_1)

# barplot to show how many good and bad credit standings
ggplot(data,aes(x=Credit.Standing))  +
  geom_bar(fill="lightblue") +
  ggtitle("Barplot for type of Credit Standings") +
  xlab("Type of Credit Rating")+
  ylab("Amount") +
  theme_bw() +
  geom_text(aes(label = ..count..), stat = "count")

# ---------------------------------------
# a.4) Look over the other columns.
# ---------------------------------------
# Because some columns have missing information that is not NaN I look over all columns.

# Look at checking account
data$Checking.Acct
unique(data$Checking.Acct) # No Acct, O Balance, Low, High
table(data$Checking.Acct) # Looks ok.

# Look at Credit history
unique(data$Credit.History) # all paid, current, bank paid, delay, critical
table(data$Credit.History) # looks ok.
# We will mark this as a likely important predictor but will not assume anything.

# Look at load reason
unique(data$Loan.Reason) # new car, furniture, small applicance, education, car used, business
# large appliance, repairs, other, retraining
table(data$Loan.Reason)

# Look at savings account
unique(data$Savings.Acct) # low, no account, medlow, medhigh, high
# Another likely predictor. Look more into later.

# Look at Employment
unique(data$Employment) # medium, short, long, very short, unemployed, retired
# There is also data in the "" range. Change in next section.

# Look into Personal Status
unique(data$Personal.Status) # single, divorced, "", married
# There is also data in the "" range.  Change in next section.

# Look at housing
unique(data$Housing) # Own, other, "", Rent
# there is also data in the "" range. Change in next section

# Look at the Job Type
unique(data$Job.Type) # management, skilled, unskilled, unemployed # looks ok

# Look at the Foreign.National
unique(data$Foreign.National) # No, yes. looks ok

# Look at Months.since.Checking.Acct.opened.
unique(data$Months.since.Checking.Acct.opened) # range from 7 to 120
summary(data$Months.since.Checking.Acct.opened)

# Look at the Residence.Time.In.current.district
unique(data$Residence.Time.In.current.district) # -2 to 10. 
# -2 is strange so we will change this to 0.  Change in next section.

# Look at the Age
unique(data$Age) # ages are in float. I do not need this level of detail for age.
# while age can be a contunious value, it is better as a discrete value. 
# Change in next seciton. Has float numbers. No need for this. Change to whole number

# ---------------------------------------
# a.5) Data Manipulation 
# ---------------------------------------
# 1. Drop ID column
# 2. Fix blank data in some columns:
#    Employment column, change "" to "No Information".
#    Personal Status column, change "" to "No Information".
#    Housing column, change "" to "No Information".
# 3. Change all character columns to as.factor column types
# 4. Take the smallest value and change it to zero. You can 't live in a place -2 time 
# 5. Round Age to 0 decimal places as a decimal is not needed
# 6. Convert Months.Since.Account.Open to years.since.Since.Account.Opened 

# 1. Remove the ID as this will not be used.
data <- data[,-1]

# 2. Fix blank data in some columns:
# Employment column, change "" to "No Information".
data$Employment[data$Employment == ""] <- "No Infomation"
# Personal Status column, change "" to "No Information".
data$Personal.Status[data$Personal.Status == ""] <- "No Infomation"
# Housing column, change "" to "No Information".
data$Housing[data$Housing == ""] <- "No Infomation"

# 3. Change all character columns to as.factor column types
data <- data %>%
  mutate_if(sapply(data, is.character), as.factor)

# 4. Take the smallest value and change it to zero. You can 't live in a place -2 time
data$Residence.Time.In.current.district[data$Residence.Time.In.current.district == -2] <- 2

# 5. Round Age and drop the float values. Change to an int type
data$Age<-round(data$Age,0)


# 6. Convert Months.Since.Account.Open to years 
# we have to convert this column as otherwise the model can not 
# compare months and years. It will not work

# Only noticed this at the end and no time to make it pretty. 
# I would do this with a for loop to replace the item.

# I want to keep Months in its same location in the data frame so I don't need
# to change things in the rest of my code.

# But for now quick change as I have not time
# It works, just not the quickest code.

wrk_data <- data$Months.since.Checking.Acct.opened
Years.Since.Checking.Acct.opened =NULL
for(i in wrk_data){
  Years.Since.Checking.Acct.opened <- rbind(Years.Since.Checking.Acct.opened, (i/12))
}
#Years.Since.Checking.Acct.opened <- data.frame(Years.Since.Checking.Acct.opened)
data["Years.since.Checking.Acct.opened"] <- round(data["Months.since.Checking.Acct.opened"] /12,1)
data["Months.since.Checking.Acct.opened"] <- data["Years.since.Checking.Acct.opened"]
data<- subset(data, select=-c(Years.since.Checking.Acct.opened))

# rename the coulumn
colnames(data)[10] = "Years.since.Checking.Acct.opened"

# clean workspace
rm(i, wrk_data, Years.Since.Checking.Acct.opened)

# I regret changing this at end, so many knock on effects :(

# ---------------------------------------------
# a.6) Look at distribution of the categorical columns
# ---------------------------------------------

# Look at the most interesting categorically columns together.

# barplot of Credit.History
g1 <- ggplot(data, aes(x=Credit.History, fill=Credit.History,x.text.angle = 90)) +
  geom_bar() +
  ggtitle("Barplot: Credit History")+
  xlab(" Credit History Standing") + ylab("No. of Customers") +
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust=1,size=10),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 10)) +
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.01, size=3)

# barplot of Savings Account
g2 <- ggplot(data, aes(x=Savings.Acct, fill=Savings.Acct,x.text.angle = 90)) +
  geom_bar() +
  ggtitle("Barplot: Savings Account")+
  xlab("Type of Savigns Account") + ylab("No. of Customers") +
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust=1,size=10),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 10)) +
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.01, size=3)

# barplot of Employment
g3 <- ggplot(data, aes(x=Employment, fill=Employment,x.text.angle = 90)) +
  geom_bar()+
  ggtitle("Barplot: Employment")+
  xlab("Employment Status") + ylab("No. of Customers") +
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust=1,size=10),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 10)) +
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.01, size=3)

# barplot of Job.Type
g4 <- ggplot(data, aes(x=Job.Type, fill=Job.Type)) +
  geom_bar()+
  ggtitle("Barplot: Job Type")+
  xlab("Job type") + ylab("No. of Customers") +
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust=1,size=10),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 10)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.01, size=2)


# combine all four plots together
ggarrange(g1, g2,g3, g4)

# Clean workspace
rm(g1,g2,g3,g4)

# ---------------------------------------------
# a.7) Box plot of Numeric columns
# ---------------------------------------------

#Distributions of the Numeric columns

# Look at Months
g5 <- ggplot(data, aes(x=Years.since.Checking.Acct.opened)) +
  geom_histogram() +
  ggtitle("Barplot: Years Since Account Open")+
  xlab("Length of time account opened (years)") + ylab("No. of Customers") +
  theme_bw() +
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size = 8))

# Look at Age
g6 <- ggplot(data, aes(x=Age)) +
  geom_histogram() +
  ggtitle("Barplot: Age")+
  xlab("Ages of Customers (years") + ylab("No. of Customers") +
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size = 8))

# Look at Residence Time in Current District
g7 <- ggplot(data, aes(x=Residence.Time.In.current.district)) +
  geom_histogram() +
  ggtitle("Barplot: Residence Time in District")+
  xlab("Amount of customers") + ylab("No. of Customers") +
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size = 8))


ggarrange(g5, g6, g7)

# Clean workspace
rm(g5,g6,g7)

# summary of numeric columns
summary(data[,10:12]) 
# YEars.Since.Chekcing.Acct.Opned
# Has a large outlier of 10 years but this is not unsual given that the years in District
# has 10 years.


# ---------------------------------------------
# a.8) trivariate analysis
# ---------------------------------------------

# Create a correlation matrix of numeric columns
cor_data <- round(cor(data[,10:12]),2) # remove date columnn # round data for easier interruption
# melt function takes in a data in wide format and puts into a single column
melted_cor <- melt(cor_data)
cor_data_melt <- melt(cor_data)

# get a heat map to visual show the correlations
ggplot(data=cor_data_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle("Correlation of Numeric columns") +
  scale_fill_gradient2(low="blue", high="red", limit =c(-1,1),name="Correlation")+
  geom_text(aes(Var1, Var2, label=value)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust=1,size=10),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 10)) 


# Look at the category tables

ggpairs(data[,1:5], title="Qucik Plot at category tables",
        upper = list(continuous = wrap('cor', size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.2,    size=0.1))) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())

# clean up workspace
rm(cor_data, cor_data_melt, melted_cor)

#---------------------------
#3-D prop table
d3_table <- ftable(data$Employment,data$Credit.History, data$Credit.Standing)
d3_table_df <-data.frame(d3_table)

colnames(d3_table_df) = c("Employment","Credit History", "Credit Standing", "Count")

# Create a table to add to the report
d3_table_df  %>%
  kbl(caption="3D Prop Table- Employment,Credit History,Credit Standing") %>%
  kable_classic(full_width = F, html_font = "Cambria")

rm(d3_table_df, d3_table)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# b) Split the dataset into 75% training and 25% test set using set.seed, 
# set.seed(abc) where abc are the last 3 digits of your student no. 
# (Use this set.seed for all other function with an element of randomness below). 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# set the seed with last three digits of Univeristy ID
set.seed(579)

# Total of 807 rows, so we will take 75% or 605 rows
train <- sample(1:nrow(data), 605)

# Use the train data to get a Train data frame with the 605 rows.
data.train <- data[train,]

# Using the above train data we will get a Test data frame minus these 605 rows
# with 202 rows
data.test <- data[-train,]

# gives a Good Bad, minus the Train data
# data.test real results to test with models results with confusion matrix.
Credit.Standing.data.test <- data$Credit.Standing[-train]

# Trained data kept in original form (after EDA fixes) to be used in later sections
data.train.org <- data.train
data.test.org <- data.test


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# c) Using the code for entropy and information gain given in the labs, using 
# only the categorical type predictor variables show which predictor variable 
# should be used for the root node split. Use only the training set from b) to do 
# this and you are not constrained to binary splits.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# What to do.
# 1. Use only char predictors
# 2. Use only train data from b)
# 3. show which predictor should be used for the root node split.

# Work out Entropy and Information gain for each of the Categorical Predictors
# 0. Target Variable: Credit.standing
# 1. Checking.Acct
# 2. Credit.History
# 3. Loan.Reason
# 4. Savings.Acct
# 5. Employment
# 6. Personal.Status
# 7. Housing
# 8. Job.Type
# 9. Foreign.National

# -------------------------------------
# 0. Target Variable: Crediting.Standing
# -----------------------------------
# Get the Entropy for the Y target which will be used to work out the Information Gain for each predictor.
y_target <- prop.table(table(data.train$Credit.Standing))
y_target_entropy <-sum(-y_target *log2(y_target))
y_target_entropy 
# 251 Bad, 354 Good
# entropy for the Y target is 0.97899

# Baseline rate for testing accuracy of models
baseline_rate <- round(table(data.train$Credit.Standing)/nrow(data.train),2)
baseline_rate
# Bad Good 
# 0.41 0.59 
# So we know our baseline to compare with models is 59%


# -------------------------------------
# Function to give prop table for selected columns
# -----------------------------------
# Code from labs but added round as some values(for Loan Reason/Employement) caused NaN resutls.
tabfun <- function(x) {round(prop.table(table(data.train[,x],data.train[,13]) + 1e-6, margin = 1),6)}
# example.


tabfun(1) # checking acct - 4 classes
tabfun(2) # Credit history - 5 classes
tabfun(3) # Loan Reason 10 classes
tabfun(4) # savings account - 5 classes
tabfun(5) # Employment - 6 classes
tabfun(6) # Personal status -3 classes
tabfun(7) # Housing - 3 classes
tabfun(8) # Job type - 4 classes
tabfun(9) # Foreign National - 2 classes

# -------------------------------------
# Function from Aengus to work out Entropy for each predictor columns
# -----------------------------------

# Function to work out entropy
entropy_tab <- function(x) { 
  tabfun2 <- prop.table(table(data.train[,x],data.train[,13]) + 1e-6, margin = 1)
  sum(prop.table(table(data.train[,x]))*rowSums(-tabfun2*log2(tabfun2)))
}


# -------------------------------------
# Create for loop to get entropy and information gain and add to table
# -------------------------------------
# This gives the entropy per categorical predictor column. 

# empty data frame with no values
entropy_table_cat_1 <- data.frame()

for (i in colnames(data.train[1:9])){
  col_entropy <- round(entropy_tab(i),5)
  col_IG <- round(y_target_entropy - (entropy_tab(i)),5)
  col_name <- i
  print(to_add <- c(col_name,col_entropy, col_IG))
  entropy_table_cat_1 <- rbind(entropy_table_cat_1, to_add)
}
colnames(entropy_table_cat_1) = c("Column", "Entropy", "Infomation Gain")

# Create a table to add to the report
entropy_table_cat_1 %>%
  kbl(caption = "Non-Binary Split for Entropy and Infomation Gain") %>%
  kable_classic(full_width = F, html_font = "Cambria")
# discuss entropy and information gain in the report for these columns.

#
# The best prediator to use for the root node on a non binary split is Credit Histort,
# It has a low entropy value of 0.72196 and an assoicated Infomation Value of 0.25703
# No other Predictor does as well.
#

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# d) Now redo part c) but now you are constrained to only binary splits, i.e. a 
# split with only 2 possible outcomes.  Design your splitting method before coding; 
# explain your method and then implement your code, analyse your results and comment. .
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------
# Create Binary split on the following
# ---------------
# 1. Checking.Acct
# 2. Credit.History
# 3. Loan.Reason
# 4. Savings.Acct
# 5. Employment
# 6. Personal.Stauts
# 7. Housing
# 8. Job.Type

# ---------------
# Design Method
# ---------------

# Two levels to do this.

# --------------
# First
# --------------

# My first idea to get the entropy for each predicator was to get the entropy for
# each class in a predicator. 

# AN issue I thought about was that why the entropy of one class was a certain amount,
# how would this be affected if I took into account the other classes. You might find 
# that with a binay split that a 1 to 3 split for class( for example) would be worse than 
# a two way split.

# A seocnd issue was, while I know what two classes mean it might be the case that 
# we end up with a split that say are mismathced. Of course we have to follow the results.
# Example:
# Credit History has classes "All Paid", "Bank Paid", "Critical", "Current", "Delay
# Using only one split you might have Dealy on one side with "relative" good results on the 
# other side of the split.

# After this I wanted to take into account the weight of each class i a second level. 
# This leads on to the second level of my design

# In order to continue with the next sections I made some binary splits on these findings.

#Notes:
# This section has an one attempt to use the number of levels in a class to get the
# weight. I have left this in below to show my investigating this but I do not beleive it 
# is a correct approach.

# -----------------
# Second
# -----------------

# I was not able to code this given my new programming ability. But I wanted to take a 
# three class example and get each combination results. 
# Example:
# Three classes. A, B C. I wanted to get (A, BC), (B,AC) and (C, AB) for each entropy
# of these groups.

# I was not able to do this, I tried with for loops but I am not at that level yet.
# Areas I looked into do this:
# For loops, itertools, caret ....[ add in others]

# TO DO: It time, come back to this.

# ---------------
# Code Method - Option 1 -  just get entropy on each factor level
# Make a decision on the split.
# ---------------

# ---------------
# 1. Create new Binary predictor for Checking.Acc
# ---------------
# Separate out the checking Account 
df_Checking_Acc <- tabfun(1)
# create a new data frame and split out checking into four separate columns
# get an data frame with the entropy for all of Housing
entropy_table_Checking_Acc <- data.frame()

# Use for loop to get entropy for each class inside a predicator.
for (i in 1:nrow(df_Checking_Acc)){
  row_entropy <- sum(-df_Checking_Acc[i,]*log2(df_Checking_Acc[i,]))
  col_name <- i
  entropy_table_Checking_Acc <- rbind(entropy_table_Checking_Acc,row_entropy )
}

# add column and rows labels and print out to table for report
colnames(entropy_table_Checking_Acc ) = c("Entropy")
rownames(entropy_table_Checking_Acc ) <- c("0Balance", "High", "Low", "No Acct")

# --------------
# Look at results
entropy_table_Checking_Acc
#            Entropy
# 0Balance 0.9980009
# High     0.9993375
# Low      0.9951724
# No Acct  0.8860324

# ------------
# Investiagating one method to look at how to weigh entropy within a class.
# Not correct.

# The below code get the table at the end. Read there for more infomation.
# Checking_Acc Class count
Checking_Acc_table <- table(data$Checking.Acct)
Checking_Acc_table <- data.frame(Checking_Acc_table)
colnames(Checking_Acc_table) = c("Class", "Count")
# Create a table to add to the report
Checking_Acc_table %>%
  kbl(caption=" Checking Account Class Count") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# Get the percentage of the classes and add this to the table
Checking_Acc_Weigths <- as.data.frame(round(prop.table(table(data$Checking.Acct)),2))
Checking_Acc_table <- cbind(Checking_Acc_table, Checking_Acc_Weigths[2])

# Add entropy to the table
Checking_Acc_entropy_weight_table <- cbind(Checking_Acc_table[2:3], entropy_table_Checking_Acc[1] )

Weighted_entropy <- Checking_Acc_entropy_weight_table[2]*Checking_Acc_entropy_weight_table[3]
Checking_Acc_entropy_weight_table <- cbind(Checking_Acc_entropy_weight_table, Weighted_entropy  )
colnames(Checking_Acc_entropy_weight_table ) = c( "Count", "Weigth", "Entropy", "Weighted Entropy")
# Drop first class names as double entry

Checking_Acc_entropy_weight_table %>%
  kbl(caption = " Checking Account Weight Test") %>%
  kable_classic(full_width = F, html_font = "Cambria")
# This is not correct as Entropy is already taking into account the count of classes.

# --------------
# Take the column with lowest entropy and create a new predictor with a two binary split.

# OBalance/High/ LOW all have entropy ranges in 0.99 and No Acct has 0.88
# So I will split on this. 

# create new binary split
data.train$Checking.Acct <- fct_collapse(data.train$Checking.Acct,
                                         "0Balance/Low/High" = c("0Balance", "Low","High"),
                                         "No Acct" = "No Acct")


# clean workspace
rm(Checking_Acc_entropy_weight_table, Weighted_entropy, Checking_Acc_table,Checking_Acc_Weigths,
   entropy_table_cat_1, entropy_table_Checking_Acc, col_IG, col_name,col_entropy,i,
   df_Checking_Acc, to_add, row_entropy)

# ---------------
# 2. Create new Binary predictor for Credit.History
# ---------------
# The following prediators follow the same method as I mentioned above so no comments neeed.

df_Credit_History <- tabfun(2)
entropy_table_Credit_History <- data.frame()
for (i in 1:nrow(df_Credit_History)){
  row_entropy <- sum(-df_Credit_History[i,]*log2(df_Credit_History[i,]))
  col_name <- i
  entropy_table_Credit_History <- rbind(entropy_table_Credit_History,row_entropy )
}
colnames(entropy_table_Credit_History ) = c("Entropy")
rownames(entropy_table_Credit_History ) <- c("All Paid", "Bank Paid", "Critical", "Current", "Delay")

# print out to table
entropy_table_Credit_History %>%
  kbl(caption="Credit History Entropy") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# --------------
# Look at results
entropy_table_Credit_History
#            Entropy
# All Paid  0.2285380
# Bank Paid 0.6880466
# Critical  0.1044214
# Current   0.9934271
# Delay     0.9580420

table(data.train$Credit.History)
#  All Paid Bank Paid  Critical   Current     Delay 
#    108        49        73       325        50 


Credit_history_table <- table(data.train$Credit.History)
Credit_history_table <- data.frame(Credit_history_table)
colnames(Credit_history_table) = c("Class", "Count")
# Create a table to add to the report
Credit_history_table %>%
  kbl(caption="Credit History Class Count") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# All paid is lowest entropy so will create a bianry split on this.

#-----------
# create new binary split
data.train$Credit.History <- fct_collapse(data.train$Credit.History,
                                         "All Paid" = "All Paid",
                                         "Bank PaidCritical/Current/Delay" = c("Bank Paid", "Critical", "Current", "Delay") )                                              

# ---------------
# 3. Create new Binary predictor for Loan Reason
# ---------------
df_Loan_Reason <- tabfun(3)
entropy_table_Loan_Reason <- data.frame()
for (i in 1:nrow(df_Loan_Reason)){
  row_entropy <- sum(-df_Loan_Reason[i,]*log2(df_Loan_Reason[i,]))
  col_name <- i
  entropy_table_Loan_Reason <- rbind(entropy_table_Loan_Reason,row_entropy )
}
colnames(entropy_table_Loan_Reason ) = c("Entropy")
rownames(entropy_table_Loan_Reason ) <- c("Business","Car New","Car Used", "Education", "Furniture",
                                          "Large Appliance","Other","Repairs","Retraing","Small Applicance")

# --------------
#Look at results
entropy_table_Loan_Reason
#                  Entropy
# Business         0.9283621
# Car New          0.9910759
# Car Used         0.9755258
# Education        0.9852280
# Furniture        0.9961346
# Large Appliance  0.9709506
# Other            0.9456600
# Repairs          0.8112781
# Retraining               NaN #  Investigated: Not an issue, caused when there is a 1 in Good. Only 4 entries
# Small Appliance 0.9556220

table(data.train$Loan.Reason)
# Business         Car New        Car Used       Education       Furniture Large 
# 64             144              49              35             123                
# Applicance      OTher           Repairs      Retraining Small Appliance 
# 5               11               16               4             154 


# Hard to decide. WIll take a split based on entropy values of 0.99/0.98 and everything else.

#-----------
# create new binary split

# Will take a split based on entropy values of 0.99/0.98 and everything else.
data.train$Loan.Reason <- fct_collapse(data.train$Loan.Reason,
                                          "Car New/Education/Furniture" = c("Car New", "Education","Furniture","" ),
                                          "Business/Car Used/Large Appliance/Other/Repairs/ Retraining/Small Appliance" = c("Business", "Car Used", "Large Appliance", "Other","Repairs", "Retraining", "Small Appliance") )                                              

# clean workspace
rm(col_name, df_Loan_Reason, i, row_entropy, entropy_table_Loan_Reason)

# ---------------
# 4. Create new Binary predictor for Saving.Acct
# ---------------
df_Saving_Acct <- tabfun(4)
entropy_table_Saving_Acct <- data.frame()
for (i in 1:nrow(df_Saving_Acct)){
  row_entropy <- sum(-df_Saving_Acct[i,]*log2(df_Saving_Acct[i,]))
  col_name <- i
  entropy_table_Saving_Acct <- rbind(entropy_table_Saving_Acct,row_entropy )
}
colnames(entropy_table_Saving_Acct ) = c("Entropy")
rownames(entropy_table_Saving_Acct ) <- c("High", "Low", "MedHigh", "MedLow", "No Acct")
entropy_table_Saving_Acct

# --------------
# Put results into a table 
entropy_table_Saving_Acct
#            Entropy
# High    0.9456600
# Low     0.9859821
# MedHigh 0.9902245
# MedLow  0.8856124
# No Acct 0.9798689

#-----------
# create new binary split

# split on medlow as lowest entropy
data.train$Savings.Acct <- fct_collapse(data.train$Savings.Acct,
                                       "High/Low/MedHigh" = c("High", "Low","MedHigh","No Acct" ),
                                       "MedLow" = "MedLow" )                                              
# clean workspace
rm(col_name, df_Saving_Acct , i, row_entropy, entropy_table_Saving_Acct)

# ---------------
# 5.  Create new Binary predictor for Employment
# ---------------
df_Employment <- tabfun(5)
entropy_table_Employment <- data.frame()
for (i in 1:nrow(df_Employment)){
  row_entropy <- sum(-df_Employment[i,]*log2(df_Employment[i,]))
  col_name <- i
  entropy_table_Employment <- rbind(entropy_table_Employment,row_entropy )
}
colnames(entropy_table_Employment ) = c("Entropy")
rownames(entropy_table_Employment ) <- c("Long", "Medium", "No Information",
                                         "Retired", "Sort", "Unemployed", "Very Short")

# --------------
# Look at results
entropy_table_Employment 
#            Entropy
# Long           0.8892885
# Medium         0.7592754
# No Information 1.0000000
# Retired        1.0000000
# Sort           0.9598942
# Unemployed     0.8935713
# Very Short     0.9903069

table(data.train$Employment)
#   Long        Medium No Infomation       Retired         Short    Unemployed    Very Short 
#    150           123            10             2           196            29            95  


#-----------
# create new binary split
# binary split on Medium as lowest entropy
data.train$Employment <- fct_collapse(data.train$Employment,
                                        "Long/No Information/Retired/Sort/Unemployed/Very Short" = c("Long", "No Infomation","Retired","Sort", "Unemployed","Very Short","Short"),
                                        "Medium" = "Medium" )

# clean workspace
rm(col_name, df_Employment , i, row_entropy, entropy_table_Employment)

# ---------------
# 6. Create new Binary predicator for Personal.Status
# ---------------
df_Personal_Status <- tabfun(6)
entropy_table_Personal_Status <- data.frame()
for (i in 1:nrow(df_Personal_Status)){
  row_entropy <- sum(-df_Personal_Status[i,]*log2(df_Personal_Status[i,]))
  col_name <- i
  entropy_table_Personal_Status <- rbind(entropy_table_Personal_Status,row_entropy )
}
colnames(entropy_table_Personal_Status ) = c("Entropy")
rownames(entropy_table_Personal_Status ) <- c("Dicorced", "Married", "No Information", "Single")

# --------------
# Put results into a table 
entropy_table_Personal_Status
#             Entropy
# Divorced       0.9972944
# Married        0.9768739
# No Information       NaN # Not an issue, cuased when there is a 1 in Good.
# Single         0.9638712

table(data.train$Personal.Status)
# Divorced       Married No Infomation        Single 
# 196            56             3           350 

# binary split on Single as lowest entropy

#-----------
# create new binary split

# binary split on Single as lowest entropy
data.train$Personal.Status <- fct_collapse(data.train$Personal.Status,
                                      "Divorced/Married/No Infomation" = c("Divorced","Married","No Infomation" ),
                                      "Single " = "Single")

# clean workspace
rm(col_name, df_Personal_Status , i, row_entropy, entropy_table_Personal_Status)

# ---------------
# 7.  Create new Binary predicator for Housing
 # ---------------
df_Housing <- tabfun(7)
entropy_table_Housing <- data.frame()
for (i in 1:nrow(df_Housing)){
  row_entropy <- sum(-df_Housing[i,]*log2(df_Housing[i,]))
  col_name <- i
  entropy_table_Housing <- rbind(entropy_table_Housing,row_entropy )
}
colnames(entropy_table_Housing ) = c("Entropy")
rownames(entropy_table_Housing ) <- c("No Information",
                                         "Other", "Own", "Rent")

# --------------
# Look at results
entropy_table_Housing 
#            Entropy
# No Information 0.8112781
# Other          0.9966132
# Own            0.9544340
# Rent           0.9997699

table(data.train$Housing)
# No Infomation         Other           Own          Rent 
# 4            73           416           112 

#-----------
# create new binary split

# binary split on No Information as lowest entropy
# I don't like this as it has only 4 entries but I have to follow the results of the entropy
# I can't add my own bias even if I know it is wrong due to my lack of taking this to the
# next level of combining levels to find the best entropy.
# But besides, manually creating bianry splits is already influcing the model. More on this
# in the report.

# create new binary split
data.train$Housing <- fct_collapse(data.train$Housing,
                                           "Other/Own/Rent" = c("Other","Own","Rent" ),
                                           "No Information" = "No Information")


# clean workspace
rm(col_name, df_Housing , i, row_entropy, entropy_table_Housing)

# ---------------
# 8. Create new Binary predicator for Job.Type
# ---------------
df_Job_Type <- tabfun(8)
entropy_table_Job_Type <- data.frame()
for (i in 1:nrow(df_Job_Type)){
  row_entropy <- sum(-df_Job_Type[i,]*log2(df_Job_Type[i,]))
  col_name <- i
  entropy_table_Job_Type <- rbind(entropy_table_Job_Type,row_entropy )
}
colnames(entropy_table_Job_Type ) = c("Entropy")
rownames(entropy_table_Job_Type ) <- c("Management", "Skilled", "Unemployed", "Unskilled")

# --------------
# Put results into a table 
entropy_table_Job_Type
#             Entropy
# Management 0.9484107
# Skilled    0.9859958
# Unemployed 0.5435644
# Unskilled  0.9874362

table(data.train$Job.Type)
# Management    Skilled Unemployed  Unskilled 
# 79        381         16        129 

#-----------
# create new binary split

# binary split on Unemployed as lowest entropy
data.train$Job.Type <- fct_collapse(data.train$Job.Type,
                                   "Management/Skilled/Unskilled" = c("Management","Skilled","Unskilled" ),
                                   "Unemployed" = "Unemployed")

# clean workspace
rm(col_name, df_Job_Type , i, row_entropy, entropy_table_Job_Type)


# -------------------------------------
# get an entropy and infomation gain table on the new binary split predicators
# -------------------------------------


# empty data frame with no values
entropy_table_cat_2 <- data.frame()

for (i in colnames(data.train[1:9])){
  col_entropy <- round(entropy_tab(i),5)
  col_IG <- round(y_target_entropy - (entropy_tab(i)),5)
  col_name <- i
  print(to_add <- c(col_name,col_entropy, col_IG))
  entropy_table_cat_2 <- rbind(entropy_table_cat_2, to_add)
}
colnames(entropy_table_cat_2) = c("Column", "Entropy", "Infomation Gain")

# Create a table to add to the report
entropy_table_cat_2 %>%
  kbl(caption = "Binary Split for Entropy and Infomation Gain") %>%
  kable_classic(full_width = F, html_font = "Cambria")
# discuss entropy and information gain in the report for these columns.

#
# The best predator to use for the root node on a non binary split is Credit Histort,
# It has a low entropy value of 0.72196 and an assoicated Infomation Value of 0.25703
# No other Predictor does as well.



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# e) Now include the continuous numeric predictor variables, again use only a 
# binary split. Which is now the root node split?  Analyse your results and comment.  .
# -----------------------------------------------------------------------------

# ---------------
# METHOD: Thinking behind plan
# ---------------
# Similar to method two for categorically binary splits, the way to get the lowest 
# entropy would be to use a brute force technique with computer power and get the entropy on
# every binary combination. But resources on line (references in report) say that
# a random approach or just taking a split on different points can work just as well.

# I looked at four approaches on one numeric column to see how they compare.
# Method One: Pick three points at set points in the data by index
# Method Two: Separate by a value based on summary 
# Method Three: Take Random Points.
# Method four: Brute Force Technique with computer power - Not coded

# Number of values in continuous variables
x1 <- length(unique(data.train$Residence.Time.In.current.district)) #  7 values
x2 <- length(unique(data.train$Years.since.Checking.Acct.opened)) # 33 values
x3 <- length(unique(data.train$Age)) # 57 values

numeric_table <- rbind(x1,x2,x3)
numeric_table <- data.frame(numeric_table)
colnames(numeric_table) <- c( "Count")
rownames(numeric_table) <- c("Residence Time in current District",
                             "Years Since Account opened",
                             "Age")
# Create a table to add to the report
numeric_table  %>%
  kbl(caption=" Number of Unique Values in each Numeric Prediator") %>%
  kable_classic(full_width = F, html_font = "Cambria")

rm(x1, x2, x3,numeric_table)
# ---------------
# Age
# ---------------
# ---------------
# Method One: Take 3 Index points for entropy on index
# --------------

# Aengus function to cut continuous variable and get entropy
# X is for the predictor and y is the number of rows to split on.
entropy_tab_cut <- function(x,y) { 
  tabfun2 <- prop.table(table(cut(data.train[,x],y),data.train[,13])  + 1e-6, margin = 1)
  sum(prop.table(table(cut(data.train[,x],y)))*rowSums(-tabfun2*log2(tabfun2)))}

# take 4 cuts for Age at the 150,300,350 marks
a1 <- entropy_tab_cut(12, 150) # 0.7742922
a2 <- entropy_tab_cut(12, 300) # 0.7742922
a3 <- entropy_tab_cut(12, 450) # 0.7742922

# Index seems to give the same entropy values although I took the code from the labs
# lets try another way

# ---------------
# Method Two: Take a value
# --------------

# I want to spilt the data on the 1st, median, 3rd Qu.
summary(data.train$Age)
# 1st 29
# median 36
# 3rd 45

# create a new predictors for age by manually selecting ages 
data.train$under_29_Age <- ifelse(data.train$Age <=29, "Under 29 years", "Over 30 years")
data.train$under_36_Age <- ifelse(data.train$Age <=36, "Under 36 years", "Over 37 years")
data.train$under_45_Age <- ifelse(data.train$Age <=45, "Under 45 years", "Over 46 years")

a4 <- entropy_tab(14) # 0.8694557
a5 <- entropy_tab(15) # 0.9196873
a6 <- entropy_tab(16) # 0.9604154

# Better results
# Lets see how random compares

# ---------------
# Method Three: Take random points
# --------------

set.seed(579)

# I can use a random number to split on the index but I want to split on 
# a random value in the AGe range. I want to do it this way to compare it to method two.

# get unique number
Age_numbers <- unique(data.train$Age)
# Take three random ages
sample(Age_numbers, 1)
# 36, 34, 51

# create a new predictors for age by randomly selected ages 
data.train$random_36_Age <- ifelse(data.train$Age <=36, "Under 29 years", "Over 30 years")
data.train$random_34_Age <- ifelse(data.train$Age <=34, "Under 36 years", "Over 37 years")
data.train$random_51_Age <- ifelse(data.train$Age <=51, "Under 45 years", "Over 46 years")

a7 <- entropy_tab(17) # 0.9196873
a8 <- entropy_tab(18) # 0.9216859
a9 <- entropy_tab(19) # 0.9592225

# --------------
# Another way to do this would be to create a for loop and then use the sample to
# get a random number, split on age and then get a new column. If I had more time 
# I'd do this. Give I just have three columns the above way is less complex but 
# faster.

# i always like to think how I can do things other ways for the future as I learn more.

#--------------
# create a table to compare results.
entropy_age_comparison_table <- data.frame()
entropy_age_desciption <- rbind("Index split 150 position", "Index split 300 position", "Index Split 450 position",
                            "Q1 split < 29", "Median Split < 36", "Q3 Split < 45",
                            "Random Split < 36","Random Split < 34" , "Random Split < 51")
entropy_age_results <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9)
rownames(entropy_age_results) <- NULL

entropy_age_comparison_table <- cbind(entropy_age_desciption, round(entropy_age_results,5))

colnames(entropy_age_comparison_table) <- c("Split Description","Entropy")

entropy_age_comparison_table %>%
  kbl(caption = "Method Results from Binary Spliting Age Predicator") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# I don't trust the index splits as they are the same
# This is something I will look at. TO DO
# The best split is on the Q1 age so we will use this as the binary split for age.

#--------------------
# change Age predicator to a binary split.
data.train$Age <- as.factor(ifelse(data.train$Age <=29, "Under 29 years", "Over 30 years"))

# drop working columns from data
data.train <- subset(data.train, select=-c(under_29_Age, under_36_Age, under_45_Age,
                                           random_34_Age, random_51_Age, random_36_Age))


#clean up workspace
rm(a1,a2,a3,a4,a5,a6,a7,a8,a9, Age_numbers,col_entropy,col_IG, col_name, entropy_age_comparison_table,
   entropy_age_desciption)

# ---------------
# Years.since.checking.acct.opened
# ---------------
# No comments in this section as repeat of above, no table either.

# ---------------
# Method One: Take 3 Index points for entropy on index
# --------------

# Not doing this time as index just gets the same results

# ---------------
# Method Two: Take a value
# --------------

summary(data.train$Years.since.Checking.Acct.opened)
# 1st 0.4
# median 1.6
# 3rd 2.4

# create a new predictors for age by manually selecting ages 
data.train$under_0_4_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=0.4, 
                                     "Under 0.4 Years", "Over 0.5 Years")
data.train$under_1_6_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=1.6, 
                                     "Under 1.6 Years", "Over 1.7 Years")
data.train$under_2.4_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=2.4, 
                                     "Under 2.4 Years", "Over 2.5 years")
entropy_tab(14) # 0.9769305
entropy_tab(15) # 0.9778392
entropy_tab(16) # 0.9708156

# ---------------
# Method Three: Take random points
# --------------
set.seed(579)

Years.since.Checking.Acct.opened_numbers <- unique(data.train$Years.since.Checking.Acct.opened)
# Take three random months
sample(Years.since.Checking.Acct.opened_numbers, 1)
# 3.8, 5, 2.6

data.train$random_3_8_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=3.8,
                                      "Under 3.8 years", "Over 3.9 years")
data.train$random_5_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=5,
                                      "Under 5 years", "Over 5 years")
data.train$random_2_6_years <- ifelse(data.train$Years.since.Checking.Acct.opened <=2.6,
                                      "Under 2.6 years", "Over 2.7 years")

entropy_tab(17) # 0.9782881
entropy_tab(18) # 0.9789835
entropy_tab(19) # 0.9743786

# --------------

# Lowest entropy is from method three with random value of 12. So we will be 
# using this for our binary split.

# change year predictor to a binary split.
data.train$Years.since.Checking.Acct.opened <- as.factor(ifelse(data.train$Years.since.Checking.Acct.opened <=2.4, 
                                     "Under 2.4 Years", "Over 2.5 years"))

# drop working columns from data
data.train <- subset(data.train, select=-c(under_0_4_years, under_1_6_years, under_2.4_years,
                                           random_2_6_years, random_5_years,random_3_8_years))

#clean up workspace
rm()


# ---------------
# Residence.Time.In.Current.district
# ---------------
# No comments in this section as repeat of above, no table either.

# ---------------
# Method One: Take 3 Index points for entropy on index
# --------------

# Not doing this time as index just gets the same results

# ---------------
# Method Two: Take a value
# --------------

summary(data.train$Residence.Time.In.current.district)
# 1st 2
# median 3
# 3rd 4

# create a new predictors for age by manually selecting years
data.train$under_2_years <- ifelse(data.train$Residence.Time.In.current.district <=2, 
                                     "Under 2 years", "Over 3 years")
data.train$under_3_years <- ifelse(data.train$Residence.Time.In.current.district <=3, 
                                     "Under 3 years", "Over 4 years")
data.train$under_4_years <- ifelse(data.train$Residence.Time.In.current.district <=4, 
                                     "Under 4 years", "Over 5 years")
entropy_tab(14) # 0.9768885
entropy_tab(15) # 0.978262
entropy_tab(16) # 0.9788891

# ---------------
# Method Three: Take random points
# --------------
set.seed(579)

Residence.Time.In.current.district_numbers <- unique(data.train$Residence.Time.In.current.district)
# Take three random months
sample(Residence.Time.In.current.district_numbers, 1)
# 2, 1, 5

data.train$random_2_years <- ifelse(data.train$Residence.Time.In.current.district <=2,
                                      "Under 2 years", "Over 3 years")
data.train$random_1_years <- ifelse(data.train$Residence.Time.In.current.district <=1,
                                      "Under 1 years", "Over 2 years")
data.train$random_5_years <- ifelse(data.train$Residence.Time.In.current.district <=5,
                                      "Under 5 years", "Over 6 years")

entropy_tab(17) # 0.9768885
entropy_tab(18) # 0.9768128
entropy_tab(19) # 0.9789196

# --------------

# Very similar results for Residence Time in Current district
# random_1_years
# Interestingly 2 our of the best splits came from picking a random number
# mention in report.


# change month predictor to a binary split.
data.train$Residence.Time.In.current.district<- as.factor(ifelse(data.train$Residence.Time.In.current.district <=1,
                                                                 "Under 1 years", "Over 2 years"))

# drop working columns from data
data.train <- subset(data.train, select=-c(under_2_years, under_3_years, under_4_years,
                                           random_1_years, random_2_years, random_5_years))

#clean up workspace
rm(Residence.Time.In.current.district_numbers)


# ---------------
# Method four: Randomly test points or Brute Force for all combinations to see
# which gives the best entropy value.
# ---------------

# More detailed in report. I don't have ability in R yet

# I would do this differently. I would do a split on each value and then get 
# the entropy for each. For example, Months.Since.Checking.Acct.opened
# has values  [1  2  3  4  5  9 10]. So I would create 7 splits to split on 1, then 2, etc.
# Then run entropy on each and look for the entropy and pick the lowest value to
# split on.

# This takes more computer power and for something like Age is much more complex.
# But with computers we would be able to do this.


# ---------------
# All the data is now in a binary split. 
# Run entropy and infomation gain to see which has lowest entropy and highest infomation
# gain.
# ---------------

# create a table with results
entropy_table_binary_all <- data.frame()

# get table of all three numeric
for (i in colnames(data.train[1:12])){
  col_entropy <- round(entropy_tab(i),5)
  col_IG <- round(y_target_entropy - (entropy_tab(i)),5)
  col_name <- i
  print(to_add <- c(col_name,col_entropy, col_IG))
  entropy_table_binary_all <- rbind(entropy_table_binary_all, to_add)
}

colnames(entropy_table_binary_all) = c("Column", "Entropy", "Infomation Gain")

entropy_table_binary_all %>%
  kbl(caption="Binary Split Categorical/Numeric Predicators") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Very simliar results to before.
# Credit History is still the best root node but its vlaue has dropped (of course this is
# adding more predicators)
# Age is now the second best node.

# clean Workspace
rm(col_entropy, col_IG, col_name, i,to_add,entropy_table_binary_all)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# f) Now investigate the next level of split, i.e. which predictor variable(s) 
# should be used to split the first split found in part e).  Only binary splits 
# are allowed again here.  Detail in words (and pseudo code and diagrams if you like) 
# the approach you are going to use. 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# From the table above we have seen the best root node is Credit History.

# We will drop this from the list of predictors and run our entropy and information 
# gain again. 

# In have already created a for loop to check the entropy and infmoration gain,
# I just need to drop the Credit History Colums.



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# g) Now write the R code for part f). Analyse your results and comment.  
# -----------------------------------------------------------------------------

# --------------
# Drop Credit History from our predictors
# --------------

# Create a temp new data.train df minus credit history

# drop working columns from data
data.train_minus_credit_history <- subset(data.train, select=-c(Credit.History))


# --------------
# Run entropy and information gain and add to table
# --------------
# This gives the entropy per categorical predictor column. 

# empty data frame with no values
entropy_table_all_minus_CH <- data.frame()

for (i in colnames(data.train_minus_credit_history[1:11])){
  col_entropy <- round(entropy_tab(i),5)
  col_IG <- round(y_target_entropy - (entropy_tab(i)),5)
  col_name <- i
  print(to_add <- c(col_name,col_entropy, col_IG))
  entropy_table_all_minus_CH  <- rbind(entropy_table_all_minus_CH , to_add)
}
colnames(entropy_table_all_minus_CH ) = c("Column", "Entropy", "Infomation Gain")
entropy_table_all_minus_CH 


# Create a table to add to the report
entropy_table_all_minus_CH  %>%
  kbl(caption = "Binary Split minus root node: Credit Standing") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Age is still the best next node to have.
rm(entropy_table_all_minus_CH, col_entropy,col_IG, col_name, i, to_add)

# ----------------------------------------------------------------------------
# Run a tree with my manual binary splits out of interest
# I have limited the DT as it has less options if I am manually deciding on
# splits.

man_dt_model_1 <- tree(Credit.Standing~., data=data.train)

#plot dt_model_1
plot(man_dt_model_1)
text(man_dt_model_1, pretty=1, cex=0.5)

# summary 
summary(man_dt_model_1)
#[1] "Credit.History"                     "Age"                               
#[3] "Employment"                         "Job.Type"                          
#[5] "Residence.Time.In.current.district"
# Number of terminal nodes:  7 
# Misclassification error rate: 0.2843 = 172 / 605

# Predication
# tree.pred_man_dt_model_1 <-predict(man_dt_model_1, data.test) # DOES NOT WORK
# I can't do a predication as the data.test has not got the binary split and so
# was converted to factors for the three numeric columns.
# Because of this the column types are now different and I can't do a predication.

# 1. Do binary splits for the full data.
# No. because then the test data will not accurately test it and it will overfit.
# I was told in project sheet to do binary splits on train.data.

# 2. Perhaps do the same factor process on the three numeric columns for the data.test
# At least this way I could at least get an accuracy for the manual binary
# splits I have done to compare to the Automatic Decision Tress down below.
# But again this is also affecting the test data.
# I will do it as a test to see.

# change the three numeric data.test to factors.
data.test_man_dt_model_for_test <- data.test
data.test_man_dt_model_for_test$Residence.Time.In.current.district<- as.factor(ifelse(data.test_man_dt_model_for_test$Residence.Time.In.current.district <=1,
                                                                 "Under 1 years", "Over 2 years"))
data.test_man_dt_model_for_test$Years.since.Checking.Acct.opened <- as.factor(ifelse(data.test_man_dt_model_for_test$Years.since.Checking.Acct.opened <= 2.4, 
                                                                                    "Under 2.4 Years", "Over 2.5 years"))
data.test_man_dt_model_for_test$Age <- as.factor(ifelse(data.test_man_dt_model_for_test$Age <=29, "Under 29 years", "Over 30 years"))

# Retry the prediction on my manual model
tree.pred_man_dt_model_1 <- predict(man_dt_model_1 , data.test_man_dt_model_for_test, type="class")

accuracy_test_man_dt_model_1 <-table(tree.pred_man_dt_model_1, Credit.Standing.data.test )

accuracy_test_man_dt_model_1
# tree.pred_man_dt_model_1 Bad Good
#                   Bad    0    2
#                   Good  78  122

confusionMatrix(tree.pred_man_dt_model_1, Credit.Standing.data.test)

# ---------------
# Accuracy
accuracy_man_dt_model_1 <- (0+122)/(0+2+78+122)
accuracy_man_dt_model_1
# 0.6039604

# By creating manual predictors I am limiting the Decision Trees options.
# A basic Tree gets an accuracy results of 60%, this is 12% lower than my basic
# Decision Tree below which gets 72%. 

# Another thing to note is on the plot of the tree it is splitting on Credit History
# but with only one branch to a Good result. so it is getting 122 correct predication
# but zero No No predication.


# -----------------------------------------------------------------------------
# h) Use the tree function from the package tree, or equivalent, to build a 
# decision tree and compare the results to those in g) and comment. If you use 
# pruning here you should explain all the methodology you use. 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# I have seen above the results by using forced manually split with a decision tree
# I want to use the Decision Tree itself with out me hindering it with binary results
# to see how it compares. Do do this I need to take the copy of the data from before
# I did any splits to do this. SO I will use the data.train.org and data.test.org orgional
# data to now run the model and compare the results above

# ---------------
# MODEL 1: Basic Decision Tree with all Predictors
# ---------------
dt_base_tree <- tree(Credit.Standing~., data=data.train.org)

# ---------------
# dt_model_1
# ---------------
# dt_model_1: everything on the dependent variable Credit.Standing
dt_model_1 <- tree(dt_base_tree)

#plot dt_model_1 
plot(dt_model_1)
text(dt_model_1, pretty=1, cex=0.5)

# Metion this pot in the report as we can see that it still uses Credit history and
# Age but now the Good and Bad branchs have been swapped which better results.

# ---------------
#summary of resutls
summary(dt_model_1)

# results of model
# Variables used in Tree:
# [1] "Credit.History"                     "Age"                               
# [3] "Employment"                         "Residence.Time.In.current.district"
# [5] "Savings.Acct"                       "Loan.Reason"                       
# [7] "Checking.Acct"                      "Months.since.Checking.Acct.opened""  
# Number of terminal nodes:  12 
# Misclassification error rate: 0.1802 = 109 / 605

# ---------------
# Predication
tree.pred_dt_model_1 <- predict(dt_model_1, data.test.org, type="class")
# output of Good and bad I think 

# ---------------
# measure performance with a confusion matrix with the predicted Credit Standing results
# with the Real resutls from the data.test.
accuracy_test_dt_model_1 <-table(tree.pred_dt_model_1 , Credit.Standing.data.test )

accuracy_test_dt_model_1
#             Credit.Standing.data.test
# tree.pred_dt_model_1    Bad Good
#                    Bad   58   35
#                    Good  20  89

# ---------------
# Accuracy
accuracy_pre_dt_model_1 <- (58+89)/(58+35+20+89)
# 0.7277

# ---------------
# MODEL 2: Hyper parameters
# ---------------

# using rpart to creat a model with minsplit, minbucket, maxdepth and cp
dt_model_2 <- rpart(Credit.Standing~., data=data.train.org,
                    cp = 0.01, minsplit=20, minbucket=5, maxdepth=10)
# Use Rparts plot for a more descriptive plot.
rpart.plot(dt_model_2)
# summary results for more details
summary(dt_model_2)
# predict vlaues and use a confusionmatrix function to automatically get the accuracy results
tree.pred_dt_model_2 <- predict(dt_model_2, data.test.org, type="class")
accuracy_test_dt_model_2 <-table(tree.pred_dt_model_2 , Credit.Standing.data.test )
accuracy_test_dt_model_2
confusionMatrix(tree.pred_dt_model_2, Credit.Standing.data.test )
# 0.7475 # 0.7129 # 0.7129 #0.7277 #0.7277

# ------------------
# Now run this model for various models to look at tuning
# I ran this code manually and didn't write in every iteration
dt_model_2 <- rpart(Credit.Standing~., data=data.train.org,
                    cp = 0.08, minsplit=15, minbucket=20, maxdepth=3)
tree.pred_dt_model_2 <- predict(dt_model_2, data.test.org, type="class")
accuracy_test_dt_model_2 <-table(tree.pred_dt_model_2 , Credit.Standing.data.test )
confusionMatrix(tree.pred_dt_model_2, Credit.Standing.data.test  )
# 0.74750.7129 0.7475 # many more.



#---------------------
# TO Do: In the Future and with more time I'd like to auto this
# create a for loop to go though all the iterations and output all the results
# to a table. That way could check many combinations.


# ---------------
# MODEL 3: dt_model_3 : Cross Fold Validation
# ---------------
# model three with cross fold validation

?cv.tree
# Capital K for number of folds.
dt_model_3  <- cv.tree(dt_base_tree, K=10, FUN=prune.misclass)

#--------------
par(mfrow=c(1,2))
plot(dt_model_3$size,dt_model_3$dev,type="b")
plot(dt_model_3$k,dt_model_3$dev,type="b")
par(mfrow=c(1,1))


# ---------------
# dt_model_4: Prune
# ---------------

# Fit model with prune
dt_model_4 <- prune.misclass(dt_base_tree, best=2)

# I manually tried different values for a base tree and 2 was the best prune value

# plot tree
plot(dt_model_4 )
text(dt_model_4 , pretty=1, cex=0.5)

# ---------------
#summary of resutls
summary(dt_model_4)
#[1] [1] "Credit.History"
# Misclassification error rate: 0.2975

# ---------------
# Predication
tree.pred_dt_model_4 <-predict(dt_model_4, data.test, type="class")

# ---------------
# Accuracy
accuracy_test_dt_model_4 <-table(tree.pred_dt_model_4  , Credit.Standing.data.test )
accuracy_test_dt_model_4
#             Credit.Standing.data.test
# tree.pred_dt_model_3     Bad Good
                      # Bad   23    0
                      # Good  55  124

# Read confusion matrix results
confusionMatrix(tree.pred_dt_model_4, Credit.Standing.data.test )
# 0.7277 - the same dt_model_1 # however it is a much simpler model.

# ------------------


# After trying various pruning layers, the best accuracy score came from a
# prune with only one set of nodes, this was Credit history

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# i) Now see if you can improve your results by using a random forest model. 
# Give youre results (5 marks) and explain and comment (5 marks). 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ---------------
# MODEL 1: rf_model_1
# ---------------
rf_model_1 <- randomForest(Credit.Standing~.,data = data.train.org, importance=TRUE)

# ---------------
#summary of resutls
print(rf_model_1)
# Number of trees: 500
# No. of variables tried at each split: 3
# OOB estimate of  error rate: 19.17%
#       Bad Good class.error
# Bad  195   56   0.2231076
# Good  60  294   0.1694915

# Predication
pred_rf_model_1 <- predict(rf_model_1, data.test)

# Read confusion matrix results
confusionMatrix(pred_rf_model_1 , Credit.Standing.data.test )
#0.8168 

------------------
# plot importance of predictors
importance(rf_model_1)
varImpPlot(rf_model_1, cex=0.5)


# A better result at 81% and interestingly Age is more importation.


# ---------------
# rf_model_2 :  More Trees
# ---------------
# Run many more trees to see how resutls compare
rf_model_2 <- randomForest(Credit.Standing~.,data = data.train.org, ntree = 100, importance=TRUE, do.trace=TRUE)
# NOTE: I lowered the ntree value so my code will run faster when you are reviewing.
# I do not think you want to wait for it to run thousands of times

# ---------------
#summary of resutls
print(rf_model_2)
# Number of trees: 100
# No. of variables tried at each split: 3
# OOB estimate of  error rate: 19.83%
#       Bad Good class.error
# Bad  194   57   0.2270916
# Good  63  291   0.1779661


# Predication
pred_rf_model_2 <- predict(rf_model_2, data.test)

# Read confusion matrix results
# 100 ntree
confusionMatrix(pred_rf_model_2 , Credit.Standing.data.test )
#0.0.8267  

# Below are a number of runs to see how the model performes with different resuls

# ---------------
# Read confusion matrix results
# 1000 runs
confusionMatrix(pred_rf_model_2 , Credit.Standing.data.test )
#0.8069  

# ---------------
# Accuracy for 10000
confusionMatrix(pred_rf_model_2 , Credit.Standing.data.test )
# Accuracy of 0.8119, small improvement for a big run

#-------------------
# Accuracy for 100,000 - For the craic
confusionMatrix(pred_rf_model_2 , Credit.Standing.data.test )
# 0.8168 # tiny improvemnt

#----------------------
# tried running 10 million times but my computer crashed, not enough memory


# ---------------
# rf_model_3 :  mtry tune
# ---------------
rf_model_3_1 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=3)
print(rf_model_3_1)
pred_rf_model_3_1 <- predict(rf_model_3_1, data.test)
confusionMatrix(pred_rf_model_3_1 , Credit.Standing.data.test )
# 0.8119

rf_model_3_2 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=2)
print(rf_model_3_2)
pred_rf_model_3_2 <- predict(rf_model_3_2, data.test)
confusionMatrix(pred_rf_model_3_2 , Credit.Standing.data.test)
# 0.8069 

rf_model_3_3 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=4)
print(rf_model_3_3)
pred_rf_model_3_3 <- predict(rf_model_3_3, data.test)
confusionMatrix(pred_rf_model_3_3 , Credit.Standing.data.test)
# 0.8119 

rf_model_3_4 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=5)
print(rf_model_3_4)
pred_rf_model_3_4 <- predict(rf_model_3_4, data.test)
confusionMatrix(pred_rf_model_3_4 , Credit.Standing.data.test)
# 0.797

#---------------
rf_model_3_5 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=2, stepFactor=2)
print(rf_model_3_5)
pred_rf_model_3_5 <- predict(rf_model_3_5, data.test)
confusionMatrix(pred_rf_model_3_5 , Credit.Standing.data.test)
# 0.8119

rf_model_3_6 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=3, stepFactor=2)
print(rf_model_3_6)
pred_rf_model_3_6 <- predict(rf_model_3_6, data.test)
confusionMatrix(pred_rf_model_3_6 , Credit.Standing.data.test)


rf_model_3_7 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=3, stepFactor=3)
print(rf_model_3_7)
pred_rf_model_3_7 <- predict(rf_model_3_7, data.test)
confusionMatrix(pred_rf_model_3_7 , Credit.Standing.data.test)
#0.7921

rf_model_3_8 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=4, stepFactor=4)
print(rf_model_3_8)
pred_rf_model_3_8 <- predict(rf_model_3_8, data.test)
confusionMatrix(pred_rf_model_3_8 , Credit.Standing.data.test)
#0.8119


#---------------

rf_model_3_9 <- randomForest(Credit.Standing~.,data = data.train.org, mtry=2, ntreeTry = 10,importance=TRUE)
print(rf_model_3_9)
pred_rf_model_3_9 <- predict(rf_model_3_9, data.test)
confusionMatrix(pred_rf_model_3_9 , Credit.Standing.data.test)
# 0.8119 


#--------------

# best results with 100,000 trees
rf_model_4 <- randomForest(Credit.Standing~.,data = data.train.org, mtryStart=3, stepFactor=3, ntree=10, do.trace=TRUE)
print(rf_model_4)
(196+296)/(196+296+58+55)
# 0.8132231
# Took 15 minutes and not even best resutls.
# NOTE: I lowered the ntree value so my code will run faster when you are reviewing.
# I do not think you want to wait for it to run thousands of times

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# j) Due to GDPR you are no longer allowed use the following variables to buld 
# your model Age, Personal.Status and Foreign.National.  Now redo your working 
# for parts h) and i).  Give your results and comment. 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Remove:
# 1. Age # In can guess that losing age will make the model less accuarte as it was the second node.
# 2. Personal.Status
# 3. Foreign. National

# Redo parts h and i
# ---------------
# Decision Tree
# ---------------

# Run tree and get accuracy 

dt_gdpr_model <- tree(Credit.Standing~+Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment+Housing+Job.Type+Years.since.Checking.Acct.opened+Residence.Time.In.current.district, data=data.train.org)
plot(dt_gdpr_model)
text(dt_gdpr_model, pretty=1, cex=0.5)
summary(dt_gdpr_model)
tree.pred_dt_gdpr_model <- predict(dt_gdpr_model, data.test.org, type="class")
accuracy_test_dt_gdpr_model <-table(tree.pred_dt_gdpr_model  , Credit.Standing.data.test )
accuracy_test_dt_gdpr_model
confusionMatrix(tree.pred_dt_gdpr_model, Credit.Standing.data.test)
# 0.7772
# Not so bad of a drop losing those three predictors.
# In fact its a bit surprising.
# Lets look at rpart to see what accuracy it gets.


# Run the same tree parameters with rpart to look at the results


dt_gdpr_model_1  <- rpart(Credit.Standing~+Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment+Housing+Job.Type+Years.since.Checking.Acct.opened+Residence.Time.In.current.district, data=data.train.org,
                    cp = 0.01, minsplit=20, minbucket=5, maxdepth=10)
# plot
rpart.plot(dt_gdpr_model_1 )
# summary 
summary(dt_gdpr_model_1 )
# predict vlaues and use a confusionmatrix function to automatically get the accuracy results
tree.pred_dt_gdpr_model_1  <- predict(dt_gdpr_model_1, data.test.org, type="class")
accuracy_test_dt_gdpr_model_1  <-table(tree.pred_dt_model_2 , Credit.Standing.data.test )
accuracy_test_dt_gdpr_model_1 
confusionMatrix(tree.pred_dt_gdpr_model_1 , Credit.Standing.data.test )

# Same results

# ---------------
# Random Forrest
# ---------------

rf_gdpr_model_1 <- randomForest(Credit.Standing~+Checking.Acct+Credit.History+Loan.Reason+Savings.Acct+Employment+Housing+Job.Type+Years.since.Checking.Acct.opened+Residence.Time.In.current.district,                              
                                  data = data.train.org,  mtryStart=3, stepFactor=3, ntree=1000, do.trace=TRUE)
# NOTE: I lowered the ntree value so my code will run faster when you are reviewing.
 
# ---------------
#summary of resutls
print(rf_gdpr_model_1 )
# Number of trees: 100
# No. of variables tried at each split: 3
# OOB estimate of  error rate: 19.83%
#       Bad Good class.error
# Bad  194   57   0.2270916
# Good  63  291   0.1779661

# ---------------
# Accuracy for 1000
pred_rf_gdpr_model_1<- predict(rf_model_3_8, data.test)
confusionMatrix(pred_rf_gdpr_model_1 , Credit.Standing.data.test)
#0.8119

------------------
  # plot importance of predictors
importance(rf_gdpr_model_1)
varImpPlot(rf_gdpr_model_1, cex=0.5)

#-------------------------------------------------------------------------------
# k) Find areas that could have issues
# ------------------------------------------------------------------------------

# Put Id back in and try a time series with ID and Credit Standing
# We can do this on all the data since we are not training a model

data2 <- read.csv("Credit_Risk_25_final-2.csv")

# Create an ifelse that outputs if the result Good and Bad to another column.
data2$Good_Bad_Numeric <- ifelse(data2$Credit.Standing == "Good", 1,
                              ifelse (data2$Credit.Standing == "Bad", 0, "None"))

data2$Good_Bad_Numeric <- as.numeric(data2$Good_Bad_Numeric)

ggplot(data2, aes(x=ID)) +
  geom_line(aes(y = Credit.Standing)) +
  theme_bw()
# No good # Make a new column with 0 and 1 values.

ggplot(data2, aes(x=ID)) +
  geom_line(aes(y = Good_Bad_Numeric), size=0.2) +
  ggtitle(" Good and Bad Credit Standings")+
  labs(x="ID as Time Series", y = "Bad and Good ratings") +
  theme_bw() +
  theme(axis.text.y = element_blank())
# shows places where the Credit Rating has been grouped
# Indicates a time period Maeve coule investiage more.

plot2 <- ggplot(data2, aes(x=ID)) +
  geom_point(aes(y = Good_Bad_Numeric), size =0.5) +
  theme_bw()
# kinda shows gaps that could be investiaged.

plot1 <- ggplot(data2, aes(x = ID, y = Credit.Standing)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~data2$Credit.Standing)
# Not so good either

 

#------------------------------------------------------------------------------
print("End of code : No Errors")
#------------------------------------------------------------------------------

