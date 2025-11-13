rm(list = ls()) # Clear variables in the right panel
cat("\014")                                                                                                                                                                                                                          # Clear console window content


# In R, any package must go through two steps before use
# Installation and loading
# install.packages("package_name")
# library(package_name)


#install.packages("pacman")
library(pacman)

pacman::p_load(magrittr, readr, caTools, ggplot2, dplyr, vcd, ROCR, plotly, chorddiag)

# Introduction to using the pacman package
#https://psmethods.postach.io/link/ryu-yan-zhong-li-yong-pacmanan-zhuang-he-jia-zai-bao-tai-bu-luo


#******************
#1. Data Preparation
#******************
#
#1.1 Transaction Data (X)
X = read_csv("C:/data/2022-Jan.csv") %>% 
  data.frame %>% setNames(c(
    "date","type","prod","cat","code","brand","price","cid","session"))
X <- X %>% filter(type == "purchase")
X$date = as.Date(X$date)
summary(X)                  # Number of transactions: 263797


par(cex=0.8)
windows();hist(X$date, "days", col=rainbow(4), las=2, freq=T, xlab="", main="Daily No. Transaction")
# "years" can be changed to "quarters", "months", "weeks", "days"


#n_distinct() is a function in the dplyr package
n_distinct(X$cid)           # Number of customers: 28220

# Example
# mydata = sample(1:10, 50, rep = TRUE)
# length(unique(mydata))
# n_distinct(mydata)


# %>% is called the pipe operator
# It's an operator in the magrittr package that makes code easier to write and read
# Reference URL
#https://bookdown.org/tonykuoyj/eloquentr/data-workflow.html
# Reference URL
#https://blog.gtwang.org/r/r-pipes-magrittr-package/

#1.2 Customer Data (A)
A = X %>% 
  mutate(days = as.integer(as.Date("2022-02-01") - date)) %>% 
  group_by(cid) %>% summarise(
    recent = min(days),     # Days since last purchase 
    # Select the minimum value in days
    freq = n(),             # Purchase frequency
    money = mean(price),   # Average purchase amount
    senior = max(days),     # Days since first purchase
    # Select the maximum value in "days"
    since = min(date)       # First purchase date # Look at its "date""
  ) %>% data.frame

# Useful functions in the dplyr package include
#mutate() adds a new column, group_by() is similar to subtotals, summarise()... etc.
#filter()	    Filter observations that meet conditions
#select()	    Select variables
#mutate()	    Add derived variables
#arrange()	  Sort observations by variables
#summarise()	Aggregate variables
#group_by()	  Group by categorical variables, often used with summarise() function

# Reference URL
#http://rstudio-pubs-static.s3.amazonaws.com/565321_6df775af00ea42f290af7677a044cfd7.html
# Reference URL
#https://rpubs.com/RitaTang/dplyr_intro





#1.4 Customer Data Summary
summary(A)


#1.5 Variable Distribution
windows();p0 = par(cex=0.8, mfrow=c(2,2), mar=c(3,3,4,2))
hist(A$recent,20,main="recency(R Activity)",ylab="",xlab="")
hist(pmin(A$freq, 10),0:10,main="frequency(F Loyalty)",ylab="",xlab="")
hist(A$senior,20,main="seniority(S Tenure)",ylab="",xlab="")
hist(log(A$money,10),main="log(money)(M Contribution)",ylab="",xlab="")


# If you want to open a new empty canvas, use the windows() function
# pmin() means parallel minima, similarly pmax() means parallel maxima
# On Mac machines, the windows() function name is not recognized,
# so please use the quartz() function to create a new empty canvas instead


A0 = A; X0 = X
save(X0, A0, file="c:/data/tf0_W.rdata")


rm(list = ls()) # Clear variables in the right panel
cat("\014") 
load("c:/data/tf0_W.rdata")

A = A0; X = X0

#******************
#2. Hierarchical Cluster Analysis
#******************

#2.1 RFM Customer Segmentation

set.seed(111)
A$grp = kmeans(scale(A[,2:4]),10)$cluster
table(A$grp)  # Group size



#2.2 Customer Group Attributes
windows();
group_by(A, grp) %>% summarise(
  recent = mean(recent), 
  freq = mean(freq), 
  money = mean(money), 
  size = n() ) %>% 
  mutate( revenue = size*money/1000 )  %>% 
  filter(size > 1) %>% 
  ggplot(aes(x=freq, y=money)) +
  geom_point(aes(size=revenue, col=recent),alpha=0.5) +
  scale_size(range=c(4,30)) +
  scale_color_gradient(low="green",high="red") +
  scale_x_log10() + scale_y_log10() + 
  geom_text(aes(label = size ),size=3) +
  theme_bw() + guides(size="none") +
  labs(title="Customer Segements",
       subtitle="(bubble_size:revenue_contribution; text:group_size)",
       color="Recency") +
  xlab("Frequency (log)") + ylab("Average Transaction Amount (log)")

# The above is a five-dimensional space chart (can you count them?)
# What are the five dimensions? ANS: (can you tell?)
# First dimension -->  X-axis: Purchase frequency (Frequency).
# Second dimension -->  Y-axis: Average transaction amount (customer unit price) (Money).
# Third dimension -->  Bubble size: Reflects this group's revenue contribution (Revenue).
# Fourth dimension -->  Bubble color: The redder it is, the longer since last purchase, possibly about to churn (Recency).
# Fifth dimension -->  Number in bubble: Represents the number of member customers in this group (Size).
# Can target customers who come often (high frequency) but buy little (low unit price) for marketing strategies,
# Develop methods to increase unit price for this group of customers.
# 
# Explanation 1:
# Comparing the top-left 127 (people) group with the bottom 3692 (people) group,
# Both are large revenue contributors, but the 127-person group's revenue contribution
# seems even higher than the 3692 group, meaning although the group only has 127 people, their
# unit price per person is very high, they are our most important customers! If we
# accidentally lose these member customers, it will have a huge impact on company revenue,
# so we must do everything to retain them!
# 
# Explanation 2:
# We can also look at the 3692 (people) group, from the X-axis we know they don't purchase
# very often, from the y-axis their purchase amounts are not high either, but this
# group has very many people, we can use marketing methods to increase their purchase
# frequency, or raise unit price, to improve this group's revenue situation. In contrast,
# the 2266 group on the right has similar purchase frequency to 3692, but their unit price
# is much higher.
# 
# Therefore, from these bubble charts, we can know where the main revenue sources come from.
# This helps us see which customer groups our marketing focus should be on.


# When we segment customers, if we only use statistical clustering methods
# we can definitely achieve "minimum within-group distance, maximum between-group distance",
# such as k-means or hierarchical clustering, but we can't maintain the continuity of segmentation rules
# , meaning each time after segmentation there will be a new set of segmentation rules!?
#
# Explanation: Because segmentation results need expert interpretation,
# different group contents will have different interpretations, even members in the same group
# may be different, then the group concept and characteristics will change
# , groups are different, new rule-based groups.
#
# If each segmentation produces a new set of segmentation rules, it means the segmentation
# rules are uncontrollable, this year's first group and next year's first group may not be the same,
# customer segmentation continuity is very difficult to maintain, therefore... this is very
# troublesome (headache) for Marketing people, so Marketing people may
# change to adopt company policies in advance, segment based on some "predetermined rules", called
# rule-based segmentation.

AN = scale(A[, c(2,3,4,5)]) %>% data.frame
# sapply(AN, mean)
# sapply(AN, sd)
kg = A$grp
#names(AN) =c(
#  "Days since last purchase","Average purchase frequency (times/days)","Average transaction amount")
# names(AN) =c(
#   "Days since last purchase","Member tenure","Average purchase frequency (times/days)","Average transaction amount","Total gross profit contribution")
# sapply(split(A,kg), colMeans) %>% round(2)  # Original scale 
# sapply(split(AN,kg), colMeans) %>% round(2)  # Standardized scale 


# Bar chart
windows();par(cex=0.8)
split(AN,kg) %>% sapply(colMeans) %>% barplot(beside=T,col=rainbow(4))
legend('topleft',legend=colnames(AN),fill=rainbow(4))





#******************
#3. Rule-Based Segmentation
#******************

#3.1 Customer Segmentation Rules

# What are "predetermined rules"?
# For example, we segment customers by "days since first purchase",
# if "days since purchase is less than a certain number of days" they are classified as New Customers,
#
# Conversely, if "days since purchase is greater than a certain number of days" they are classified as Regular
# Customers. If regular customers "have purchased within a certain period", we further
# classify them as Core Customers and Key Customers.
#
# If regular customers "have not purchased in that period", we classify them as Sleeping
# Customers, for sleeping customers we can also see how long they've been sleeping, we can
# further subdivide into different levels of sleep (hahaha, this is interesting!), as for why we need to divide sleep
# levels this way?
#
# ANS: Because the longer they sleep, if the company wants to bring them back, it will need to spend more
#     resources to bring them back, otherwise it will be harder to bring them back.
#
# For regular customers who are not sleeping, we can also use "revenue contribution value" to divide them into
# Core Customers and Key Customers.

# Recommendation: Usually first draw out your desired customer segmentation rules, after multiple discussions,
#       then finally write it into Code


# New Customers are further divided into two categories N1 and N2
# N1: New Customers, N2: New Potential Customers

# Regular Customers are also divided into two categories R1 and R2
# R1: Key Customers, R2: Core Customers

# Sleeping Customers are divided into three categories S1, S2, S3
# S1: Drowsy Customers, S2: Half-Asleep Customers, S3: Deep-Sleep Customers

# K: Customer's average purchase cycle
# rx: recent
# fx: freq
# mx: money
# sx: senior

STS = c("N1","N2","R1","R2","S1","S2","S3")
Status = function(rx,fx,mx,sx,K) {factor(
  ifelse(sx < 2*K,
         ifelse(fx*mx > 50, "N2", "N1"),
         ifelse(rx < 2*K,
                ifelse(sx/fx < 0.75*K,"R2","R1"),
                ifelse(rx < 3*K,"S1",
                       ifelse(rx < 4*K,"S2","S3")))), STS)}



#3.2 Average Purchase Cycle for Each Member Customer
K = as.integer(sum(A$senior[A$freq>1]) / sum(A$freq[A$freq>1])); K

# Need at least two visits to calculate their purchase cycle time
# So more than once is coded as A$freq > 1

#=============================
# Explanation of Purchase Cycle Time K Calculation Details
#=============================
# Calculate number of people who visited more than twice
length(A$freq[A$freq>1])
# [1] 25600

# Similarly, calculate number of people who visited once or less
length(A$freq[A$freq<2])
# [1] 2620

# Use table() function to combine and see number of people who came once vs twice or more
table(A$freq>1)
# FALSE  TRUE 
# 2620 25600  

# Sum verification
sum(table(A$freq>1))
#[1] 28220

# Can also see the proportion of people
# Look at proportions
table(A$freq>1) %>% prop.table()
# FALSE       TRUE 
# 0.09284196 0.90715804 
table(A$freq>1) %>% prop.table() %>% sum()  # Proportion sum verification
# [1] 1

# Number of times customers visited the store
A$freq

# Condition where customers visited more than once
A$freq>1

# Days since customer became a member
A$senior

# Days since becoming a member for those who visited more than once
A$senior[A$freq>1]

# Number of records (i.e., people) who visited more than once
length(A$senior[A$freq>1])
#[1] 25600

# Number of records (i.e., people) who visited less than twice
length(A$senior[A$freq<2])
#[1] 2620


# Total days, sum all
# Sum of days for those who visited more than once
sum(A$senior[A$freq>1])
#[1] 391046

# Total frequency, sum all
sum(A$freq[A$freq > 1])
#[1] 261177

as.integer(391046/261177)
#[1] 1


#3.3 Sliding Data Window
Y = list()              # Create an empty LIST
for(y in 2022) {   # At the end of each year, consolidate customer data into a data frame
  D = as.Date(paste0(c(y),"-01-31")) # Current period, previous period end date 
  Y[[paste0("Y",y)]] = X %>%        # Start from transaction data
    filter(date <= D[1]) %>%        # Cut data to end date
    mutate(days = 1 + as.integer(D[1] - date)) %>%   # Days from transaction to end date
    group_by(cid) %>% summarise(    # Summarize by customer ...
      recent = min(days),           #   Days from last purchase to end date   
      freq = n(),                   #   Purchase frequency (up to end date)   
      money = mean(price),         #   Average purchase amount (up to end date)
      senior = max(days),           #   Days from first purchase to end date
      status = Status(recent,freq,money,senior,K),  # Status at end date
      since = min(date),                      # First purchase date
      y_freq = sum(date > D[2]),              # Current period purchase frequency
      y_revenue = sum(price[date > D[2]])    # Current period purchase amount
    ) %>% data.frame }

# Y's meaning is to calculate each customer's segmentation status at the end of that year (2010 to 2015),
# implying that each member customer at the end of next year,
# this year's segmentation status is N1, next year might become N2, the year after might
# become R, then might become S. Like the customer lifecycle of birth, aging, sickness, and death.
# Therefore, the hardest part of customer value management is needing to do segmentation
# every period, when doing segmentation each period you'll discover customers' status is changing.

head(Y$Y2022) # head can view the first six records


#3.4 Cumulative Customer Count at End of Each Year
sapply(Y, nrow)  # Through sapply you can apply your specified function to each column of the list, and output results in vector, matrix, or list form.
# Can understand the benefits of using list now~ right?
# products each year. Each customer's R, F, M, S, and current segmentation label (which tank)
# and whether the customer came in the last year, how many times, how much money, etc.
# With this information, the company can better observe changes in each customer group.


# From the above, we can also do table() operations on the status column data in each year's data frame,
# then calculate the total number of each status type each year.
# Also verify! Does each year's status details match what the nrow() function calculates?
sapply(Y, function(df) table(df$status))
# Please think about how to verify that the overall and group numbers are correct?

# Y2022
# N1   639
# N2   163
# R1    92
# R2    73
# S1   958
# S2   962
# S3 25333



#3.5 Group Size Change Trends
windows();par(cex=0.8, mfrow=c(1,1))
cols = c("gold","orange","blue","green","pink","magenta","darkred") # Specify color for each group
sapply(Y, function(df) table(df$status)) %>% barplot(col=cols) 
legend("topleft",rev(STS),fill=rev(cols)) # Used to label the chart legend, specified in top-left corner.



#3.6 Dynamic Analysis of Group Attributes
CustSegments = do.call(rbind, lapply(Y, function(d) {
  group_by(d, status) %>% summarise(
    average_frequency = mean(freq),
    average_amount = mean(money),
    total_revenue = sum(y_revenue),
    total_no_orders = sum(y_freq),
    average_recency = mean(recent),
    average_seniority = mean(senior),
    group_size = n()
  )})) %>% ungroup %>% 
  mutate(year=rep(2022)) %>% data.frame
head(CustSegments)


df = CustSegments %>% transmute(
  `Group` = as.character(status), year = year, 
  `Average Purchase Frequency` = average_frequency, 
  `Average Unit Price` = average_amount,
  `Total Revenue Contribution` = total_revenue
)

# install.packages("plotly")
# library(plotly)
ggplot(df, aes(
  x=`Average Purchase Frequency`,y=`Average Unit Price`,color=`Group`,group=`Group`,ids=year)) +
  geom_point(aes(size=`Total Revenue Contribution`,frame=year),alpha=0.8) +
  scale_size(range=c(2,12)) -> g
ggplotly(g)


filter(df,`Group`%in%c('N1','N2','R1','R2')) %>% 
  ggplot(aes(
    x=`Average Purchase Frequency`,y=`Average Unit Price`,color=`Group`,group=`Group`,ids=year)) +
  geom_path(alpha=0.5,size=2) +
  geom_point(aes(size=`Total Revenue Contribution`),alpha=0.8) +
  scale_size(range=c(2,12)) -> g
ggplotly(g)


############# Stuck Stuck #####################

#3.7 Dynamic Analysis of Group Attributes
df = Y$Y2022[,c(1,6)]
tx = table(df$status.x, df$status.y) %>% 
  as.data.frame.matrix() %>% as.matrix()
# Update date: 2022.6.11
# Compatible with new R-4.0.0, can use the following command, reinstall package
# devtools::install_github("mattflor/chorddiag", force = TRUE)
# Method as follows:
# install.packages("devtools")
# library(devtools)
# devtools::install_github("mattflor/chorddiag", force = TRUE)
tx %>% prop.table(1) %>% round(3)   # Flow matrix (%)


#3.8 Interactive Flow Analysis


#library(chorddiag)
chorddiag(tx, groupColors=cols)


# Reference website for chord diagrams
#http://rwepa.blogspot.com/search?q=chorddiag


#4. Build Models


# 4.1 Prepare Data
CX = left_join(Y$Y2022)
head(CX)


names(CX)[8:11] = c("freq0","revenue0","Retain", "Revenue") 
CX$Retain = CX$Retain > 0
head(CX)


table(CX$Retain) %>% prop.table()  # Average retention probability = 22.54%



#4.2 Build Classification Model
mRet = glm(Retain ~ ., CX[,c(2:3,6,8:10)], family=binomial()) # Use logistic regression to predict whether customer will purchase
summary(mRet)


#4.3 Estimate Classification Model Accuracy
## Confusion Matrix  
pred = predict(mRet,type="response")
table(pred>0.5,CX$Retain) 

table(pred>0.5,CX$Retain) %>%
  {sum(diag(.))/sum(.)}            # Accuracy (ACC) at threshold probability = 0.5: 85.19%


colAUC(pred,CX$Retain)             # Discrimination rate (AUC): 87.92%


#install.packages("analogue")
#library(analogue)


windows();
prediction(pred, CX$Retain) %>% 
  performance("tpr", "fpr") %>% 
  plot(print.cutoffs.at=seq(0,1,0.1))


# 4.4 Build Quantity Model
# Next we predict how much those who will purchase will spend. 
# We must use regression to predict quantity.

dx = subset(CX, Revenue > 0)  # Only model those who purchased
mRev = lm(log(Revenue) ~ recent + freq + log(1+money) + senior +
            status + freq0 + log(1+revenue0), dx)  
summary(mRev)                 # Coefficient of determination: R2 = 0.713
# From above we can see certain variables have significant explanatory power for the model,
# achieving a coefficient of determination of 0.713.

windows();plot(log(dx$Revenue), predict(mRev), col='pink', cex=0.65)
abline(0,1,col='red') 




# 5. Estimate Customer Lifetime Value

CX = Y$Y2022
names(CX)[8:9] = c("freq0","revenue0")

# Predict Y2022 retention rate
CX$ProbRetain = predict(mRet,CX,type='response')

# Predict Y2022 purchase amount
CX$PredRevenue = exp(predict(mRev,CX))

# Change data frame CX's data to Y2022 data, and change variable columns 8, 9 to names the model can recognize: freq0 and revenue0.
# Store classification prediction model (whether will purchase in Y2022) prediction results as ProbRetain in CX.
# Store quantity prediction model (how much will spend in Y2022) prediction results as PredRevenue in CX.


#5.2 Estimate Customer Lifetime Value (CLV)
# Next we calculate customer lifetime value to understand each customer's potential value.
# 
# Customer i's lifetime value
# Vi=∑t=0Ng×mirti(1+d)t=g×mi∑t=0N(ri1+d)t
# 
# mi, ri: Customer i's expected (per period) revenue contribution, retention probability
# g, d: Company's (pre-tax) operating profit margin, cost of capital


g = 0.5   # (Pre-tax) profit margin
N = 5     # Number of periods = 5
d = 0.1   # Interest rate = 10%
CX$CLV = g * CX$PredRevenue * rowSums(sapply(
  0:N, function(i) (CX$ProbRetain/(1+d))^i ) )

summary(CX$CLV)


par(mar=c(2,2,3,1), cex=0.8)
windows();hist(log(CX$CLV,10), xlab="", ylab="")
# Calculate customer lifetime value: multiply profit margin by expected profit,
# then multiply by sum of present value of expected retention rates for next five years.


# 5.3 Compare Value Across Groups
# We consolidate results into a table, can directly see each group's average retention probability
# , expected revenue contribution, and lifetime value, this helps us understand each consumer status's
# characteristics and can formulate strategies for them.


# Average revenue contribution, retention probability, lifetime value by group
CX %>% group_by(status) %>% summarise_at(vars(ProbRetain:CLV), mean)

# Plot boxplot of customer lifetime value by customer status segmentation.
windows();par(mar=c(3,3,4,2), cex=0.8)
boxplot(log(CLV,10)~status, CX, main="CLV by Groups")


#6. Set Marketing Strategy, Plan Marketing Tools

#7. Select Marketing Targets
# 
# 7.1 Retention for R2 Group
# R2 group's predicted retention rate and purchase amount

windows();par(mfrow=c(1,2), mar=c(4,3,3,2), cex=0.8)
hist(CX$ProbRetain[CX$status=="R2"],main="ProbRetain",xlab="")
hist(log(CX$PredRevenue[CX$status=="R2"],10),main="PredRevenue",xlab="")


# 7.2 Estimate Expected Return
# First assume marketing tool cost and expected benefit, assume there's a tool costing 10 that can raise next period's purchase probability to 0.75
cost = 10        # Cost
effect = 0.75    # Benefit: next period's purchase probability

# Then estimate this marketing tool's expected return for each R2 customer

Target = subset(CX, status=="S3")
Target$ExpReturn = (effect - Target$ProbRetain) * Target$PredRevenue - cost
summary(Target$ExpReturn)
# Results show! This tool's average return for R2 customers is negative, WHY?


# 7.3 Select Marketing Targets
# However, we can still pick many marketing targets with large expected returns from R2

Target %>% arrange(desc(ExpReturn)) %>% select(cid, ExpReturn) %>% head(258)

sum(Target$ExpReturn > 0)                 # Feasible targets: 258

# Among R2, 258 people have expected return greater than zero, if we use this tool on these 258 people, our expected return is:

sum(Target$ExpReturn[Target$ExpReturn > 0])   # Expected return: 6464



####QUIZ:
# We can calculate expected return for implementing this tool on all groups...

Target = CX
Target$ExpReturn = (effect - Target$ProbRetain) * Target$PredRevenue - cost
filter(Target, Target$ExpReturn > 0) %>%
  group_by(status) %>% summarise(
    No.Target = n(),
    AvgROI = mean(ExpReturn),
    TotalROI = sum(ExpReturn)) %>% data.frame

# Do you feel anything unreasonable (weird)?
# If this tool's effect doesn't change, i.e., assume this tool has the same effect on each group,
# then S3 actually has the highest expected return


# 8. Conclusion

# If you only have customer ID, transaction date, transaction amount three columns, you can do analyses including:
# 
# For all customers and each customer segment:
# Group size and growth trends
# Group attributes: such as average CLV, average revenue contribution, growth rate, gross margin (need cost data) etc.
# Group characteristic comparison and trend analysis
# Inter-group flow and average flow probability
# For each customer:
# Retention rate, expected purchase amount, lifetime value
# Current group, and probability of transitioning to each group next period
# If there are marketing tool usage records, we can also estimate each marketing tool's success probability for each customer
# Generally speaking, these analysis results are sufficient for us to formulate customer development and customer retention strategies; as for customer acquisition strategies, we usually need to pull customer personal attribute data from CRM to do that.

# Check & Save
is.na(Z) %>% colSums
##  date  cust   age  area   cat  prod   qty  cost price   tid 
##     0     0     0     0     0     0     0     0     0     0
is.na(X) %>% colSums
##    tid   date   cust    age   area  items pieces  total  gross 
##      0      0      0      0      0      0      0      0      0
is.na(A) %>% colSums
## cust    r    s    f    m  rev  raw  age area 
##    0    0    0    0    0    0    0    0    0
A0 = A; X0 = X; Z0 = Z
save(Z0, X0, A0, file="c:/data/tf0.rdata")




