##Lending Club Loanbook Analysis##
##data frame 'loanbook' downloaded from 'https://www.kaggle.com/wendykan/lending-club-loan-data/kernels'

## Usually with Kaagle data sets we try and predict some obviously binary variable,
## in this case it seems like we should be trying to predict 'bad loans'. On a second look however, that is not going to be very useful
## as lending club has alraedy weeded out the predictably bad loans, and a only a decision tree
## with lots of complexity that overfits would bother classifying any loan as bad.
## There is still a lot of useful, practical information here, especially if we take the
## perspective of a potential lender. Then we flip the question on its head and must ask the
## question, does Lending Tree constitute a good investment, and if so which loans are the
## 'best loans' to finance.

#  import packages, may have to use install.packages() for packages you dont have.
library(DescTools) 
library(dplyr) 
library(magrittr) 
library(randomForest)
library(rpart) 
library(rattle)
library(ggplot2)
library(readxl)
library(party)


#upload loanbook csv file and comlumn name dictionary
    ##Make sure to change the path names on the following lines
loanbook <- read.csv("/Users/christopherhamblin/Desktop/R Projects/lending-club-loan-data/loan.csv") #!change path name
loanbookdictionary<-read_excel('/Users/christopherhamblin/Desktop/R Projects/lending-club-loan-data/LCDataDictionary.xlsx') #change pathname

##Preliminary plotting and Summary Statistics
str(loanbook)
summary(loanbook)
Desc(loanbook$loan_amnt, main = "Loan Amount Distribution", plotit = TRUE)
Desc(loanbook$grade, main= "Grade Distribution", plotit=TRUE)
Desc(loanbook$loan_status, main='Loan Status Distribution', plotit=TRUE)
Desc(loanbook$purpose, main = "Loan Purpose Distribution", plotit = TRUE)
Desc(loanbook$term, main='Loan Term Distribution', plotit=TRUE)

#Clean/Prep Data for Analysis and Prediction

      ## label bad loans with 1 good loans with 0
loanbook<-mutate(loanbook, bad=ifelse(loan_status %in%
     c("Charged Off",
       "Default",
       "Does not meet the credit policy. Status:Charged Off",
       "Default Receiver", 
       "Late (16-30 days)",
       "Late (31-120 days)"), 1, 0))

loanbook$loan_status2<-'Current'
loanbook$loan_status2[loanbook$bad==1]<-'Bad'
loanbook$loan_status2[loanbook$loan_status=='Fully Paid']<-'Fully Paid'
loanbook$loan_status2<-factor(loanbook$loan_status2)

#Loans that do not meet Credit Policy
loanbook$not_cred_pol<-'0'
loanbook$not_cred_pol[loanbook$loan_status=='Does not meet the credit policy. Status:Charged Off']<-'Charged Off'
loanbook$not_cred_pol[loanbook$loan_status=='Does not meet the credit policy. Status:Fully Paid']<-'Fully Paid'
loanbook$not_cred_pol<-factor(loanbook$not_cred_pol)


#Plot loan Status by Loan Grade
ggplot(loanbook, aes(grade,fill = loan_status2)) +
  geom_bar(position = "fill") +
  labs(title = "Status by Loan Grade",x = "Grade", y = "Rate", fill="Loan Status")
                 
      ##Change dates from factor to date type 
loanbook$issue_d <- as.Date(gsub("^", "01-", loanbook$issue_d), format="%d-%b-%Y")
loanbook$earliest_cr_line <- as.Date(gsub("^", "01-", loanbook$earliest_cr_line), format="%d-%b-%Y")
loanbook$term<-as.character(loanbook$term)
loanbook$term<-as.integer(substr(loanbook$term,1,3))


##Analysis

#Bad Loan Rate Over Time

loanbydate<-loanbook %>% group_by(issue_d) %>% 
  summarize(percent_bad=sum(bad)/length(bad),
  not_cred_pol_paid=sum(not_cred_pol=='Fully Paid'),not_cred_pol_bad=sum(not_cred_pol=='Charged Off'), total_issued=length(bad))

     ## Plot
badloan_date_plot=ggplot(loanbydate, aes(x=issue_d, y=percent_bad, colour=total_issued))
 badloan_date_plot + geom_point() +
 scale_colour_gradient(low="blue", high="red", trans='log',breaks=c(100,1000,10000),labels=c(100,1000,10000)) +
 labs(title='Bad Loans Over Time',x='Date Issued (by month)',y='Bad Loan Rate', colour='Loans Issued (log scale)')
 
 # This plot gives some very interesting information. We see a dramatic decrease in the
 # bad loan rate over the first couple of years. 3 years ago the badloan rate then starts
 # to decrease linearly, given that the loan term is 3 years this is what we would expect,
 # the fact that the decrease is linear is interesting because it tells us maintaining
 # a current loan status does not indicate a good debtor, debtors are equally likely to fail 
 # on their loan payments at any point in there payment term.
 
 #Lets filter out current loans that have been issued recently, since we know the badloan rate
 #is artificially low
 
loanbook_recentfilter<-filter(loanbook,!(loan_status=='Current' & issue_d>'2014-01-01'))

  #Plot
ggplot(loanbook_recentfilter, aes(grade,fill = loan_status2)) +
  geom_bar(position = "fill") +
  labs(title = "Status by Loan Grade (Recent loans filtered out)",x = "Grade", y = "Rate", fill="Loan Status")



#New Credit Policy
cred_pol_plot=ggplot(loanbydate, aes(x=issue_d, y=(not_cred_pol_paid+not_cred_pol_bad)/total_issued))
cred_pol_plot +  geom_area() +
labs(title='Non-Policy Loans Over Time',x='Date Issued (by month)',y='Non-Policy Loan Rate')

# We see clearly that Lending Tree instituted their new credit policy in 2011, but judging
# by the previous graph this had no effect on the bad loan rate.


#Filter risky loans with decision tree. Since we are really interested in finding
#'Good Loans' we want a tree that airs on the side of caution, so over fitting is not
# as much of an issue as long as our errors are false positives in marking 'bad loans'

  #Fill in missing values
loanbook_recentfilter$annual_inc[is.na(loanbook_recentfilter$annual_inc)]<-0
loanbook_recentfilter$delinq_2yrs[is.na(loanbook_recentfilter$delinq_2yrs)]<-0


set.seed(400)
fit <- randomForest(as.factor(bad) ~ grade+emp_length+home_ownership+annual_inc+verification_status+purpose+dti+delinq_2yrs,
                      data=loanbook_recentfilter, 
                      importance=TRUE,
                      ntree=500)


##Determining good loans 
  #Expected returns by loan grade
  #We will filter out all current loans because they are unpredictable and very old loans
  # because there bad loan rate is not representative.

loanbook_fullfilter<-loanbook %>% filter(issue_d>'2008-12-01' & loan_status!='Current')

loanbygrade<-loanbook_fullfilter %>% group_by(grade) %>%
  summarize(tot_loaned=sum(funded_amnt), tot_rec=sum(total_pymnt), avg_term=mean(term), ROI=tot_rec/tot_loaned-1,annual_ROI=ROI/(avg_term/12))

ggplot(data=loanbygrade, aes(x=grade, y=annual_ROI)) +
  geom_bar(stat='identity') +
  labs(title = "Returns by Loan Grade",x = "Grade", y = "Annual ROI")

#This is very unsettling, Lending Tree is losing money on every one of its loan grade groups
#perhaps its best if we don't invest afterall

      


