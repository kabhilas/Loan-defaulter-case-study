# Load required libraries.
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

#*********************************** Data Sourcing ***************************************************
# Read the dataset using read.csv function
loan <- loan.dataset <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)
str(loan)
head(loan)
# There are 39717 obs. of  111 variables



#********************************** Data Cleaning ****************************************************
#Remove all unwanted columns without much variance and useful information inorder to save memory
# and help in ffaster processing.

# a) After analysing the data it can be seen that there are several columns with single value.
# These columns will not be helpful in any calulation and visualization because of no variancce and thus can be removed from dataset
columns_with_single_value <- as.vector(which(sapply(loan, function(x) length(unique(x))==1)))
colnames(loan[, columns_with_single_value])
length(columns_with_single_value)  # There are 60 variables with single value in them
loan <- loan[, -columns_with_single_value]
str(loan)



# b) Remove columns with values 0 and NA as there is not much variance
which(sapply(loan, function(x) length(unique(x))==2))
  #term collections_12_mths_ex_med   chargeoff_within_12_mths                  tax_liens 
  #6                         48                         49                         51
unique(loan$term)                        # 36 months and 60 months 
unique(loan$collections_12_mths_ex_med)  # 0 and NA
unique(loan$chargeoff_within_12_mths)    # 0 and NA
unique(loan$tax_liens)                   # 0 and NA
loan <- loan[, -c(48,49,51)]   # 48 columns/variables in the dataset

# c) Removing columns having more than 60 % values as NA or blanks
round(sum(is.na(loan$mths_since_last_record))/length(loan$mths_since_last_record) * 100, 2) # 92.9 %
round(sum(is.na(loan$mths_since_last_delinq))/length(loan$mths_since_last_delinq) * 100, 2) # 64.56 %
round(length(which(loan$next_pymnt_d == ""))/length(loan$next_pymnt_d) * 100, 2) #97.13
loan <- subset(loan, select = -c(mths_since_last_record, mths_since_last_delinq,next_pymnt_d))

#d) url seems to be useless , so get rid of url column
loan$url <- NULL


#e)column 'desc' seems to be useless as we are not going to do text analysis.So get rid of it. 
loan$desc <- NULL


#e)columns 'purpose'and 'title' captures same information .Purpose is more generalised.So get rid of 'title'
loan$title <- NULL

#f) Let's check if Ids and member Ids are unique or not
sum(duplicated(loan$id)) # 0 Ids are unique
sum(duplicated(loan$member_id)) # 0 Member Ids are also unique

#g) Since 1 to 1 mapping is between id and member id , we can drop one column
# drop member-id column
loan$member_id <- NULL

#h) check the count unique vallues in 'purpose' column
# 14 categories of loan purpose
length(unique(loan$purpose)) 

#Check the structure of loan after removing all unwanted columns
#45 variables left for analysis
str(loan)
head(loan, 20) 


#i)Let's convert and generalise the required columns
# 1>term and interest rates are character datatype, need to be converted to numeric
loan$int_rate = as.numeric(gsub("%", "", loan$int_rate))
loan$revol_util = as.numeric(gsub("%", "", loan$revol_util))
loan$term = as.numeric(gsub(" months| ", "", loan$term))

# 2> Let's convert the date fields in POSIXct format
loan$issue_d = paste("01",loan$issue_d,sep="-")
loan$issue_d = parse_date_time(loan$issue_d, orders = c("dmy"), locale = "eng")
loan$earliest_cr_line = paste("01",loan$earliest_cr_line,sep="-")
loan$earliest_cr_line = parse_date_time(loan$earliest_cr_line, orders = c("dmy"), locale = "eng")
loan$last_pymnt_d = paste("01",loan$last_pymnt_d,sep="-")
loan$last_pymnt_d = parse_date_time(loan$last_pymnt_d, orders = c("dmy"), locale = "eng")
loan$last_credit_pull_d = paste("01",loan$last_credit_pull_d,sep="-")
loan$last_credit_pull_d = parse_date_time(loan$last_credit_pull_d, orders = c("dmy"), locale = "eng")
# Some of the records have failed to parse because of being blank or NA

# Let's sort the column names
loan <- loan[c(sort(colnames(loan)))]

#Write cleaned file
write.csv(loan,"loan_cleanedsubset.csv",row.names = FALSE)

#********************************** Data Cleaning Ends ****************************************************




# All the 39717 records are "Loan Accepted". Let's see the grouping based on status 
summarise(group_by(loan, loan_status), count = length(id)) 
#   loan_status count
#   <chr>       <int>
# 1 Charged Off  5627
# 2 Current      1140
# 3 Fully Paid  32950
# considering only the Charged off and Fully paid records
loan <- subset(loan, loan$loan_status != "Current")
length(loan$id) # 38577 records
loan_bkp <-loan
#loan <-loan_bkp
######################################################

loan_Charged_off <-subset(loan, loan$loan_status == "Charged Off")
loan_FP <-subset(loan, loan$loan_status == "Fully Paid")

################ Setting theme for plots ##############################
p_theme <-theme(axis.title = element_text(colour="grey20",size=10,face="bold")
      ,axis.text.x = element_text(face="bold", color="#993333",size=8)
      ,axis.text.y = element_text(face="bold", color="#993333",size=8)
      ,title= element_text(colour="Orange",size=15,face="bold")) 


#********************************** Univariate analysis ****************************************************
#let's analyse one by one all variables for different loan status excluding current


################# Plot-1:Log2 plot of charged off cases across state#########################
ggplot(loan_Charged_off, aes(reorder(addr_state, -table(addr_state)[addr_state]))) + geom_bar(stat="count" ,fill="blue",col="Black") + 
  scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5) + 
  scale_x_discrete(labels = abbreviate)+ labs(title = "Charged off cases across state",
                                             x = "State",
                                             y = "Count off charged of cases")+ p_theme

################# Plot-2:Log2 plot of charged off cases across zipcode#########################

ggplot(loan_Charged_off, aes(reorder(zip_code ,zip_code))) + geom_point( stat="count",col="Blue" ) + labs(title = "Charged off cases per zip-code",
                                              x = "Zipcode",
                                              y = " Count off charged off cases")+ p_theme

################# Plot-3:Annual income distribution for charged off cases#########################
################ Checking and removing outliers of Annual income column #################
boxplot(loan$annual_inc)
quantile(loan$annual_inc)
summary(loan$annual_inc)
#Taking values below 75th percentile or 3rd Qu.
loan<-subset(loan,loan$annual_inc<=82000)
boxplot(loan$annual_inc)
summary(loan$annual_inc)

#derrive annual income into bins of 10000's
loan$income_bin<-as.factor(round(loan$annual_inc/10000,digits = 0)*10000)# 90 levels created for income_bin
ggplot(subset(loan, loan$loan_status=="Charged Off") ,aes(income_bin))+ geom_bar(stat="count", col= "black") + geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("Anual Income bins") + ylab("Count") + ggtitle("Anual income distribution for charged off Cases") +p_theme


                                                                                                     
################# Plot-4 delinq_2yrs analysis for charged of cases ##############################

boxplot(loan$delinq_2yrs)
summary(loan$delinq_2yrs)
boxplot(loan$delinq_2yrs~loan$loan_status)

nrow(loan_Charged_off[loan_Charged_off$delinq_2yrs==0,])
nrow(loan_FP[loan_Charged_off$delinq_2yrs==0,])
nrow(loan_Charged_off)
nrow(loan_FP)

################# Plot-5 DTI analysis for charged of cases ##############################


boxplot(loan_Charged_off$dti, axes = FALSE, staplewex = 1 ,col="Blue")
text(y = boxplot.stats(loan_Charged_off$dti)$stats, labels = boxplot.stats(loan_Charged_off$dti)$stats, x = 1.3)
summary(loan_Charged_off$dti)

################# Plot-6 Employment duration Analysis for charged off Cases #########################
unique(loan$emp_length)
tmp <- subset(loan, loan$loan_status=="Charged Off")
tmp <- tmp [-which(tmp$emp_length =="n/a" ),]
ggplot(tmp ,aes(emp_length ,fill=emp_length))+ geom_bar(stat="count", col= "black") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("Employment Tenure") + ylab("Count") + ggtitle("Employement duration Analysis for charged off Cases") +p_theme


################# Plot-7 Funded amount and Fund amount invested################
nrow(loan_Charged_off[loan_Charged_off$funded_amnt == loan_Charged_off$funded_amnt_inv, ])
boxplot(loan_Charged_off$funded_amnt)
text(y = boxplot.stats(loan_Charged_off$funded_amnt_inv)$stats, labels = boxplot.stats(loan_Charged_off$funded_amnt_inv)$stats, x = 1.35)
summary(loan_Charged_off$funded_amnt)

boxplot(loan_Charged_off$funded_amnt_inv , axes = FALSE, staplewex = 1 )
text(y = boxplot.stats(loan_Charged_off$funded_amnt_inv)$stats, labels = boxplot.stats(loan_Charged_off$funded_amnt_inv)$stats, x = 1.35)
summary(loan_Charged_off$funded_amnt_inv)
################# Plot-8 Home Ownership ###################################
ggplot(loan_Charged_off,aes(reorder(home_ownership , -table(home_ownership)[home_ownership])))+geom_bar(fill='#FFB90F')+geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ylim(0,4000)+ ggtitle('UA of Home Ownership for charged off cases') + p_theme

################# Plot-9 inq_last_6mths ###################################
ggplot(loan_Charged_off,aes(inq_last_6mths ))+geom_bar(fill='#006400')+geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ylim(0,4000)+ ggtitle('UA of inq_last_6mths charged off cases') + p_theme


################# Plot-10 Installments analysis for charged off cases ###################################
boxplot(loan_Charged_off$installment , axes = FALSE, staplewex = 1,col="darkgoldenrod" )
text(y = boxplot.stats(loan_Charged_off$installment)$stats, labels = boxplot.stats(loan_Charged_off$installment)$stats, x = 1.32)
summary(loan_Charged_off$installment)
# Create installment bins
str(loan_Charged_off)
df<-loan_Charged_off
df$installment_bin[df$installment >= 0 & df$installment < 200] <- c("0-200")
df$installment_bin[df$installment >= 200 & df$installment < 400] <- c("200-400")
df$installment_bin[df$installment >= 400 & df$installment < 600] <- c("400-600")
df$installment_bin[df$installment >= 600 & df$installment < 800] <- c("600-800")
df$installment_bin[df$installment >= 800 & df$installment < 1000] <- c("800-1000")
df$installment_bin[df$installment >= 1000 & df$installment < 1200] <- c("1000-1200")
df$installment_bin[df$installment >= 1200 & df$installment < 1400] <- c("1200-1400")

ggplot(df, aes(df$installment_bin)) +geom_bar(fill='#006400')+geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ylim(0,2000)+ xlab("Bins of size $200") + ylab("Frequency") + ggtitle("Binned Instalment Freq.") + p_theme



################# Plot-11 Interest Rate Analysis####################################
ggplot(loan_Charged_off,aes(int_rate))+ylim(0,4000)+ geom_histogram( fill='blue',position="dodge",binwidth=1,colour="black") +labs(title = "UA of Interest Rate -charged off cases" ,x = "Interest rate",y = "Frequency")+ p_theme

################# Plot-12 Open account analysis ####################################
boxplot(loan_Charged_off$open_acc,axes = FALSE, staplewex = 1)
text(y = boxplot.stats(loan_Charged_off$open_acc)$stats, labels = boxplot.stats(loan_Charged_off$open_acc)$stats, x = 1.25)
summary(loan_Charged_off$open_acc)
ggplot(loan_Charged_off, aes(x=loan_Charged_off$open_acc)) + geom_bar(stat = "count" ,fill='brown1') + xlab("Open Accounts") + ylab("Count") + ggtitle("Histogram of Open Accounts") + p_theme

################# Plot-13 Public Record  and public record Bankruptices Analysis ########################################
ggplot(loan_Charged_off,aes(pub_rec))+geom_bar(stat = "count" ,fill='darkgoldenrod') + xlab("derogatory public records") + ylab("Count") + ggtitle("Derogatory public records cases for Charged off ") + p_theme

#removing NA values from pub_rec_bankruptcies
length(which(is.na(loan$pub_rec_bankruptcies))) 
tmp <- loan_Charged_off [-which(is.na(loan_Charged_off$pub_rec_bankruptcies)),]
ggplot(tmp,aes(pub_rec_bankruptcies))+geom_bar(stat = "count" ,fill='darkgoldenrod') + xlab("Public record bankruptcies") + ylab("Count") + ggtitle("Public record bankruptcies for Charged off ") + p_theme


################# Plot-14 last_pymnt_amnt distribution for charged off cases ####
boxplot(loan_Charged_off$last_pymnt_amnt)
summary(loan_Charged_off$last_pymnt_amnt)

################# Plot-15 Total Payment distribution for charged off cases ####
 boxplot(loan_Charged_off$total_pymnt, axes = FALSE, staplewex = 1)
text(y = boxplot.stats(loan_Charged_off$total_pymnt)$stats, labels = boxplot.stats(round(loan_Charged_off$total_pymnt),2)$stats, x = 1.35) 
summary(loan_Charged_off$total_pymnt)
################# Plot-16 Total Payment inverse for charged off cases ####
boxplot(loan_Charged_off$total_pymnt_inv, axes = FALSE, staplewex = 1 )
text(y = boxplot.stats(loan_Charged_off$total_pymnt_inv)$stats, labels = boxplot.stats( round(loan_Charged_off$total_pymnt_inv),2)$stats, x = 1.3)
summary(loan$total_pymnt_inv)

################# Plot-17 grade and subgradedistribution for charged off cases ####
ggplot(loan_Charged_off, aes(grade )) + geom_bar(stat="count" ,fill='aquamarine')+ 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-3) + 
  xlab("Grade") + ylab("Total Count") + ggtitle("Grade Vs Charged_off") +p_theme

#Sub Grade Vs Charged_off
ggplot(loan_Charged_off, aes(sub_grade, fill = grade)) + geom_bar(stat="count") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) + 
  xlab("Sub Grade") + ylab("Total Count") + ggtitle("Sub Grade Vs Charged_off")


################# Plot-18 loan purpose analysis for charged off cases ###################
p_theme_1 <-theme(axis.title = element_text(colour="grey20",size=10,face="bold")
                ,axis.text.x = element_text(face="bold", color="#993333",size=8 ,angle=45,vjust=0.9 )
                ,axis.text.y = element_text(face="bold", color="#993333",size=8)
                ,title= element_text(colour="Orange",size=15,face="bold"))
ggplot(data = loan_Charged_off, aes(reorder(purpose, -table(purpose)[purpose]))) + geom_bar(stat="count" , fill='darkolivegreen2', col='Black') +scale_y_continuous(trans='log2') +geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("Purpose given by applicant") + ylab("Count of Applicants") +ggtitle("Reasons for which the Loan was taken") +p_theme_1


################# Plot-19 Term Vs Charged_off ###################

ggplot(loan_Charged_off, aes(term)) + geom_bar(stat="count" , fill="chocolate", col="Black") + 
  geom_text(stat='count',aes(label=..count..), vjust=-0.25) + 
  xlab("Term") + ylab("Total Count") +  
  ggtitle("Term Vs Charged_off") +p_theme
 

################## Plot-20 Total open CLs for charged off cases ###################
boxplot(loan_Charged_off$total_acc)
summary(loan$total_acc)

################### Plot-21 Total payement and total payment invested for charged off cases ###################
boxplot(loan_Charged_off$total_pymnt)
summary(loan_Charged_off$total_pymnt)

boxplot(loan_Charged_off$total_pymnt_inv)
summary(loan_Charged_off$total_pymnt_inv)

#################### Plot-22 Total interest, late fee and principal recd ###################
boxplot(loan_Charged_off$total_rec_int)
summary(loan_Charged_off$total_rec_int)

boxplot(loan_Charged_off$total_rec_late_fee)
summary(loan_Charged_off$total_rec_late_fee)


boxplot(loan_Charged_off$total_rec_late_fee)
summary(loan_Charged_off$total_rec_late_fee)

#################### Plot-23 Verification Status Vs Charged_off ###################
ggplot(loan_Charged_off, aes(reorder(verification_status, -table(verification_status)[verification_status]))) + 
  geom_bar(stat="count" ,fill='aquamarine4', col='black') +geom_text(stat='count',aes(label=..count..),vjust=-0.5) +
  xlab("Verification Status")+ ylab("Total Count") + 
  ggtitle("Verification Status Vs Charged_off") +p_theme




#********************************** Bivariate Analysis ****************************************************
# Grade
ggplot(loan[loan$emp_length != "N/A",], aes(x= loan_status,  group=grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~grade) + scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels=abbreviate) + p_theme

# Scatter plot for multivariate 
ggplot(loan, aes(x = loan_amnt, y = purpose, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter()

# Verification Status
ggplot(loan, aes(x= loan_status,  group=verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="Status") +
  facet_grid(~verification_status) + scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels=abbreviate)

# Home Status
ggplot(loan, aes(x= loan_status,  group=home_ownership)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="Loan Status") +
  facet_grid(~home_ownership) + scale_y_continuous(labels = scales::percent)

# Purpose
ggplot(loan, aes(x= loan_status,  group=purpose)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") + facet_grid(~purpose) + scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels=abbreviate)

# Employee Experience
tmp <- loan [-which(loan$emp_length =="n/a" ),]
ggplot(tmp, aes(x= loan_status,  group=emp_length)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) + labs(y = "Percent", fill="Status") +
  facet_grid(~emp_length) + scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels=abbreviate)

# Employee
ggplot(tmp, aes(x=tmp$emp_length, fill=factor(home_ownership))) + geom_bar(position="dodge",stat="count") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Employment tenure") + ylab("Applicants Count") + 
  ggtitle("charged of cases on  Basis of the type of Home ad emp_tenure") 

################################################## THE-END ############################################################################################################




