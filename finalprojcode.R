# Load the data
fulldata <- read.csv(file.choose())

# Check for any observations missing a response value
fulldata[!complete.cases(fulldata$loan_status),]

# Select variables.
vars <- c("id", "member_id", "loan_amnt", "grade","home_ownership", "annual_inc", "loan_status", "dti", "fico_range_low", "inq_last_6mths", "mths_since_last_delinq", "open_acc", "is_inc_v")
projdata <- fulldata[vars]

# Change the Loan Status variable into a binary variable.
Cu <- grepl(pattern="Current", x = projdata$loan_status)
Fp <- grepl(pattern="Fully Paid", x = projdata$loan_status)
projdata$Response <- ifelse(Cu | Fp,0,1)

# Seperate the data into the 1's and 0's.
ones <- projdata[which(projdata$Response==1),]
zeros <- projdata[which(projdata$Response==0),]

#########################
# Variable: Loan Amount #
#########################

# Summary of the variable
summary(projdata$loan_amnt)

# View histogram and then Transform the variable (square root makes the data look more normal)
par(mfrow=c(1,2))
hist(projdata$loan_amnt,xlab="Loan Amount", ylab="Frequency", main="Histogram of Loan Amounts")
hist(sqrt(projdata$loan_amnt),xlab="Loan Amount", ylab="Frequency", main="Histogram of Loan Amounts")


###################
# Variable: Grade #
###################

# Summary of the variable
summary(projdata$grade)
par(mfrow=c(1,1))
plot(projdata$grade,xlab="Grade", ylab="Frequency", main="Histogram of Grades")

# Plot the good/bad loans side by side.
par(mfrow=c(1,2))
plot(ones$grade, xlab="Grade", ylab="Frequency", main="Distribution of (bad) Grades")
plot(zeros$grade, xlab="Grade", ylab="Frequency", main="Distribution of (good) Grades")

############################
# Variable: Home Ownership #
############################

# Summary of the variable
summary(projdata$home_ownership)

# Place the 8 NONE observations into the OTHER group.
projdata$home_ownership[projdata$home_ownership=="NONE"] <- "OTHER"
ones$home_ownership[ones$home_ownership=="NONE"] <- "OTHER"
zeros$home_ownership[zeros$home_ownership=="NONE"] <- "OTHER"

# Drop the now empty NONE factor level.
projdata$home_ownership <- droplevels(projdata$home_ownership)
ones$home_ownership <- droplevels(ones$home_ownership)
zeros$home_ownership <- droplevels(zeros$home_ownership)

# Plot the good/bad Home Ownership levels side by side.
par(mfrow=c(1,2))
plot(ones$home_ownership, xlab="Home Ownership", ylab="Frequency", main="Distribution of (bad) Ownership Status")
plot(zeros$home_ownership, xlab="Home Ownership", ylab="Frequency", main="Distribution of (good) Ownership Status")

###########################
# Variable: Annual Income #
###########################

# Summary of the variable
summary(projdata$annual_inc)

# Drop the missing observations
projdata <- projdata[!is.na(projdata$annual_inc),]
ones <- ones[!is.na(ones$annual_inc),]
zeros <- zeros[!is.na(zeros$annual_inc),]

# View histogram and then Transform the variable (log makes the data look more normal)
hist(projdata$annual_inc,xlab="Annual Income", ylab="Frequency", main="Histogram of Annual Incomes")
hist(log(projdata$annual_inc),xlab="log(Annual Income)", ylab="Frequency", main="Histogram of log(Annual Incomes)")

#################
# Variable: DTI #
#################

# Summary of the variable
summary(projdata$dti)

# Histogram of DTI Ratios
par(mfrow=c(1,1))
hist(projdata$dti, xlab="DTI Ratio", ylab="Frequency", main="Histogram of DTI Ratios")

############################
# Variable: FICO Range Low #
############################

# Summary of the variable
summary(projdata$fico_range_low)

# Histogram of FICO score lower bounds
hist(projdata$fico_range_low, xlab="FICO Score (Low", ylab="Frequency", main= "Histogram of FICO Score Lower Bounds")

##########################################
# Variable: Inquiries in last six months #
##########################################

# Summary of the variable
summary(projdata$inq_last_6mths)

# Drop the 25 missing observations
projdata <- projdata[!is.na(projdata$inq_last_6mths),]
ones <- ones[!is.na(ones$inq_last_6mths),]
zeros <- zeros[!is.na(zeros$inq_last_6mths),]

# Histogram of Inquiries
hist(projdata$inq_last_6mths, xlab="Inquiries", ylab="Frequency", main= "Histogram of Inquiries (6 Months)")

# Create binary variable from inquiries
projdata$InqFact[projdata$inq_last_6mths > 0] <- "Yes"
projdata$InqFact[projdata$inq_last_6mths == 0] <- "No"

ones$InqFact[ones$inq_last_6mths > 0] <- "Yes"
ones$InqFact[ones$inq_last_6mths == 0] <- "No"

zeros$InqFact[zeros$inq_last_6mths > 0] <- "Yes"
zeros$InqFact[zeros$inq_last_6mths == 0] <- "No"

# Make the variable from a string variable to a factor variable
projdata$InqFact <- factor(projdata$InqFact, labels=c("No", "Yes"))

ones$InqFact <- factor(ones$InqFact, labels=c("No", "Yes"))
zeros$InqFact <- factor(zeros$InqFact, labels=c("No", "Yes"))

# Summary of the new binary variable
summary(projdata$InqFact)

# Plot the good/bad distributions side by side.
par(mfrow=c(1,2))
plot(ones$InqFact, xlab="Inquiries (6 months)?", ylab="Frequency", main="Distribution of (bad) Inquiries")
plot(zeros$InqFact, xlab="Inquiries (6 months)?", ylab="Frequency", main="Distribution of (good) Inquiries")

###########################################
# Variable: Months Since Last Delinquency #
###########################################

# Summary of the variable
summary(projdata$mths_since_last_delinq)

# Histogram of Inquiries
hist(projdata$mths_since_last_delinq, xlab="Months Since Last Delinquency", ylab="Frequency", main= "Histogram of Months Since Last Delinquency")

# Due to large amount of NA's, make into a binary variable.
projdata$DelinqFact[projdata$mths_since_last_delinq >= 0] <- "Yes"
projdata$DelinqFact[is.na(projdata$mths_since_last_delinq)] <- "No"

ones$DelinqFact[ones$mths_since_last_delinq >= 0] <- "Yes"
ones$DelinqFact[is.na(ones$mths_since_last_delinq)] <- "No"

zeros$DelinqFact[zeros$mths_since_last_delinq >= 0] <- "Yes"
zeros$DelinqFact[is.na(zeros$mths_since_last_delinq)] <- "No"

# Make the variable from a string variable to a factor variable
projdata$DelinqFact <- factor(projdata$DelinqFact, labels=c("No", "Yes"))

ones$DelinqFact <- factor(ones$DelinqFact, labels=c("No", "Yes"))
zeros$DelinqFact <- factor(zeros$DelinqFact, labels=c("No", "Yes"))

# Summary of the new binary variable
summary(projdata$DelinqFact)

# Plot the good/bad distributions side by side.
par(mfrow=c(1,2))
plot(ones$InqFact, xlab="Delinquency?", ylab="Frequency", main="Distribution of (bad) Delinquencies")
plot(zeros$InqFact, xlab="Delinquency?", ylab="Frequency", main="Distribution of (good) Delinquencies")

###########################
# Variable: Open Accounts #
###########################

# Summary of the variable
summary(projdata$open_acc)

# Histogram of the variable
par(mfrow=c(1,2))
hist(projdata$open_acc, xlab="Open Accounts", ylab="Frequency", main="Histogram of Open Accounts")

# Log Transform the variable
hist(log(projdata$open_acc), xlab="log(Open Accounts)", ylab="Frequency", main="Histogram of log(Open Accounts)")

#############################
# Variable: Verified Income #
#############################

# Summary of the variable
summary(projdata$is_inc_v)

# Relabel the factor levels
levels(projdata$is_inc_v) <- c("no", "source", "yes")
levels(ones$is_inc_v) <- c("no", "source", "yes")
levels(zeros$is_inc_v) <- c("no", "source", "yes")

# Plot the good/bad distributions side by side.
plot(ones$is_inc_v, xlab="Verified?", ylab="Frequency", main="Distribution of (bad) verification")
plot(zeros$is_inc_v, xlab="Verified?", ylab="Frequency", main="Distribution of (good) verification")


