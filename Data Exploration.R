library(readr)
churn = read.csv("C:\\Users\\user\\Documents\\Working Directory\\Working Directory_Intellipaat\\R Language\\Practice Session\\1st  Stimulus\\customer_churn.csv")

############ Questions on Data Extraction ############ 


# 1.a.Extract 'InternetService' column and store it in 'customer_internet_service'
customer_internet_service = churn$InternetService
str(customer_internet_service)
table(customer_internet_service)

# b.Extract 'PaperlessBilling' column and store it in 'customer_Paperless_Billing'
customer_Paperless_Billing = churn$PaperlessBilling
str(customer_Paperless_Billing)
table(customer_Paperless_Billing)

# c.Extract 'StreamingTV' column and store it in 'customer_Streaming_TV'
customer_Streaming_TV = churn$StreamingTV
str(customer_Streaming_TV)
table(customer_Streaming_TV)


# 2.Extract the 3rd, 6th and 9th columns from the 'customer_churn' data.frame & store it in 'customer_random_columns'
customer_random_columns = churn[c(3,6,9)]
str(customer_random_columns)


# 3.Extract all the columns from column number-10 to column-number 20 and store the result in 'customer_10_20'
customer_10_20 = churn[,10:20]
str(customer_10_20)
ncol(customer_10_20)


# 4.	Extract only these row numbers: 65, 765, 3726 & 7000 and store the result in 'customer_random_rows'
customer_random_rows = churn[c(65, 765, 3726, 7000),]
str(customer_random_rows)
nrow(customer_random_rows)


# 5.Extract all the rows starting from row number-655 to row number-6550 & store the result in 'customer_continuous_rows'
customer_continuous_rows = churn[655:6550,]
str(customer_continuous_rows)
dim(customer_continuous_rows)

x = 7043-654 ; y = 7043-6550
x-y


# 6.Extract row numbers- 10, 100 & 1000 & column numbers- 5, 10, 15 & store the result in 'customer_random_data'
customer_random_data = churn[c(10,100,1000),c(5,10,15)]
str(customer_random_data)
dim(customer_random_data)


############  Questions on Flow Control Statements  ############ 


# 1.Check if the value in the 6th cell of 'PaymentMethod' column is 'Electronic check'. If yes, print "Yes, the payment method is Electronic check"
data.frame(colnames(churn)) # Returns column index numbers in table format,df=DataFrame name.

match("PaymentMethod",names(churn))

require(fastmatch)
fmatch("PaymentMethod",names(churn))

grep("^PaymentMethod$", colnames(churn))

# If one wanted the number of all the column names that begin with "PaymentMethod" to be removed, one would write:
churn2 = churn[ , - grep("^PaymentMethod", colnames(churn))] 

if(churn[6,18] == "Electronic check"){
  print("Yes, the payment method is Electronic check")
}else{
  print("No, the payment method is not Electronic check")
}

                    # OR #

if(churn[6,18] != "Electronic check"){
  print("No, the payment method is not Electronic check ")
}else{
  print("Yes, the payment method is Electronic check")
}
   

#rownames(churn) # Rownames will return rownumbers present in Dataset,df=DataFrame name.
#data.frame(as.integer(rownames(churn))) # Returns Row index numbers in table format ,df=DataFrame name.
 

# 2.a.Check the value present in 12th cell of 'Contract' column. If it's 'month-to-month', print 'The contract is on a month to month basis'.
match("Contract",names(churn))
a= NULL
a = as.character(churn[12,16])
a = toString(a)
a
if(churn[12,16] == "month-to-month"){
  print("The contract is on a month to month basis")
}else{
  print(paste("The contract is not on a month to month basis, instead it is on a", a, "basis",sep=" "))
}
                  
                   # OR #

if(churn[12,16] == "month-to-month"){
  print("The contract is on a month to month basis")
}else{
  if(churn[12,16] == "One year"){
    print("The contract is on a yearly basis")
  }else{
    print("The contract is on a two-year basis")
  }
}

                   # OR #

if (churn[12,16] == "month-to-month") {
  print("The contract is on a month to month basis")
} else if (churn[12,16] == "One year") {
  print("The contract is on a yearly basis")
} else {
  print("The contract is on a two-year basis")
}


# 3.Use switch to check the gender in 6th cell of 'gender' column.
match("gender",names(churn))
churn[6,2]
b = switch(
  as.character(churn[6,2]),
  "Female" = "Give a discount of 50% in 'MonthlyCharges",
  "Male" = "Give a discount of 20% in 'MonthlyCharges",
  "Dog" = "NO discount"
)
print(b)


# 4.Use for loop to get the count of customers whose 'InternetService' is 'DSL'
counter = 0
for (a in churn$InternetService){
  if(a == "DSL") {
    counter = counter + 1
  }
}
print(counter)
table(churn$InternetService) # DSL = 2421


# 5.Use while to find the number of customers whose tenure is exactly '2' months
##t = ifelse(churn$tenure==2,2,0)
##table(t)
s = churn$tenure
class(s)
i = 1
l = 0
while (i <= length(s)) {
  
  # body
  if(s[i] == 2) {
    l = l+1
  }
  i = i + 1 
}
print(l)


k = ifelse(churn$tenure==2,"2s","not 2s")
table(k)


############ Questions on Inbuilt Functions ############ 


# 1.Do these operations with the head() function: a.Get the last record of 'TotalCharges' column
head(churn$PhoneService,4)


# b.Get the first 8 records from 'Contract' column
head(churn$Contract, 8)


# 2.Do these operations with the tail() function: a.Get the last record of 'TotalCharges' column
tail(churn$TotalCharges,1)


# b.Get the last 5 records of 'tenure' column
tail(churn$tenure, 5)


# 3.Find the average, minimum, maximum & range from the 'tenure' column
avg = mean(churn$tenure)
avg

mini = min(churn$tenure)
mini

maxi = max(churn$tenure)
maxi

ran = range(churn$tenure)
ran


# 4.Get 10 random values from the 'TotalCharges' column using sample()
?sample
sample(churn$TotalCharges,10)


# 5.Find the count of different levels in 'PaymentMethod' & 'Contract' columns using table()
count = table(churn$PaymentMethod,churn$Contract)
count
class(count)



