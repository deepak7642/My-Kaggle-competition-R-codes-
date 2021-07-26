# Inclass Prediction Competition - IPBA Cars Price Prediction
## Predict prices for used cars for a Startup

# Objective - Develop a pricing model for third-party companies that will make an estimate of the Price of the customer's car directly from their online portal. This will save customers time and help the company to reduce its cost and also streamline the process of selling used cars.


# Loading Libraries 
library(tidyr)
library(ggplot2)
library(dplyr)


## Importing the train, test and sample files 
tr <- read.csv('C:\\Users\\DEEPAK KAURA\\Desktop\\IPBA B-8 (Kaggle)\\train2.csv')
ts <- read.csv('C:\\Users\\DEEPAK KAURA\\Desktop\\IPBA B-8 (Kaggle)\\test2.csv')
cs <- read.csv('C:\\Users\\DEEPAK KAURA\\Desktop\\IPBA B-8 (Kaggle)\\sample2.csv')


# Taking a copy of the train and test datasets
trT <- tr
tsT <- ts

head(trT)

head(tsT)


# Preliminary Thoughts About The Data
## Just by looking at the first few rows of both the train and test datasets, these were my preliminary thoughts -
  
### Variables Engine, Power and Mileage could be converted to numerical values and separating and removing the text after the numbers.

### Name column could be split into brand, and model that could be used as independent variables in the model


# Data Cleaning


#### Before we do any exploratory analysis we will first clean up the variables Engine, Power and Mileage on both the test and train datasets.

# Here we drop Price column for temporary bases 
trT <- subset(trT, select = -c(Price))


# Here we combined train and test files 
flldt<-rbind(trT, tsT) 


# Checking total rows in train , test and combined file of train and test
nrow(trT)
nrow(tsT)
nrow(flldt)

# let's see after combining of train and test files
head(flldt)


# let's convert variables who are in character form into factor
flldt <- flldt %>%
  mutate_if(is.character, as.factor)


# let's convert engine, power and mileage variables into numeric variables
flldt$Engine <- as.numeric(flldt$Engine)
flldt$Power <- as.numeric(flldt$Power)
flldt$Mileage <- as.numeric(flldt$Mileage)


head(flldt)


# Summary of Seat column to check NA's and 0's
summary(flldt$Seats)

colSums(is.na(flldt))

#By looking at the above summary and NA count we can see that Seat column has NA's . Before we deal with the missing values lets transform the Name column and seperate out Brand and Model names in the dataset.


library(purrr)
library(stringr)

# Spliting the Name column by space and taking the Model name and Brand Name, this value is then stored in respective columns
flldt$Brand <-map(str_split(flldt$Name,  " "),1)
flldt$Model <-map(str_split(flldt$Name,  " "),2)

flldt$Brand <-as.factor(unlist(flldt$Brand))
flldt$Model <-as.factor(unlist(flldt$Model))

sapply(flldt,class)


# let's check how Brand and Model columns looks
View(flldt)

# Now we have got the brand and model seperated out

# Dealing with missing values

#### To deal with all missing values across variables, I think the simplest and best way do it could be to take a median based on Name (new variable created). Generally based on my understanding of the automobile market in India, most of the cars are sold with 1 single engine and seats option which is likely to have same power, mileage and seats. So our missing values will be treated based on this assumption. If we still have missing values after this treatment we will use Model (new variable created).

#### Because we are going to use is.na command to deal with missing values we will replace all 0's with NA


flldt$Seats[flldt$Seats == 0] <- NA


# Replacing based on Car Name

new_flldt <- flldt %>%  group_by(Name) %>%
  mutate_at(c("Seats"), funs(ifelse(is.na(.), median(., na.rm = TRUE),.)))


colSums(is.na(new_flldt))
colSums(is.na(flldt))


### After replacing with Name we still have missing values, so next we replace with model

## Replacing based on Model
new_flldt <- new_flldt %>%  group_by(Model) %>%
  mutate_at(c("Seats"), funs(ifelse(is.na(.), median(., na.rm = TRUE),.)))

colSums(is.na(new_flldt))
colSums(is.na(flldt))

### We have reduced the missing values, but there are still a few, this time we replace with Brand

## Replacing based on Brand

new_flldt <- new_flldt %>%  group_by(Brand) %>%
  mutate_at(c("Seats"), funs(ifelse(is.na(.), median(., na.rm = TRUE),.)))

colSums(is.na(new_flldt))
colSums(is.na(flldt))


# Now we have got the full data set without any missing values we will seperate it in to train and test datasets respectively based on the number of rows.

new_tr<- new_flldt[1:4809,]
new_ts<- new_flldt[4810:6019,]


# Adding the Price Column back to the train dataset
new_tr$Price<-tr$Price
head(new_tr)

View(new_tr)


# changing four columns in test file
colnames(new_ts)[4] <- "KMS_Driven"
colnames(new_ts)[6] <- "Fuel"
colnames(new_ts)[12] <- "Owner"
colnames(new_ts)[13] <- "New Price" 



# let's check for test file
head(new_ts)

View(new_ts)


# Here we change the four columns name in train file

colnames(new_tr)[4] <- "KMS_Driven"
colnames(new_tr)[6] <- "Fuel"
colnames(new_tr)[12] <- "Owner"
colnames(new_tr)[13] <- "New Price" 



# Exploratory Data Analysis


# Looking at the price variable first
summary(new_tr$Price)
hist(new_tr$Price)
boxplot(new_tr$Price)


#### There seem to be an outlier with the highest value at 334.56 which is very high compared to the rest of the dataset. For now we will leave it as it is.
#### The price column looks right skewed with a long tail on the right side. Generally regression models work best if the dependent variable has normal distribution. Let us check if taking log tranformation of the price column changes the distribution.

hist(log(new_tr$Price))

#### Doing log transformation has improved the distribution drastically and made it look like normal distribution, when we build a model we can compare with for both the log transformed price and the actual price and compare results.


summary(new_tr$KMS_Driven)
hist(new_tr$KMS_Driven)
boxplot(new_tr$KMS_Driven)

#### Kilometers_Driven clearly seem to have an outlier with 65 Lakh kilometers, I think each vehicle can cover about a 1-2 lakh kilometers if mantained very well in its life time, so this is a clear outlier, we remove this row.


new_tr<-new_tr[!(new_tr$KMS_Driven) ==6565068 ,]

# Lets look at the summary, histogram and boxplot after removing the outlier

summary(new_tr$KMS_Driven)
hist(new_tr$KMS_Driven)
boxplot(new_tr$KMS_Driven)


# Lets now look at summary, histogram and boxplot for the remaining variables - Engine, Power and Mileage

summary(new_tr$Engine)
hist(new_tr$Engine)
boxplot(new_tr$Engine)




summary(new_tr$Power)
hist(new_tr$Power)
boxplot(new_tr$Power)




summary(new_tr$Mileage)
hist(new_tr$Mileage)
boxplot(new_tr$Mileage)


#### There is nothing particular that stands out in these columns, we will now move to multivariate analysis.


# MultiVariate Analysis

plot(new_tr$Year,new_tr$Price)

#### We look at Price and Year scatterplot, as the vehicle gets older the price of it reduces, our data seems to be following that pattern.



plot(new_tr$Price,new_tr$KMS_Driven)

#### We look at Price and Kilometers Driven scatterplot, as the vehicle is driven more the price of it reduces, our data seems to be following that pattern.




plot(new_tr$Location,new_tr$Price)



ggplot(new_tr, aes(x=reorder(Location, -Price), y=Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

#### We can see that the average vehicle price is higher in Bangalore and Coimbatore, this aligns with the fact that road tax on vehicles in India is highest in the state of Tamil Nadu and Karnataka.



ggplot(new_tr, aes(x=reorder(Fuel, -Price), y=Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

#### We can see that the average vehicle price is highest for electric vehicles, as India doesnt have many mass market electric vehicles available. Also Diesel vehicles are priced higher than petrol. Fuel Type vs Price analysis also aligns with our general understanding of the autombile market.



ggplot(new_tr, aes(Transmission, Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

#### Automatic vehilces are priced higher than manual, because of the convenience they offer and also because it has a more complicated technology involved.



ggplot(new_tr, aes(x=reorder(Seats, -Price), y=Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

#### 2 seater cars are most expensive, this is because generally sports cars with powerful engines have only 2 seats are very expensive.



ggplot(new_tr, aes(x=reorder(Owner, -Price), y=Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")


#### Cars with just one owner are generally expensive than second, third and fourth and above. As the car is sold multiple times its prices reduces.



ggplot(new_tr, aes(x=reorder(Brand, -Price), y=Price)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")



# Business Insights And Validations From EDA

### As the vehicle gets older the price of it reduces, our data seems to be following that pattern.

### As the vehicle is driven more the price of it reduces, our data seems to be following that pattern.

### Average vehicle price is higher in Bangalore and Coimbatore, this aligns with the fact that road tax on vehicles in India is highest in the state of Tamil Nadu and Karnataka.

### We can see that the average vehicle price is highest for electric vehicles, as India doesnt have many mass market electric vehicles available. Also Diesel vehicles are priced higher than petrol. Fuel Type vs Price analysis also aligns with our general understanding of the autombile market.

### Automatic vehilces are priced higher than manual, because of the convenience they offer and also because it has a more complicated technology involved.

### 2 seater cars are most expensive, this is because generally sports cars with powerful engines have only 2 seats are very expensive.

### Cars with just one owner are generally expensive than second, third and fourth and above. As the car is sold multiple times its prices reduces.


# Creating a Linear Regression Model

mlr_price = lm(log(Price) ~ Year + KMS_Driven + factor(Location) + factor(Fuel) + factor(Transmission) + Engine + Power + Mileage + factor(Seats) +  factor(Owner) + factor(Brand) + factor(Model), data = new_tr)
summary(mlr_price)


new_ts$Brand[new_ts$Brand == "Ambassador"] <- "Toyota"
new_ts$Model[new_ts$Model == "Classic"] <- "Qualis"
new_ts$Model[new_ts$Model == "Beetle"] <- "Polo"
new_ts$Model[new_ts$Model == "Boxster"] <- "Cayenne"
new_ts$Model[new_ts$Model == "Countryman"] <- "Cooper"
new_ts$Model[new_ts$Model == "Prius"] <- "Camry"
new_ts$Model[new_ts$Model == "Tiguan"] <- "Jetta"

# Making the Prediction

new_ts$predicted_price = exp(predict(mlr_price, new_ts))



cs$id = new_ts$id
cs$Price = new_ts$predicted_price
 

write.csv(cs, "submission_new.csv", row.names = FALSE)

