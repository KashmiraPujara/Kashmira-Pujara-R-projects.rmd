#Assignmnet 2A

install.packages('readr')
install.packages ('dplyr')
```{r}
library(dplyr) 
library(lubridate)
library(readr) 
library(stringr) 
library(forcats)
library(tidyr)
```


```{r}
setwd('/users/kashmirapujara/downloads/')
birthdays = read.csv('president_birthdays.csv')
heights = read.csv('president_heights.csv')
states = read.csv('president_states.csv')
```

```{r}
head(birthdays)
```

```{r}
#For Question 1 - The birthdays of US Presidents need to be parsed into the ISO8601 standard for dates. Which of the following will accomplish this? Select all that apply.
#Answer Code Below
library(lubridate); ymd(birthdays$birthday) #not working  
as.Date(birthdays$birthday,format='%b / %d / %Y') #not wokring
library(lubridate); mdy(birthdays$birthday) #wokring
as.Date(birthdays$birthday,format='%m / %d / %Y')#wokring
```


```{r}
#For question 2 - The names of Presidents contain a leading space. This can pose a problem if it is used for joining tables. Which of the following functions will remove that leading space? Select all that apply.
#Answer Code Below
birthdays$Name

library(stringr); str_trim(birthdays$Name)#wokring
library(readr) ; parse_string(birthdays$Name)#not wokring
as.character(birthdays$Name)#not wokring
library(readr) ; parse_character(birthdays$Name)#working
```
```{r}
#For Question 3 - Now that the date is in a standard format, it should easy to extract the month. Next, summarize the number of presidents by month of birth. Which month were the fewest Presidents born in?
#Answer Code Below
a=birthdays %>%
  mutate(birthday = mdy(birthdays$birthday))%>%
  select(-Name)

birthdays = data.frame(birthdays,a) 

birthdays
table(month(birthdays$birthday.1))
```


```{r}
#For Question 4 - In the dataset, heights, the height of Presidents contain the unit in which height is expressed. As a result of this, it is not possible to use the data in the heights column for quantitative analysis. Run appropriate code to extract only the height in inches and save as a new variable. What is the average height?
#Answer Code Below

b=heights %>%
  mutate(height = parse_number(height))

b
mean(b$height)
```
```{r}
#For question 5 - In the dataset, heights, create a new variable containing height in cm. Use the conversion, 1 inch = 2.54 cm. What is the median height in cm?
#Answer Code Below
CM=b$height*2.54
HeightswithCM = data_frame(b,CM)
median(HeightswithCM$CM)


```
```{r}
#For question 6 - In the dataset, heights, standardize height in inches such that mean is 0 and standard deviation is 1. What is President Joe Biden's standardized height? 
#Answer Code Below

StandardValues = HeightswithCM%>%
  mutate(Standardized = scale(height, center = T, scale = T))%>%
  select(-height)%>%
  select(-CM)%>%
  select(-Name)

HeightswithCMandSV = data.frame(HeightswithCM,StandardValues)
HeightswithCMandSV

```



```{r}
#For question 7 - In the dataset height, bin height in inches into a categorical variable with the following four groups:

#Short: height<=66
#Average: 66 < height <= 69
#Tall: 69 < height <= 72
#Very Tall: height > 72
#Call this variable, height_cat4. 
#Based on this new variable, height_cat4, how many Presidents are categorized as being "Very Tall"?
#Answer Code Below

height_cat4 = factor(c("Short","Average","Tall","VeryTall"))

H = HeightswithCMandSV
H$height_cat4 = as.factor(ifelse(H$height<=66 , "Short",
                                 ifelse(H$height > 66 & H$height <=69 ,"Average",
                                        ifelse(H$height > 69 & H$height <= 72,"Tall",
                                               ifelse(H$height >=72,"Very Tall" ,"NONE")))))

H
table(H$height_cat4)
```

```{r}
#For uestion 8 - Review the following code

#library(dplyr); library(forcats)
#heights %>%
#      mutate(height_cat = fct_recode(.f = height_cat4, "Average" = "Short", "Average" = "Average","Tall" = "Tall","Very Tall" = "Very Tall"))

#How many levels (or categories) does height_cat contain?
#Answer Code Below
library(dplyr); library(forcats)
H %>%
  mutate(height_cat = fct_recode(.f = height_cat4, "Average" = "Short", "Average" = "Average","Tall" = "Tall","Very Tall" = "Very Tall"))

```

```{r}
#For question 9 - The data in the dataset, states contains information about the birth state of each President. Join the heights data with states data to get a dataset that contains US Presidents' height and birth state.  What is the average height (in inches) of Presidents born in New Jersey?
#Answer Code Below

states_and_heights = H%>%
  inner_join(states , by = 'Name')

head(states_and_heights)


states_and_heights%>%
  mutate(Birth.State = parse_character(states_and_heights$Birth.State))%>%
  filter(Birth.State == 'New Jersey')%>%
  summarize(Mean = mean(height, na.rm=TRUE))


```
```{r}
#Question 10 - Based on the the results of joining tables in the previous question, what is the average height (in inches) of Presidents born in New York?
#Answer Code Below
states_and_heights%>%
  mutate(Birth.State = parse_character(states_and_heights$Birth.State))%>%
  filter(Birth.State == 'New York')%>%
  summarize(Mean = mean(height, na.rm=TRUE))
```


#Assignment 2B

```{r}
library(dplyr)
library(readr)
library(tidyr)
```

```{r}
sales = 
  data.frame(company = c("Apple","Google","Amazon","Microsoft"),
             year_2017 = c(229234,110855,177866,96571),
             year_2018 = c(65595,136819,232887,110360),
             year_2019 = c(260174,161857,280522,125843),
             year_2020 = c(274515,182527,386064,143015),
             year_2021 = c(365817,257637,469822,168088))
```

```{r}
income = 
  data.frame(company = c("Apple","Google","Amazon","Microsoft"),
             year_2017 = c(61344,26178,4106,28970),
             year_2018 = c(70898,27524,12421,35011),
             year_2019 = c(63930,34231,14541,42933),
             year_2020 = c(66288,41224,22899,52826),
             year_2021 = c(108949,78714,24879,69903))
```

```{r}
model = paste('model',1:10,sep = '')
r2 = c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp = c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss = c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results = data.frame(model, r2, cp, rss)
```


```{r}
#For Question 1 - Let us create a data frame that compares ten models on three metrics: r2, rss, and cp. Run the following code to generate the data.
#Answer Code Below
model = paste('model',1:10,sep = '')
r2 = c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp = c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss = c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results = data.frame(model, r2, cp, rss)

#This data is in a format which limits the types of analysis that can be run and functions that can be applied. Please select the option that will transform results into a meaningful tall format with model in column 1, metric (which takes on values 'r2', 'cp', and 'rss') in column 2 and value (which contains numbers representing the three metrics) in column 3. The final dataset should have 30 rows and 3 columns.


results
results%>%  pivot_wider(names_from = model,values_from = r2:rss) #Not working
results %>% pivot_longer('model', 'metric', 'value')#Not wokring
results %>% pivot_longer(cols = cp:rss, names_to = 'metric', values_to = 'value')#Not wokring
results %>% pivot_longer(cols = 2:4, names_to = 'metric', values_to = 'value')#Wokring
```

```{r}
#For question 2 - What is the operating margin ratio (=income/sales) for Amazon in 2017?
#Answer Code Below
head(sales)

sales_tall =sales%>%
  pivot_longer(cols = 2:6,names_to = 'Years',values_to = 'Sales Values')

head(income)

income_tall = income%>%
  pivot_longer(cols = 2:6,names_to = 'Years',values_to = 'Income Values')
```

```{r}
sales_tall= sales_tall%>%
  mutate(Years=parse_number(Years))

income_tall= income_tall%>%
  mutate(Years=parse_number(Years))
```

```{r}
sales_tall
income_tall
```

```{r}
JOIN = sales_tall%>%
  inner_join(income_tall,by = c('company' = 'company','Years'='Years'))
```

```{r}
JOIN
```

```{r}
JOIN %>%
  filter(Years == 2017)%>%
  filter(company =='Amazon')
```
```{r}
AmazonOperatingMarginRatio2017 = 4106/177866
AmazonOperatingMarginRatio2017
```

```{r}
#For question 3 - What is the operating margin ratio (=income/sales) for Amazon in 2021?
#Answer Code Below
JOIN %>%
  filter(Years == 2021)%>%
  filter(company =='Amazon')

```

```{r}
AmazonOperatingMarginRatio2021 = 24879/469822
AmazonOperatingMarginRatio2021
```

```{r}
#For question 4 - What is the operating margin ratio (=income/sales) for Google in 2017?
#Answer Code Below
JOIN %>%
  filter(Years == 2017)%>%
  filter(company =='Google')
```

```{r}
GoogleOperatingMarginRatio2017 = 26178/110855
GoogleOperatingMarginRatio2017
```

```{r}
#For Question 5 - What is the operating margin ratio (=income/sales) for Google in 2021?
#Answer Code Below
JOIN %>%
  filter(Years == 2021)%>%
  filter(company =='Google')
```

```{r}
GoogleOperatingMarginRatio2021 = 78714/257637
GoogleOperatingMarginRatio2021
```
```{r}
#For Question 6 - What is the average operating margin ratio (=income/sales) across the four companies in  2017?
#Answer Code Below
JOIN%>%
  filter(Years == 2017)

```

```{r}
#Individual operating ratios
0.2999865+ #microsoft
  0.02308479+ #amazon
  0.2361463+ #google
  0.2676043 #apple

0.8268219/4 #Individual average 

```


```{r}
#individual average = 0.2067055
```

```{r}
#For Question 7 - What is the average operating margin ratio (=income/sales) across the four companies in  2021?
#Answer Code Below
JOIN%>%
  filter(Years == 2021)
```

```{r}
0.4158714+ # Microsoft
  0.0529541+ # Amazon
  0.3055229+ # Google
  0.2978238 # Apple 
```
```{r}
1.072172/4 #individual average
```


```{r}
#Individual Value = 0.268043
```


#Assignment 2C
```{r}
#install.packages('caret')
library(caret)
data(Sacramento)
houses_missing = Sacramento
set.seed(1031)
for(i in 1:sample(150:300,1)){
  set.seed(i)
  x = sample(1:nrow(Sacramento), 1)
  y = sample(c(3,4,5,7), 1)
  houses_missing[x, y] = NA
}
houses_missing
```
```{r}
#For question 1 - In the dataset, `houses_missing`, How many observations are missing? To answer this question, you have to count all the NA values. 
table(is.na(houses_missing))
```

```{r}
#For Question 2 - One way to address missing data is to delete all rows that contains missing data. If this was done with `houses_missing`, how many rows would be left?

#Hint: To answer this question, begin with a method for identifying a row with one or more NAs. table(rowSums(is.na(houses_missing)))
```
```{r}
#For question 3 - Impute the missing values using the `rf` method for imputation in `mice`. Set seed to 1031. Use defaults for all other arguments. What is the imputed price for the house in row 44? This house is located in the city, Antelope, in zip code z95843, has 2 bedrooms and 2 baths, 1043 sqft and type is Residential.
#install.packages("mice")
library(mice)
house_missing_rf = complete(mice(houses_missing,method = 'rf',seed = 1031))
house_missing_rf

```

```{r}
#For question 4 - Now, repeat the imputation process above but using the `cart` method in `mice`. Set seed to 1031. Use defaults for all other arguments. What is the imputed price for the house in row 44?
#Answer Code Below
house_missing_cart = complete(mice(houses_missing,method = 'cart',seed = 1031))
house_missing_cart
```

```{r}
#For question 5 - Next, use the `preprocess` function from `library(caret)` to impute missing values. Set seed to 1031. Use the method `bagImpute`. Use defaults for all other arguments. What is the imputed price for the house in row 44?
#Answer Code Below
set.seed(1031)
library(caret)
house_missing_bagImputemodel = preProcess(houses_missing,'bagImpute')
house_missing_bagImputemodel

house_missing_bagImputepredict = predict(house_missing_bagImputemodel,houses_missing)
house_missing_bagImputepredict
```


# Assignmnet 3

```{r}
install.packages("ISLR")
library(ISLR)
OJ
```


```{r}
#Question 6 - The Carseats data that accompanies library(ISLR2) contains sales of child car seats at 400 different stores. To access the data, load the 'ISLR2' package and call the `Carseats` data as illustrated below.


#Use simple random sampling to split the data into a train and test sample with 80% of the data in the train sample. Use a seed of 617. Compute the average price in the train sample and in the test sample.  What is the absolute difference in average price between the train and test samples?
#Answer Code Below

library(ISLR)
set.seed(617)

splitsimple=sample(x=1:nrow(Carseats),size=0.8*nrow(Carseats))
trainsimple = Carseats[splitsimple,]
testsimple = Carseats[-splitsimple,]

a=mean(trainsimple$Price)
b=mean(testsimple$Price)

a-b
```



```{r}
#Question 7
#Now, use stratified sampling to split the "Carseats" data into a train and test sample with 80% of the data in the train sample. Do the sampling in such a way that the distribution of Price is approximately equal in both samples. Use a seed of 617 and utilize createDataPartition for the split with groups set to 10.  Compute the average price in the train sample and in the test sample. What is the absolute difference in average price between the train and test samples?
#Answer Code Below
library(caret)
set.seed(617)
splitstratified=createDataPartition(y=Carseats$Price,
                                    p=0.8,
                                    list = F, 
                                    groups = 10)
trainstratified = Carseats[splitstratified,]
teststratified = Carseats[-splitstratified,]

c = mean(trainstratified$Price)
d = mean(teststratified$Price)

c-d

```
```{r}
#Question 8 
#Use stratified sampling to split the OJ dataset into a train and test sample with 80% of the data in the train sample. Ensure that the proportion of Minute Maid (MM) noted in the "Purchase" variable is approximately equal across train and test samples. Use a seed of 617. Utilize `sample.split()` from the `caTools` package for this problem. What is the proportion of Minute Maid (MM) purchases in the train dataset?
#Answer Code Below


OJ$MMYes_NO = ifelse(OJ$Purchase == 'MM','Yes','No')
library(caTools)
set.seed(617)
splitOJ = sample.split(Y= OJ$MMYes_NO, SplitRatio = 0.8)
trainOJ = OJ[splitOJ,]
testOJ = OJ[!splitOJ,]

prop.table(x=table(trainOJ$MMYes_NO))


```

#Assignment 4 A

```{r}
houses =read.csv("houses.csv")
nrow(houses)

```



```{r}
#For question 1 - Let us start by splitting the data into a train and test sample such that 70% of the data is in the train sample. Use createDataPartition from the caret package with groups = 100. Set seed to 1031 and be sure to do this just before passing createDataPartition()

#What is the average house price in the train sample? Do not round your answer.
#Answer Code Below
library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
trainhouses = houses[split,]
testhouses = houses[-split,]
averagerpricetrain = mean(trainhouses$price)
averagerpricetrain
```
```{r}
#For question 2 - What is the average house price in the test sample? Do not round your answer



averagerpricetest = mean(testhouses$price)
averagerpricetest
```

```{r}
#For question 3 - It is best to explore the data to better understand the structure and nature of the data and to spot unusual values. But, such exploration must only be done with the train data. Now, review the following code before running it to understand what it is doing.
#Answer Code Below
library(dplyr); library(tidyr); library(ggplot2)
trainhouses %>%
  select(id,price:sqft_lot,age)%>%
  gather(key=numericVariable,value=value,price:age)%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales='free_y')
#You have no doubt noticed a few outliers. Inspect the outlier for bedrooms. What is the living area (sqft_living) for the house with the most bedrooms?
#Answer Code Below

trainhouses%>%
  filter(bedrooms>30)
```
```{r}
#For Question 4 - It seems reasonable to expect or hypothesize that larger houses cost more. Construct a scatterplot to examine the relationship between sqft_living and price, placing sqft_living on the horizontal axis and price on the vertical axis. What is the direction of the points?
#Answer Code Below

trainhouses%>%
  ggplot(aes(x=sqft_living , y = price)) + 
  geom_point()

```
```{r}
#For question 5 - What is the correlation between sqft_living and price? (Do you see a link between the scatterplot and the correlation measure?)
#Answer Code Below

sqftliving = trainhouses$sqft_living
prices = trainhouses$price

correaltion = cor(sqftliving,prices)
correaltion
```

#Assignmnet 4B
```{r}
#For question 1 - 
#Construct a simple regression to predict house price from area (sqft_living). Call this model1. Let us examine how well the model is predicting price. What is the p-value for the F-statistic?
#Answer Code Below

library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
trainhouses = houses[split,]
testhouses = houses[-split,]
```

```{r}
#For question 1 and 2 - What is the R2 (R-squared) for model1? (Think about what this says about model performance).
#Answer Code Below
model1 = lm(price ~ sqft_living, trainhouses)
summary(model1)
```

```{r}
#For question 3 - What is the RMSE (i.e., root mean squared error) for model 1?


#Answer Code Below

predict(model1 , newdata =  data.frame(sqft_living = trainhouses$sqft_living))

summary(model1)


price_real = trainhouses$price
price_predicted = predict(model1)

e = price_real - price_predicted
errorsquared = e^2
msetrain = mean(errorsquared)
rmsetrainmodel1 = sqrt(msetrain) ; rmsetrainmodel1

```
```{r}
#For question 4 - Since model1 is built on sample data, it is important to see if the coefficient estimates will be non-zero in the population. Based on model1 results and assuming an alpha of 0.05, indicate your agreement with the following statement: The coefficient of sqft_living is significantly different from zero.
#Answer Code Below
#TRUE 
pred_test = predict(model1 , newdata = testhouses )
rmse_test = sqrt(mean((pred_test-testhouses$price)^2)) ; rmse_test

summary(model1)

```

```{r}
#For Question 5 - Based on model1, on average, what would a 1400 square foot house cost?


#Answer Code Below

a = predict(model1,newdata=data.frame(sqft_living=1400)) 
a
```



```{r}
#For Question 6 - Based on model1, if a homeowner were to put in a 200 square foot addition on the house, by how much would the price be expected to go up?
#Answer Code Below

#Price = -47764.278 + 282.092*sqft_living

b = 282.092*200
b

```


```{r}
#For Question 7 - Construct another simple regression to predict house price from waterfront. Call this model2. Note, for the waterfront variable, 1 indicates the house has a view to the waterfront. What is the R2 for model2?
#Answer Code Below
head(trainhouses)
model2 = lm(price~factor(waterfront),data=trainhouses)

summary(model2)


```



```{r}
#For question 10 - Which of the two models has a lower RMSE (root mean squared error)? A lower RMSE reflects better predictions.
#Answer Code Below

predict(model2 , newdata =  data.frame(waterfront = trainhouses$waterfront))

pricerealmodel2 = trainhouses$price
price_predicted_waterfront = predict(model2)


emodel2 = price_real - price_predicted_waterfront
errorsquaredmodel2 = emodel2^2
msetrainmodel2 = mean(errorsquaredmodel2)
rmsetrainmodel2 = sqrt(msetrainmodel2) ; rmsetrainmodel2


rmsetrainmodel1 ; rmsetrainmodel2

```

#Assignment 4C

```{r}
library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
trainhouses = houses[split,]
testhouses = houses[-split,]
```
```{r}
#For question 1 & 2
# Question 1 - Now let us use both the predictors from model1 and model2 to predict price. Use sqft_living and waterfront to predict price. Do not model their interaction. Call this model3.
#Question 2 - Using model3, quantify the impact of a waterfront view on the expected price, while holding area (sqft_living) constant. Specifically, how much more is the expected price of a house with a waterfront view compared to one without a waterfront view, while holding area (sqft_living) constant? This question is slightly different from the question asked of model2 and so is the answer.
#How does R2 of model3 compare to that of model1 and model2? Select all that apply.
#Answer Code Below
model3 = lm(price ~ sqft_living + factor(waterfront), trainhouses)
summary(model3)


model3a = lm(price ~ sqft_living + factor(waterfront) + sqft_living*factor(waterfront) , trainhouses)
summary(model3 ) ; summary(model3a)


#model 1 - 0.4968
#model 2 - 0.05889
#model 3 - 0.5291


price_real_model3 = trainhouses$price
price_predicted_3 = predict(model3)


emodel3 = price_real_model3 - price_predicted_3
errorsquaredmodel3 = emodel3^2
msetrainmodel3 = mean(errorsquaredmodel3)
rmsetrainmodel3 = sqrt(msetrainmodel3) ; rmsetrainmodel3
```
```{r}
#For question 3 - Now, run a multiple regression model with the following predictors:

#bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age
#Call this model4. What is the R2 for model4?
#Answer Code Below

model4 = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,trainhouses)
summary(model4)
```
```{r}
#For question 4 - What is the RMSE for model4?


#Answer Code Below

price_real_model4 = trainhouses$price
price_predicted_4 = predict(model4)


emodel4 = price_real_model4 - price_predicted_4
errorsquaredmodel4 = emodel4^2
msetrainmodel4 = mean(errorsquaredmodel4)
rmsetrainmodel4 = sqrt(msetrainmodel4) ; rmsetrainmodel4
```
```{r}
#For question 5 - Which of the four models constructed so far has the lowest RMSE?


#Answer Code Below

#model 1 - 261068.9
#model 2 - 357030.1
#model 3 - 252545.7
#model 4 - 217865.8
```


```{r}
#For question 6 Based on model4, which of the following predictors have a statistically significant influence on price? Select all that apply.
#Answer- all
```


```{r}
#For question 7 - Based on model4, if a person decides to add another bathroom, what would be the increase in expected price, holding all other predictors constant?
#Answer- 5.337e+04
```

```{r}
#For question 8 - Which of the predictors in model4 exerts the strongest influence on price?


#Answer Code Below
library(lm.beta)
lm.beta(model4)


# age -         0.29436960
# sqft_living - 0.43082385
# bedrooms -   -0.09701320
# bathrooms -   0.11107546
# grade -       0.38970373 
```


```{r}
#For question 9 & 10 
#Question 9 - Finally, let us apply model4 estimated on the train sample to the test sample. What is the R2 for the test sample?
#Question 10 - What is RMSE for model4 on the test sample?


#Answer Code Below

pred_test = predict(model4 , newdata = testhouses)
rmse_test = sqrt(mean((pred_test-testhouses$price)^2)) 


sse_test = sum((pred_test - testhouses$price)^2)
sst_test = sum((mean(trainhouses$price) - testhouses$price)^2) 

RSqaured = 1-sse_test/sst_test

rmse_test ; RSqaured
```

#Assignment 5A eBayAssignment

```{r}
ebay = read.csv('ebayAssignment.csv')
```
```{r}
#Question 1 - How many rows are in the data?
#Answer Code Below

nrow(ebay)
```
```{r}
#Question 2 - How many iPads are Black in color?


#Answer Code Below
table(ebay$color)
```
```{r}
#Question 3  - Which of the following iPads does this dataset contain? (Select all that apply)

#Answer Code Below
table(ebay$productline)

```
```{r}
#Question 4 - What is the uniqueID of the iPad with the highest startprice?


#Answer Code Below
library(dplyr)

ebay%>%
  arrange(desc(startprice))
```

# Assignment 5B



```{r}
#Question 1
#Split the Data into a train sample and a test sample using a seed of 196 such that 80% of the data is in the train sample. Use sample.split from library(caTools). Hereafter, we will only use the train sample for exploring and building the model. The test sample will only be utilized for evaluating the model.
library(caTools)
set.seed(196)

split = sample.split(Y = ebay$sold, SplitRatio = 0.8)
trainebay = ebay[split,]
testebay = ebay[!split,]
#Answer Code Below
How many rows are in the train sample?
  
  nrow(trainebay)
```
```{r}
library(dplyr)

a = trainebay%>%
  filter(sold == 0)

b = trainebay%>%
  filter(sold == 1)


median(a$startprice) ; median(b$startprice)
```


```{r}
#Question 2 - What is the median startprice of iPads that sold? 
#Question 3 - What is the median startprice of iPads that did not sell?
#Answer Code Below
tapply(X = trainebay$startprice, INDEX = trainebay$sold, 'median')
```
```{r}
#Question 4 -  Now, let us run a model to predict the variables that influence whether an iPad will be sold or not. Since the variable to be predicted only takes on two values, we will use a logistic regression model. Use the 'glm' function to build a model with 'sold' as the dependent variable and the following independent variables: biddable, startprice, condition, cellular, carrier, color, storage, productline, noDescription, charCountDescription, upperCaseDescription, startprice_99end Be sure to set family as 'binomial'.

#What is the AIC?
#Answer Code Below
model1ebay = glm(formula = sold~biddable + startprice + condition + cellular + carrier + color + storage + productline + noDescription + charCountDescription + upperCaseDescription + startprice_99end, 
                 data = trainebay , 
                 family = 'binomial')

summary(model1ebay)
```
```{r}
#Question 5 - Now, let us examine individual variables. Which of the following variables has a significant influence on whether an iPad is sold or not? (Select all that apply). Use a less conservative alpha of 0.10, i.e., compare p-value to 0.10.

- biddable, startprice , productline , cellular
```

```{r}
#Question 6 - Based on the results of the model, does a 99 ending for startprice increase the chance of an iPad being sold?
#Question 7 - Based on the results of the model, does color of the iPad have an impact on whether an iPad is sold?
# Both Answers = No
```

#Assignment 5 C
```{r}
#Question 1 - Simpler models are generally preferred to more complex models because they are less likely to overfit the data. So, let us drop out non-signficant variables from model1 above but keep variables that previous research or experience indicates should have an effect. So, estimate model2 with the following variables: biddable, startprice, condition, storage, productline, upperCaseDescription, startprice_99end
#What is the AIC?
#Answer Code Below
library(caTools)
set.seed(196)

split = sample.split(Y = ebay$sold, SplitRatio = 0.8)
trainebay = ebay[split,]
testebay = ebay[!split,]

model2ebay = glm(formula = sold~biddable + startprice + condition + storage + productline + upperCaseDescription +  startprice_99end, 
                 data = trainebay , 
                 family = 'binomial')

summary(model2ebay)
```
```{r}
#Question 2 - Based on the coefficient of upperCaseDescription, what advice would you give someone selling iPads on eBay?
#Answer - Use a lot of upper case letters in the description
```

```{r}
#Question 3 - You will note that the data contains a number of factor variables. In order to model factor variables, they have to be dummy coded. Fortunately, glm and lm functions automatically dummy code factor variables and then run the dummy variables in the model. The first level is usually selected to be the baseline or reference variable to which each of the other levels is compared. Review the results of model2 and the coefficients of the variables. (After controlling for the effects of all other variables in the model), what sells better iPad3 or iPad 1?
#Answer - Ipad 1
```

```{r}
#Question 4 - If startprice goes up by $1, what will be the % reduction in the chance of selling an iPad. To interpret coefficients in logistic regression, you have to exponentiate the coefficient. E.g., exp(coefficient)
#Answer - 1%
#Answer Code Below
(exp(model2ebay$coefficients[3])-1)*100 
exp(-0.0090062)

```

```{r}
#Question 5 - Based on model2 (and controlling for the effects of all other variables), how much more (or less) likely is an iPad Air 1/2 to sell compared to an iPad 1?
#Answer Code Below
(exp(model2ebay$coefficients[12])-1)*100 
exp(1.8873038)
```
```{r}
#Question 6 - Now, let us run one more model called model_productline. For this model, predict the variable 'sold' using only 'productline'. Is the sign of the coefficient for iPad Air1/2 in this model the same as that in model2?
#Answer Code Below
model_productline = glm(formula = sold~productline, 
                        data = trainebay , 
                        family = 'binomial')

summary(model_productline)
```

#Assignment 5D

```{r}
#Prep
library(caTools)
set.seed(196)

split = sample.split(Y = ebay$sold, SplitRatio = 0.8)
trainebay = ebay[split,]
testebay = ebay[!split,]

```

```{r}
#Prep
model2ebay = glm(formula = sold~biddable + startprice + condition + storage + productline + upperCaseDescription +  startprice_99end, 
                 data = trainebay , 
                 family = 'binomial')

summary(model2ebay)
```
```{r}
#Question 1 - Make predictions on the test set using model2. Place all the predictions in a variable "pred". Now, let us use the model to find out what is the probability of sale for an iPad with UniqueID 10940? You could do this by running the following code.
#pred[test$UniqueID==10940]

#Answer Code Below

pred = predict(model2ebay , newdata = testebay , type = 'response')
pred[testebay$UniqueID==10940]
```
```{r}
#Question 2 - What is the accuracy of model2 on the test set? Use a threshold of 0.5.


#Answer Code Below

ct1 = table(sold = testebay$sold , prediction = as.integer(pred > 0.5)) ; ct1
(ct1[1,1] + ct1[2,2])/ (nrow(testebay))
```
```{r}
#Question 3 - Let us see if there is any incremental benefit from using model2 over the baseline. Note, if you examine 'sold' in the train sample, it would be easy to see that most iPads don't sell. If one did not have any information on the independent variables one would predict an iPad will not sell. Baseline is the proportion (percent/100) of times one would be correct in the test sample if one were to make this assumption. 
#Answer Code Below

sum(testebay$sold == 0) / nrow(testebay)
```

```{r}
#Question 4 - Is model2 performing better than the baseline?


#answer - Yes 
```


```{r}
#Question 5 - The accuracy measure depends on the cut-value (or threshold) used. Hence a more popular measure is area under the curve (or AUC). AUC is computed by finding the area under the curve of a plot of Sensitivity vs. 1-Specificity. AUC is model performance measure that is independent of any particular threshold. You can do this by running the following code. Ensure that your set of predictions is called 'pred' and your test sample is called test
# install.packages('ROCR')   # if you have not installed ROCR, be sure to install it first. 
library(ROCR)
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve

#Answer Code Below

library(ROCR)
ROCRpred = prediction(pred,testebay$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve
```
#Assignmnet 6A

```{r}
#Prep
houses = read.csv("houses.csv")

```
```{r}
library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
trainhouses = houses[split,]
testhouses = houses[-split,]
```
```{r}
#Question 1 - Let us start by splitting the data into a train and test sample such that 70% of the data is in the train sample. Use createDataPartition from the caret package with groups = 100 and list=F. Set seed to 1031 and be sure to do this just before passing createDataPartition()
#What is the average house price in the train sample?
#Answer Code Below

mean(trainhouses$price)
```

```{r}
#Question 2 - Now, examine bivariate correlations with price to identify variables that are weakly related to (or not relevant) for predicting price. Which of the following variables has the weakest relationship with price?
#Answer - condition

round(cor(trainhouses),2)
```
```{r}
#Question 3 Now, examine correlations amongst the predictors. Which pair has the highest bivariate correlation? You can visualize the relationship by running the following code. This code assumes the train sample is called train.  

install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower', colors=c('red3','white','green3'))
However, to answer the question, you will have to examine bivariate correlations and answer the question: which pair has the highest bivariate correlation?
  
  #sqft_basement and age - 0.14
  #sqft_living and sqft_lot - 0.17
  #sqft_basement and sqft_lot - 0.01
  #sqft_living and bathrooms - 0.75 Correct 
  #Answer Code Below
  
  library(ggcorrplot)
ggcorrplot(cor(trainhouses[,c(3:7, 10:13,16)]),method = 'square', colors=c('red3','white','green3'))
```
```{r}
#Question 4 - Theory or domain knowledge can help identify sources of multicollinearity. The area of a house (sqft_living) is the sum of area above the basement (sqft_above) and the basement (sqft_basement). This is useful to know because multicollinearity can arise not only from associations between a pair of predictors but also between a linear combination of predictors. But, first let's verify this by computing the correlation between sqft_living and the sum of sqft_above and sqft_basement. What is the correlation?

cor.test(trainhouses$sqft_living,(trainhouses$sqft_above+trainhouses$sqft_basement))
```
```{r}
#Question 5 - As is apparent from the previous question, the threat of collinearity can also come from linear relationships between sets of variables. One way to assess the threat of multicollinearity in a linear regression is to compute the Variance Inflating Factor (VIF). To do this, run a multiple regression model with the following predictors: bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age. Call this model1. Now, use vif() from library(car) as follows

# install.packages('car')
library(car)
vif(model1)

Which predictor has the highest VIF?
  model1houses = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,
                    data = trainhouses)
#Answer Code Below
library(car)
vif(model1houses)
```

#Assignment 6B

```{r}
library(caret)
set.seed(1031)
split = createDataPartition(y=houses$price,p = 0.7,list = F,groups = 100)
train = houses[split,]
test = houses[-split,]
```

```{r}
#Question 1 - Let's examine some algorithm driven ways of feature selection predictors for price. We are going to select from the following predictors: bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Evaluate all possible subsets to identify the best-six predictor model. Which of the following variables are included in the best six-predictor model? (Select all that apply)
#Answer Code Below
library(leaps)

model2houses = regsubsets(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view +  condition + grade + age,
                          data=train, 
                          nvmax=10)
summary(model2houses)

```
```{r}
#Question 2- What is the R2 for the best 6 predictor model?


#Answer Code Below
model3houses = lm(price~bedrooms + sqft_living + waterfront + view + grade + age , 
                  data = train)

summary(model3houses)
```
```{r}
#Question 3 - Next, run a forward stepwise regression model. As we did when choosing from all possible subsets, we are going to select from the following predictors: bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Which of the following variables were included in the best model? (Select all that apply)
#Answer Code Below
start_mod_houses = lm(price~1,data=train) # selects only intercepts because we begin with no predictors

empty_mod_houses = lm(price~1,data=train) # identical as the earlier mod

full_mod_houses = lm(price~.,
                     data=train)  # contains all the variables

forwardStepwise_houses = step(start_mod_houses,
                              scope=list(upper=full_mod_houses,lower=empty_mod_houses),
                              direction='forward') 
#summary(forwardStepwise_houses)
```
```{r}
#Question 4 - Now, run a backward stepwise regression model. As we did when choosing from all possible subsets, we are going to select from the following predictors.bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Which of the following variables were included in the best model? (Select all that apply
#Answer Code Below
start_mod_houses_b = lm(price~.,
                        data=train)
empty_mod_houses_b = lm(price~1,data=train)
full_mod_houses_b = lm(price~.,
                       data=train)
backwardStepwise_houses_b = step(start_mod_houses_b,
                                 scope=list(upper=full_mod_houses_b,lower=empty_mod_houses_b),
                                 direction='backward')

#summary(backwardStepwise_houses_b)
```
```{r}
#Question 5 - Next, run a hybrid stepwise regression model, where both forward and backward algorithms operate simultaneously. As we did when choosing from all possible subsets, we are going to select from the following predictors. bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Which of the following variables were included in the best model? (Select all that apply)
#Answer Code Below
start_mod_houses_s = lm(price~1,data=train) # selects only intercepts because we begin with no predictors

empty_mod_houses_s = lm(price~1,data=train) # identical as the earlier mod

full_mod_houses_s = lm(price~.,
                       data=train)  # contains all the variables

hybridStepwise_houses_s = step(start_mod_houses_s,
                               scope=list(upper=full_mod_houses_s,lower=empty_mod_houses_s),
                               direction='both')
```
```{r}
#Question 6 - Now, use a Lasso model to select features. As we did above, we are going to select from the following predictors: bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Use a Lasso model to select features. Set the seed to 1031. Use cv.glmnet which finds the best lambda through 10-fold crossvalidation. Which of the following variables were included in the best model? (Select all that apply)
#Answer Code Below
library(glmnet)

x_houses = model.matrix(price~.-1,data=train) #list of predictors except the predictors
y_houses = trainhouses$price

set.seed(1031)
lasso_houses = cv.glmnet(x = x_houses, 
                         y = y_houses, 
                         alpha = 1,
                         type.measure = 'mse')

coef(lasso_houses)
```
```{r}
#Question 7 - What is the R2 for the model selected by lasso?


#Answer Code Below
model4_houses = lm(price~bathrooms + sqft_living + waterfront + view + grade + age,
                   data = train)

summary(model4_houses)
```
```{r}
#Question 8 - Dimension reduction: Now, rather than selecting individual variables, we will capture the essence in a few components so as to retain at least 90% of the information. Run the following code to reduce the predictors to a few components.

library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price
#How many components did we use to capture the information in the predictors?
#Answer Code Below

library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

trainComponents

```

```{r}

```

```{r}
#Question 9 - Now, use only the components to predict price in the train sample. What is the R2?


#Answer Code Below

model5_houses = lm(price ~ .,
                   dat = trainComponents)
summary(model5_houses)
```
```{r}
#Question 10 - Next, let us impose the trained component structure on the test sample. Run the following code to do this.

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price
#Next, apply the train model created with components to the test-component dataset just created. Compute R2 on the test set. Remember R2 = 1 – sse/sst.

#What is the R2 in the test sample?
#Answer Code Below

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price


pred1 = predict(model5_houses,newdata=testComponents)
sse = sum((pred1-testComponents$price)^2)
sst = sum((mean(trainComponents$price) - testComponents$price)^2)
r2_test = 1 - sse/sst
r2_test
```

#Assignment 7A


```{r}
wages= read.csv("assignment7_wages.csv")

```

```{r}
#Question 1 - Which of the following variables are non-metric?


#Answer- Sex and Race 

```

```{r}
#Question 2 - Some of the values of earn in this dataset are below 0. Assuming the data is called wages, remove these data points by running the following code

wages = wages[wages$earn>0,]
#What fraction (a number between 0 and 1) of the respondents are female?
#Answer - 0.627924
wages = wages[wages$earn>0,]
#Answer Code Below
library(dplyr)

a = wages%>%
  filter(sex == 'female')


nrow(a)/nrow(wages)
```
```{r}
#Question 3 - Now, split the data into a train and test sample. Use the following code for the split. 

set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]
#Approximately, what percent of the data is placed in the training sample?
#Answer Code Below
#Answer - 0.75

set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]

nrow(train)/nrow(wages)
```

```{r}
#Question 4- How many observations are in the training set?
#Answer Code Below
#answer - 1026
nrow(train)

```

```{r}
#Question 5 - Which of following races earns the least? Answer based on the train set.
#Answer Code Below
#answer- Hispanic
library(ggplot2)
train%>%
  group_by(race)%>%
  summarise(mean = mean(earn))

```

#Assignment 7B


```{r}
wages= read.csv("assignment7_wages.csv")
wages = wages[wages$earn>0,]
head(wages)
```
```{r}
set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]
```


```{r}
#Question 1- Now, construct a linear regression model to predict earn using all other variables. Call this model1.

#Which of the following variables are significant (p < 0.05)? Select all that apply.


#Answwer- sex , ed , age , height 

model1 = lm(earn ~ height + sex + race + ed + age , train)
summary(model1)


```

```{r}
#Question 2 - For model1, compute the root mean squared error (RMSE) on the train sample. Later on, we will use this RMSE a linear regression model to a regression tree.
#Answer - 26957.41 

pred_train = predict(model1)
rmse_train = sqrt(mean((pred_train - train$earn)^2))

rmse_train

```



```{r}
#Question 3
#Sometimes variables in combination act differently than when evaluated individually model1 does not incorporate such combinations called interactions. Let us try and understand the meaning of an interaction using this dataset. One may speculate that education has a positive influence on earnings. This is a main effect. Similarly one might expect that men on average earn more than women. This is also a main effect. Now, let us propose an interaction hypothesis. Education impacts earning differently for men than for women. Specifically, education boosts earnings more for men than for women. This is an interaction effect. Visualize this interaction effect by plotting a regresssion between ed and earn for each gender. 


ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))
#What is the approximate difference in earn between 12 years and 16 years of education for Males?
#Answer - - 20000
```

```{r}
#Question 4 - Based on the chart drawn for the previous question, what is the approximate difference in earn between 12 years and 16 years of education for Females?
#Answer - 15000
```

```{r}
#Question 5 - Now, construct a regression that models main and interaction effects of ed and sex on earn.

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)

#Which of the following variables are significant? (Select all that apply)
#Answer - ed , sexmale:ed

model_sex_ed = lm(earn~sex + ed + sex*ed,train)
summary(model_sex_ed)

```

```{r}
#Question 6 - Based on model_sex_ed, the variables sex and ed interact in influencing earn.


#Answer - True
```

```{r}
#Question 7 - Now, that we understand what an interaction effect is, construct a model that incorporates all the variables in model1 and the interaction between sex and ed. Call this model2.  (To include the interaction effect, you need to add a variable called sex*ed like you did in the model_sex_ed.)

#What is the RMSE for model2 (on train sample)?
#Answer - 26908.29 

model2 = lm(earn ~ height + sex + race + ed + age + sex*ed, train)


pred_train2 = predict(model2 , train)
rmse_train2 = sqrt(mean((pred_train2 - train$earn)^2))

rmse_train2
```

```{r}
#Question 8 - The RMSE for model2 is lower than RMSE for model1. (Think why?)


#Answer - True
```


```{r}
#Question 9 - Now, construct another model called model3 by adding the interaction between sex and age to model2.

#What is the RMSE for model3 (on train sample)?
#Answer - 26860.93

model3  = lm(earn ~ height + sex + race + ed + age + sex*ed + sex*age , train)

pred_train3 = predict(model3 , train)
rmse_train3 = sqrt(mean((pred_train3 - train$earn)^2))

rmse_train3

```


```{r}
#Question 10 - Now, construct another model called model4 by adding the interaction between age and ed to model3. What is the RMSE for model4 (on train sample)?
#Answer - 26847.67

model4  = lm(earn ~ height + sex + race + ed + age + sex*ed + sex*age + age*ed , train)

pred_train4 = predict(model4 , train)
rmse_train4 = sqrt(mean((pred_train4 - train$earn)^2))

rmse_train4
```

```{r}
#Question 11 - Finally, construct a model called model5 that considers all possible pairwise interactions. Adding all possible interactions can be tedious, so we will use a shortcut. Review the following code before running it. 

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
#What is the RMSE for this model (on train sample)?
#Answer - 26519.18

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)

pred_train5 = predict(model5 , train)
rmse_train5 = sqrt(mean((pred_train5 - train$earn)^2))

rmse_train5
```
```{r}
#Question 12 - Which of the following variables from model1 are statistically significant in model5? (Can you think of why the statistical significance of variables has changed?)
#Answer - none of the above  

summary(model5)
```

#Assignment 7C

```{r}
#Question 1 Develop a regression tree model to predict earn using all other variables. Use library(rpart) to construct the regression tree model and library(rpart.plot) to plot it. Since this is a regression tree, do not set the method='class', as the latter is used for classification trees.  Call this tree1. Next, use the following code to plot tree1 with an argument, digits=4 to get 2-decimal precision.  

rpart.plot(tree1, digits=5)
#Which is the first variable to be used for the split?
#Answer - sex

library(rpart) ; library(rpart.plot)

tree1 = rpart(earn~height+sex+race+ed+age,data = train, method = 'anova')
rpart.plot(tree1)


```
```{r}
#Question 2 - Based on the tree plot for tree1, which of the following is true about people who earn the most?
#Answer - - More than 18 years of education
```

```{r}
#Question 3 - Based on the tree plot for tree1, which of the following is true about people who earn the least?
#Answer - Age less than 29 years
```

```{r}
#Question 4 - How many leaves does tree1 have?


#Answer - 11 
```

```{r}
#Question 5 - Now, compute the RMSE for tree1. What is the RMSE (on train sample)?


#Answer - 25143.97

pred_tree1 = predict(tree1)

sqrt(mean((pred_tree1 - train$earn)^2))
```

```{r}
#Question 6 - Now, let us change the defaults for the tree model by, first reducing complexity and then adding complexity. We use the minbucket parameter, one of many ways to change complexity of the default tree. You can learn more about controlling tree structure by checking help for rpart.control.  First, construct a simpler tree called treeSimp1 by adding the following argument within the rpart function: control=rpart.control(minbucket=20)

treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
#How many leaves does treeSimp1 have?
#Answer - 9 

treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
rpart.plot(treeSimp1)
```
```{r}
#Question 7 - What is the RMSE for treeSimp1 (on train sample)?


#Answer - 25683.12

pred_treesimp1 = predict(treeSimp1)

sqrt(mean((pred_treesimp1 - train$earn)^2))
```
```{r}
#Question 8 -Construct an even simpler tree with minbucket of 50. Call this model, treeSimp2

#How many leaves does treeSimp2 have? 
#Answer - 6

treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2)

```
```{r}
#Question 9 - What is the RMSE for treeSimp2 (on train sample)?


#Answer - 26728.12

pred_treesimp2 = predict(treeSimp2)

sqrt(mean((pred_treesimp2 - train$earn)^2))
```

```{r}
#Question 10 - Now, let us construct some large bushy trees. Construct a tree with minbucket of 5. Call it treeComplex1.

#What is the RMSE (on train sample)?
#Answer - 25143.97

treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1)

pred_treecomp1 = predict(treeComplex1)

sqrt(mean((pred_treecomp1 - train$earn)^2))

```

```{r}
#Question 11 - Next, construct a maximal tree called treeComplex2. For this tree, set minbucket to 1.

#What is the RMSE (on train sample)?
#Answer - 22725.7 

treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2)

pred_treecomp2 = predict(treeComplex2)

sqrt(mean((pred_treecomp2 - train$earn)^2))

```

#Assignment 7D

```{r}
#Question 1 - For an unbiased measure of model performance, we assess performance on the test sample.

#What is the test RMSE for the linear regression model with the lowest train RMSE?


#Answer - 27154.5

model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)

pred_train5 = predict(model5 , train)
rmse_train5 = sqrt(mean((pred_train5 - train$earn)^2))

rmse_train5

pred_test5 = predict(model5 , newdata = test)
rmse_test5 = sqrt(mean((pred_train5 - test$earn)^2))

rmse_test5

```
```{r}
#Question 2 - What is the test set RMSE for the tree1?


#Answer - 27059.98

tree1 = rpart(earn~height+sex+race+ed+age,data = train, method = 'anova')

pred_tree1_test = predict(tree1 , newdata = test)
rmse_tree1_test = sqrt(mean((pred_tree1_test - test$earn)^2))

rmse_tree1_test
```

```{r}
#Question 3 - What is the test set RMSE for treeSimp2
#Answer - 27990.53

treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))

pred_treeSimp2_test = predict(treeSimp2 , newdata = test)
rmse_treeSimp2_test = sqrt(mean((pred_treeSimp2_test - test$earn)^2))

rmse_treeSimp2_test

```

```{r}
#Question 4 - What is the test set RMSE for treeComplex2?


#Answer - 30141.22

treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))

pred_treeComplex2_test = predict(treeComplex2 , newdata = test)
rmse_treeComplex2_test = sqrt(mean((pred_treeComplex2_test - test$earn)^2))

rmse_treeComplex2_test

```
```{r}
#Question 5 - Which of the above four models performed best on the test sample?


#Answer - tree1
```


#Assignment 8 A
```{r}
library(ISLR)
dataOJ = OJ

dataOJ

```
```{r}
OJ$MMYes_NO = ifelse(OJ$Purchase == 'MM','Yes','No')
library(caTools)
set.seed(1234)
splitOJ = sample.split(Y= OJ$MMYes_NO, SplitRatio = 0.7)
trainOJ = OJ[splitOJ,]
testOJ = OJ[!splitOJ,]
```

```{r}
#Question 1 - Load the dataset OJ by calling the ISLR library.

library(ISLR)
data(OJ)
#Next, split the dataset OJ into train and test samples such that 70% of data is placed in the train sample. Use the caTools package to do the split and set seed to 1234.

#How many rows are in the train sample?
#Answer - 749

nrow(trainOJ)
```
```{r}
#Question 2 - In the train sample, how many Minute Maid purchases were made


#Answer - 292

table(trainOJ$Purchase == 'MM')

```
```{r}
#Question 3 - What is the average Price for Minute Maid (in the train sample)? Do not round your answer and do not include units of currency.


#Answer - 2.087223

mean(trainOJ$PriceMM)


```
```{r}
#Question 4 - What is the average Discount for Minute Maid (in the train sample)? Do not round your answer and do not include currency units.


#Answer - 0.1237116

mean(trainOJ$DiscMM)
```

```{r}
#Question 5 - How many purchases of Minute Maid were made in Week 275?


#Answer - 17 

trainOJ%>%
  filter(Purchase == "MM")%>%
  filter(WeekofPurchase == 275 )

```

#Assignment 8B

```{r}
library(ISLR)
dataOJ = OJ

dataOJ

```

```{r}
OJ$MMYes_NO = ifelse(OJ$Purchase == 'MM','Yes','No')
library(caTools)
set.seed(1234)
splitOJ = sample.split(Y= OJ$MMYes_NO, SplitRatio = 0.7)
trainOJ = OJ[splitOJ,]
testOJ = OJ[!splitOJ,]
```

```{r}
#Question 1 -Let us construct a classification tree model to predict "Purchase" of Citrus Hill and MinuteMaid using the following variables: Price, Discount, Special and Percent Discount for each of Citrus Hill and Minute Maid, Loyalty for Citrus Hill and difference in Sale Price Specifically, the variables are PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH What is the auc for the test sample? Do not round your answer.

#Tip: The predict function when applied to a classification tree (rather than a regression tree) produces a matrix of probabilities of 0 and probabilities of 1. When entering the predictions into the prediction function of the ROCR package, you need to use pred[,2] and not pred. As an e.g.,

#pred = predict(treeModel,newdata=test)

#ROCRpred = prediction(pred[,2],test$dependentVariable)
#Answer -  0.8628776

treeModel = rpart(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH , trainOJ , method = 'class' )

pred_treeModel = predict(treeModel,newdata=testOJ , type='prob')[,2]


pred_treeModel

library(ROCR)
ROCRPred = prediction(predictions = pred_treeModel, labels = testOJ$Purchase)
auc = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc

```

```{r}
#Question 2 - Now, tune the model to optimize complexity. Use 10-fold cross-validation and test cp values ranging from 0 to 0.1 in steps of 0.001. What is the optimal cp? Be sure to set seed to 100 just before running the train function.
#Answer -  0.005

tuneGrid = expand.grid(cp = seq(0,0.1,0.001))
library(caret)
trControl = trainControl(method = 'cv',number = 10)
set.seed(100)
tree_cv = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                data = trainOJ,
                method = 'rpart',
                trControl = trControl, 
                tuneGrid = tuneGrid)

tree_cv$bestTune

```
```{r}
#Question 3 - Rerun the tree model with the optimal cp value. What is the auc for this model on the test sample?
#Answer -  0.8628776

treeModelbestcp  = rpart(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH , trainOJ , method = 'class' , cp = tree_cv$bestTune)

pred_treeModelbestcp = predict(treeModelbestcp,newdata=testOJ , type='prob')[,2]


library(ROCR)
ROCRPredtreeModelbestcp = prediction(predictions = pred_treeModelbestcp, labels = testOJ$Purchase)
auctreeModelbestcp = as.numeric(performance(prediction.obj = ROCRPredtreeModelbestcp,measure = 'auc')@y.values); auctreeModelbestcp

```

#Assignment 8C

```{r}
library(ISLR)
dataOJ = OJ

dataOJ

```

```{r}
OJ$MMYes_NO = ifelse(OJ$Purchase == 'MM','Yes','No')
library(caTools)
set.seed(1234)
splitOJ = sample.split(Y= OJ$MMYes_NO, SplitRatio = 0.7)
trainOJ = OJ[splitOJ,]
testOJ = OJ[!splitOJ,]

```

```{r}
#Question 1 - Using the same variables employed in the classification tree model, let us construct a bag model. Use 1000 trees. Remember, we are running a bag model here, so one has to set a value for mtry. (Hint: Think of the difference between a bag model and a random forest model.) Set seed to 100 just before the step where you run the bag model. In order to get an AUC, we need to get the prediction probability for each prediction. For this purpose, in the predict function, use argument type = "prob" and use the second column of the output.

#What is the auc for the test sample? Round your answer to two decimal places. If you are not sure how to round, use the round function in R: round(x,2)
#Answer - 0.87 
library(ipred)
library(randomForest)
set.seed(100)
bag = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH , 
                   trainOJ, 
                   mtry = 10,
                   ntree = 1000)

pred_bag = predict(bag,newdata=testOJ , type='prob')[,2]

library(ROCR)
ROCRPredbag = prediction(predictions = pred_bag, labels = testOJ$Purchase)
aucbag = as.numeric(performance(prediction.obj = ROCRPredbag,measure = 'auc')@y.values); round(aucbag,2)



```
```{r}
#Question 2 - Next, let us construct a random forest model. Use 1000 trees. Do not set mtry as we will use the default. Set seed to 100 just before the step where you run the forest model. As in the case of the bag model, for the predict function, use argument type = "prob" and use the second column of the output.

#What is the auc for the test sample? Round your answer to two decimal places. If you are not sure how to round, use the round function in R: round(x,2)
#Answer - 0.88

library(randomForest)
set.seed(100)
forest = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH , 
                      trainOJ, 
                      ntree = 1000)

pred_forest = predict(forest,newdata=testOJ , type='prob')[,2]

library(ROCR)
ROCRPredforest = prediction(predictions = pred_forest, labels = testOJ$Purchase)
aucforest = as.numeric(performance(prediction.obj = ROCRPredforest,measure = 'auc')@y.values); round(aucforest,2)
```
```{r}
#Question 3 - In this dataset, the levels of variable Purchase are 1 and 2. But, in order to run a boosting model for a two-level classification model, the dependent variable can only take values 0 and 1. So, run the following to create a new variable Purchase2 as follows:

#For the new variable, 0 represents CH and 1 represents MM

#train$Purchase2 = as.numeric(train$Purchase)-1

#test$Purchase2 = as.numeric(test$Purchase)-1

#Use this new variable Purchase2 and not Purchase. Run a gradient boosting model (gbm) with 1000 trees using Purchase2. Set distribution to "bernoulli", interaction depth to 1 and shrinkage parameter to 0.04. Set seed to 100 just before the step where you run the gradient boosting model. Use the same independent variables used in above models.

#In the predict function, use argument type ="response" and set n.trees = 100. Unlike the randomForest package you do not need to request the second column. Probabilities generated are the chance of purchasing the juice labeled as 1 which is Minute Maid.

#What is the auc for this model on the test sample? Round your answer to two decimal places. If you are not sure how to round, use the round function in R: round(x,2)
#Answer - 0.89

trainOJ$Purchase2 = as.numeric(trainOJ$Purchase)-1

testOJ$Purchase2 = as.numeric(testOJ$Purchase)-1

library(gbm)
set.seed(100)
boost = gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
            data=trainOJ,
            distribution="bernoulli",
            n.trees = 1000,
            interaction.depth = 1,
            shrinkage = 0.04)

pred_boost = predict(boost,newdata=testOJ , type='response')


library(ROCR)
ROCRPredboost = prediction(predictions = pred_boost, labels = testOJ$Purchase)
aucboost = as.numeric(performance(prediction.obj = ROCRPredboost,measure = 'auc')@y.values); round(aucboost,2)



```


