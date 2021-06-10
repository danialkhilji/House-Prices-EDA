#The objective of this file is to look into the insights of the house prices
#variations in relation to different factors. This data is of a Boston city in America
#and it shows different qualities of each particular house.
#Link to originall dataset: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

#This dataset was downloaded from the Kaggle website from the
#House Prices - Advanced Regression Techniques competition

#Import dataset into the current directory
houses <- read.csv('C:/Users/dania/Documents/Datasets/House Prices/train.csv')
#First look at the dataset
head(houses)
tail(houses)
dim(houses)
colnames(houses)

#Loading automatic EDA library
library(SmartEDA)

#Brief summary of the dataset
ExpData(houses, type=1)

#Total rows are 1460. Total number of features equals 81, of which 36 is quantitative, 
#43 categorical + Id and SalePrice.

#For a more clear summary
exp_data <-ExpData(houses, type=2)
library(ggplot2)
ggplot(exp_data, aes(x=Per_of_Missing, y=Variable_Name)) + 
  geom_bar(stat='identity')

#Per_of_missing column shows the most missing values in each column of dataset
#Highest value of per_of_missing are PoolQC (Pool quality), MiscFeatures (miscellaneous features),
#Alley (access to alley), Fence and FireplaceQu (Fire place quality). Most of the NA in the
#columns does not mean that data is missing, instead it means that particular feature is not 
#installed in the home. For example PoolQC is NA for homes where Pool is not installed so
#these NA's can be renamed like NP (means No Pool).

#Summary statistics of all numerical variables
num_summary <- ExpNumStat(houses)
num_summary
#This info is given for only numerical features

#Statistical summary of all categorical features with respect to target variable
ExpCatStat(houses, Target = 'SalePrice', plot = TRUE)

#Box plot for comparison with SalePrice (target)
ExpNumViz(data = houses, target = 'SalePrice')

#Some of the common relations in the above scatter plots are:
#Price is highest for MSSubClass 60 and 190 (value ascending from top in plot)
#Price increases as the OverallQual (Overall quality) increases
#Price has highest variation in OverallCond (Overall condition) = 5
#Price increases abruptly for homes built (YearBuilt) in 2008 or later
#BsmtFinSF1 and BsmtFinSF2 (Rating of basement finish area) are 0 for most of the homes
#since these homes do not have basement at all
#X1stFlrSf and X2ndSlrSF (first and second floor square feet area respectively)
#X1stFlrSf has a much clear postive correlation with Sale Price as compared to the
#X2ndtFlrSf correlation which is more scattered
#GrLivArea (Above grade (ground) living area square feet) also shows a linear
#positive correlation
#TotRmsAbvGrd: Total rooms above grade (does not include bathrooms) also shows a 
#positive linear correlation with respect to Sale Price
#Features which show different factors of Garage also displays generally a positive 
#linear relation
#Open or Enclosed Porch area in square feet does not show any correlation instead
#it is scattered in manner
#PoolArea is 0 for most of the houses therefore no right conclusion can be made
#MoSold (Month Sold) shows that price is highest in months of June and July and January

#For visualizing categorical features
ExpCatViz(houses)

#Some of the key highlights from the bar plots
#MSZoning (Identify general zoning classification of the sale) shows that
#most of the houses are in the RL (Residential Low Density) = 79% area 
#Alley (type of alley access to property) features proves that most of the 
#homes does not have an alley access
#LotShape (General shape of property) is Reg (Regular) = 63% and IR1 
#(slightly irregular) = 33%
#LandContour (Flatness of property) = 90% for flat level
#Condition1 (Proximity to various conditions) are mostly normal (86%)
#Condition2 are normal for almost all properties (99%)
#BldgType (Type of dwelling) is 84% for 1Fam (Single-family detached)
#HouseStyle (Style of dwelling) is 50% for 1Story (One story) house and 30% and 11%
#for 2Story (two story) and 1.5Fin (One and half story: second level finished) respectively
#RoofStlye is Gable = 78% houses and other comman style is Hip = 20%
#RoofMatl (Roof Material) shows that almost all of the roofs are CompShg
#(Standard composite Shingle) and rest of the few are made of Tar and Gravel
#ExterQual (Exterior material quality) is TA (Typical/Average) for 62% of the houses
#where as another big portion is of Gd (Good) quality consisting of 33%
#ExterCond (External Condition) is average (TA) for 88% of the houses
#Foundation is commonly made up of PConc (Poured concrete) = 44% or 
#CBlock (Cinder blocks) = 43%
#BsmtQual represent the height of the basement which is either TA (typical: 80-89 inches) or 
#Gd (good: 90-99 inches) in most of the houses
#HeatingQC (Heating uality and condition) is Ex (excellent) for half of the houses and 
#rest of the houses have either good or average quality and condition
#CentralAir (central air conditioning) is installed in most houses
#KitchenQaul (kitchen quality) is also average for 50% houses and 40% are in good condition
#Funtional shows the overall functionality of the house which is average for most of the homes
#GarageType either attached or detached type for most of the houses
#GarageFinish varies considerably
#Garage quality and condition is average for 90% of the houses
#PavedDrive is almost in all of the houses in Boston
#OverallQual is seen to be varying in the average rating range or it can be called average rating
#YrSold (year sold) is seen to be distributed equally among all years except in 2010 which shows very 
#low real state market

#From the above evaluation, below are most of the categories which have the data distributed
#among all types with each feature (this information will be used in the next EDA package)
#YrSold, GarageCars, Fireplaces, BedroomAbvGr, OverallCond, OverallQual, Fence,
#GarageFinish, GarageType, FireplaceQu, KitchenQual, HeatingQC, BsmtFinType1
#BsmtExposure, BsmtQual, Foundation, MasVnrType, HouseStyle, BldgType
#(These features were selected on the criteria if the distribution is among three or more
#than three types and each of the type is atleast contributing 10% to that particular feature)

#To plot univariate Quantile Quantile normalized plots 
ExpOutQQ(houses)

#To create tables of categorical features to generate frequency or cross tables
ExpCTable(houses[,1:40])
ExpCTable(houses[,41:81])
#This function gives normalized frequency of each type of each feature. This another
#way to come to the conclusions mentioned above in the categorical and numerical plots


#Loading automatic EDA library
library(explore)

#To describe the overall dataset
describe_tbl(houses)
#The first line shows that there are approx. 1500 observations and 81 features/columns
#Number of missing features are 460
#19 features are those which contain NA values if any

#There are many different ways to describe data in explore package
#This will describe categorical and continuous both variables
t(describe_all(houses))
#t() is to transpose the row matrix into column matrix so that all the info is displayed

#Only those features are selected which were selected during the categorical analysis in the last package
#To describe specific categorical variables

#To describe categorical variable. Since there are multiple categorical variable and
#the describe_cat does not take all the categorical columns at once, I tried to run
#the command in loop but the function was not taking input inside loop as well. Therefore,
#the function is run separately for each categorical variable.
#for(feature in cat_features)

describe_cat(houses, 'YrSold')
describe_cat(houses, 'GarageCars')
describe_cat(houses, 'Fireplaces')
describe_cat(houses, 'BedroomAbvGr')
describe_cat(houses, 'OverallCond')
describe_cat(houses, 'OverallQual')
describe_cat(houses, 'Fence')
describe_cat(houses, 'GarageFinish')
describe_cat(houses, 'GarageType')
describe_cat(houses, 'FireplaceQu')
describe_cat(houses, 'KitchenQual')
describe_cat(houses, 'HeatingQC')
describe_cat(houses, 'BsmtFinType1')
describe_cat(houses, 'BsmtExposure')
describe_cat(houses, 'BsmtQual')
describe_cat(houses, 'Foundation')
describe_cat(houses, 'MasVnrType')
describe_cat(houses, 'HouseStyle')
describe_cat(houses, 'BldgType')

#Explain a target variable using a decision tree
explain_tree(houses, SalePrice, out = 'model')
#The decision tree gives a structured way to reach to the dependent conclusion

#The most powerful feature of the explore package is the line below
explore(houses)

#This will open a interactive interface called Shiny. In this interface, one can
#easily select the target and variable for comparison using drop down menu.
#Report all button generates the html page showing summary, distribution scatter plot
#among other useful information. This functionality is useful when there is a need to
#explore dataset by changing the targets and variables and generating their results

#Reducing number of features to visualize better in the code below #20=yrbuilt
houses_reduced <- houses[, c(2,3,4,7,13,14,16,17,18,20,22,28,40,52,54,57,59,63,74,77,78,81)]
colnames(houses_reduced)

#for (ftr in colnames(houses_reduced)){
#  explore_bar(data = houses_reduced, var = as.name(ftr), target = SalePrice)}

#explore_bar is used to plot bar plot between target and var. Same as the describe_cat issue,
#explore_bar also does not work inside loop. The logically correct loop has been commented above.
explore_bar(data = houses_reduced, var = MSZoning, target = SalePrice)
#Majority of the houses are in residential low density (RL) zone
explore_bar(data = houses_reduced, var = Alley, target = SalePrice)
#Over 75% of all price category houses have no type of alley access to property
explore_bar(data = houses_reduced, var = Neighborhood, target = SalePrice)
#The neighborhood generally share same percentage of neighborhood but houses in NAmes have
#the highest share among all neighborhoods
explore_bar(data = houses_reduced, var = Condition1, target = SalePrice)
#Most of the houses have Normal conditions
explore_bar(data = houses_reduced, var = BldgType, target = SalePrice)
#The type of dwelling for single-family detached (1Fam) have houses above 60% in all price ranges
explore_bar(data = houses_reduced, var = HouseStyle, target = SalePrice)
#Majority of houses are either one story or two story. Above 60% of the houses in 
#price category of 110000 are one story
explore_bar(data = houses_reduced, var = OverallQual, target = SalePrice)
#Overall quality is mostly in range 5-6
explore_bar(data = houses_reduced, var = YearBuilt, target = SalePrice, max_cat = 115)
#There is very little variation in the number of houses built in particulr year
explore_bar(data = houses_reduced, var = RoofStyle, target = SalePrice)
#Only few expensive houses had Mansard or Shed roof style. While rest of the houses had 
#either Gable or Hip style where Gable style being the most famous among all.
explore_bar(data = houses_reduced, var = ExterQual, target = SalePrice)
#Exterior quality is average/typical for above 50% of houses of each price category while
#good quality is at the second
explore_bar(data = houses_reduced, var = Heating, target = SalePrice)
#Roughly all of the houses had GasA (Gas forced warm air furnace) heating systems
explore_bar(data = houses_reduced, var = BedroomAbvGr, target = SalePrice)
#Most of the bedrooms above ground are 3. Houses with 145000 and 155000 price category
#were most of those houses which had 3 rooms. Another unique finding is that most of the 
#houses with 5 bedrooms above ground are in 135000 price category which is second cheapest
#price category
explore_bar(data = houses_reduced, var = KitchenQual, target = SalePrice)
#Kitchen quality is typical for most of the houses
explore_bar(data = houses_reduced, var = Fireplaces, target = SalePrice)
#Majority of houses had no fireplace and houses with fireplaces had at most one fireplace
explore_bar(data = houses_reduced, var = GarageType, target = SalePrice)
#Most of the houses either had attached garage or detached garage and houses with 
#190000 price category had the highest number of attached garages
explore_bar(data = houses_reduced, var = GarageArea, target = SalePrice, max_cat = 280)
#Garage area vary from 150 Square feet to 600 square feet roughly.
#Houses sold in the 130000 price category were mostly the houses that had same 
#garage area of approximately 370 square feet
explore_bar(data = houses_reduced, var = Fence, target = SalePrice)
#Above 60% of the houses sold in each price category had no fence
explore_bar(data = houses_reduced, var = MoSold, target = SalePrice)
#The highest number of houses are sold in the summer time. Specifically in June.
#And lrgest number of houses sold were priced 155000$ 
explore_bar(data = houses_reduced, var = YrSold, target = SalePrice)
#Maximum number of houses ever sold in the range were in 2009 of price 110000$
#whereas it seems to be the least sold price houses in other years. The major reason
#might be the 2008 financial crisis who's effect was felt in 2009 where highest number houses
#were the cheapest category houses.

#From the above conclusions we can see the correlations of two variables against target
#Such as between: GarageType and Fence, HouseStyle and RoofStyle
explore_cor(houses_reduced, GarageType, GarageArea, target = SalePrice, 
            title = 'Relation of Garage Type and Garage Area with respect to Sale Price', color = 'Yellow')
explore_cor(houses_reduced, HouseStyle, RoofStyle, target = SalePrice)