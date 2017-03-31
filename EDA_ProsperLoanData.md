Prosper Loan Data Exploratory Data Analysis
================

> Prosper is a **marketplace lending platform**, with over $8 billion in funded loans. Prosper allows people to invest in each other in a way that is financially and socially rewarding. On Prosper, borrowers list loan requests between $2,000 and $35,000 and individual investors invest as little as $25 in each loan listing they select. Prosper handles the servicing of the loan on behalf of the matched borrowers and investors. In the next few sections, we will clean up the data and do exploratory data analysis on the loan data using univariate, bivariate and multivariate graphs and summaries. Analysis section highlights the interesting reflections from the plots section. In Final Plots and Summary section, we will identify top three charts and provide final reflections regarding the dataset.

Data Wrangling
--------------

> The dataset that we are investigating has data from loans borrowed from 2005 to 2014. There are variables that are related to loan history, credit history, occupation, income range etc. of the borrower. Few of the columns in the dataset are printed below:

    ## [1] "This data table stored as 'dt' has 113937 rows and 81 columns"

    ##  [1] "ListingKey"                "ListingNumber"            
    ##  [3] "ListingCreationDate"       "CreditGrade"              
    ##  [5] "Term"                      "LoanStatus"               
    ##  [7] "ClosedDate"                "BorrowerAPR"              
    ##  [9] "BorrowerRate"              "LenderYield"              
    ## [11] "EstimatedEffectiveYield"   "EstimatedLoss"            
    ## [13] "EstimatedReturn"           "ProsperRating..numeric."  
    ## [15] "ProsperRating..Alpha."     "ProsperScore"             
    ## [17] "ListingCategory..numeric." "BorrowerState"            
    ## [19] "Occupation"                "EmploymentStatus"

> Next, since we have 81 variables, and some of the cells might have missing data, I will drop the columns that have more than 80% NA's.

``` r
dt <- dt[ ,colSums(is.na(dt)) < nrow(dt)*0.80]
```

> Some of the variable names are longer and we will shorten them next. I will only update the ones that will be used for the analysis and leave the others as is.

``` r
setnames(dt, old = c('ProsperRating..Alpha.', 'ListingCategory..numeric.'), 
         new = c('ProsperRating', 'ListingCategory'))
```

> Now that the data is cleaner, let us start plotting.

Univariate Plots Section
------------------------

> In this section, we will be using summary tables and univariate plots to draw conclusions based on our data.

**Characteristics of Loan:**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-4-1.png)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1000    4000    6500    8337   12000   35000

> The Borrowed Amount ranges from $1000 to $35000. Most loans borrowed were of amount $4000, with median at $6500 and mean of $8337.

![](EDA_ProsperLoanData_files/figure-markdown_github/Univariate_Plots-1.png)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.1340  0.1840  0.1928  0.2500  0.4975

> The mean borrowing rate was **0.192** with median at 0.184 on loan amounts

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-7-1.png)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0     0.0     0.0   152.8     0.0  2704.0

> Most users were good on payments with **zero** median delinquent days. However, 9 borrowers have over **7 years** of delinquency (2555 days).

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-9-1.png)

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-10-1.png)

> Years 2005 and 2014 might have incomplete data so we have exluded them. The number of loans had a uptrend from 2006-2008, it dipped in 2009 and then increased till 2013 year. The number of loans show a cyclical pattern.

``` r
table(dt$Term)
```

    ## 
    ##    12    36    60 
    ##  1614 87778 24545

> There were three loan terms as per the data provided- Typical loan terms were **36 months** (87778 loans). Second most popular was 60 months or 5 year term (24545 loans) and the least popular was 12 month term (1614 loans).

**Characteristics of Borrower:**

> Next, let us investigate the characteristics of the borrowers in our dataset using summary tables and barplots. The first variable we will be looking into is credit score. We are provided two columns- CreditScoreRangeLower and CreditScoreRangeUpper, following is the plot for Upper range of the Credit scores

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
summary(dt$CreditScoreRangeLower)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0   660.0   680.0   685.6   720.0   880.0     591

``` r
summary(dt$CreditScoreRangeUpper)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    19.0   679.0   699.0   704.6   739.0   899.0     591

> Median Credit score range of a borrower is **680.0 to 699.0** and Mean ranges from 685.6 to 704.6. The **max score is 880 to 899** and there are a few borrowers (133 to be exact) with very low credit score (0-19). We will revisit this later and drop the borrowers with credit score less than 400

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-14-1.png)

> Prosper Ratings Range from AA to HR (best to worst) with most borrowers with prosper rating C, There are many borrowers(~30,000) which no prosper rating. We will revisit this later and drop the data with missing prosper ratings

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-15-1.png)

    ##   Not employed             $0      $1-24,999 $25,000-49,999 $50,000-74,999 
    ##            806            621           7274          32192          31050 
    ## $75,000-99,999      $100,000+           NA's 
    ##          16916          17337           7741

> Most Borrowers are **employed** (count = 67322), 26355 have full time jobs, and 6134 are self employed. Their income ranges mostly from **$25,000 to $49,999 per annum**, 17337 borrowers earning over $100,000 and over 1600 unemployed/retired/$0 income borrowers. However, there are 8669 borrowers with their **non verifiable income**. Ignoring these, the top 3 borrowers are still in income range: $25,000-$49,999 followed by $50,000-$74,999 and $100,000+.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-17-1.png)

> The **top 7 Categories for Occupation** are 'Other', 'Professional', 'Computer Programmer', 'Executive', 'Teacher', 'Administrative Assistant' and 'Analyst'.

**Where was the loan borrowed?**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-18-1.png)

> Since displaying meaningful data from all 50 states is difficult, we picked only **Top 5 states** which had the most number of loans. The distribution is uniform across the top 5 states. Note the peaks at 10K, 15K, 20K etc. Still wondering why the mode is $4K which is not a multiple of 5K. Let us investigate this anomaly in next section.

**What was the loan borrowed for?**

> Following Listing Categories were provided in the dataset as the purpose of the loan- 0 - Not Available, 1 - Debt Consolidation, 2 - Home Improvement, 3 - Business, 4 - Personal Loan, 5 - Student Use, 6 - Auto, 7- Other, 8 - Baby&Adoption, 9 - Boat, 10 - Cosmetic Procedure, 11 - Engagement Ring, 12 - Green Loans, 13 - Household Expenses, 14 - Large Purchases, 15 - Medical/Dental, 16 - Motorcycle, 17 - RV, 18 - Taxes, 19 - Vacation, 20 - Wedding Loans

> Let us explore **top 10** reasons for borrowing the loan

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-19-1.png)

> Top reason for borrowing was debt consolidation. Other reasons were home improvement, business and auto in that order

Univariate Analysis
-------------------

> Based on the above charts, some of the questions that can be answered are as follows:

#### What is the structure of your dataset?

> The dataset had 113937 rows and 81 columns, some of the columns with more than 80% missing values were dropped to resulting in 73 columns

#### What is/are the main feature(s) of interest in your dataset?

> So far, prosper rating and borrower rates seem to be intersting.

#### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

> Investors and their yield, borrowers and their income would help understand their relationships better.

#### Did you create any new variables from existing variables in the dataset?

> Some placeholder variables for dplyr function were created. for histograms, we haven't created additional features by combining variables

#### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?

> There are a few unusual features in this dataset like there borrowers who entered $0 as their income. Also some borrowers had very low credit score(&lt;20) which did not make sense. Other than that, there were missing values. So plots were made with subsets of data to ignore those values. Also transformed the axis for few of the charts above. Outliers in the dataset although identified, haven't been removed yet. We will eliminate them in our bivariate analysis.

Bivariate Plots Section
-----------------------

**Scatterplot matrix** &gt; Before we start exploring bivariate variables in detail, we will create a scatterplot matrix for the numerical factors

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-20-1.png)

> Here, I showed ggpair plot for few of the numerical factors and some of the variables show strong correlations like Borrrower Rate, Lender Yield and Borrowed Amount.

**Removing Outliers and Unusual Data**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-21-1.png)

> age\_with\_months column is the age of the loan including months. Colored by ProsperRatings we see that the ratings did not exist before July 2009 as they appear as missing data. Before that, there was CreditGrade rating system. Since we have more data for ProsperRating vs CreditGrade, rows with CreditGrade rating system will be excluded from our analysis going further. We will also remove borrowers that had income range as not displayed or equal to 0, have borrower rate &gt;0.35, Employment Status as not displayed or missing or have credit score lower than 400

``` r
dt <- subset(dt, !(ProsperRatings == ""))
dt <- subset(dt, !(IncomeRanges %in% c("Not displayed", "", "$0", "Not employed")))
dt <- subset(dt, CreditScoreRangeUpper > 400)
dt <- subset(dt, BorrowerRate < 0.35)
dt <- subset(dt, !(EmploymentStatus %in% c("" , "Not available")))
```

**What Determines Borrower Rate?**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-23-1.png)

> From the above plots,
> + Borrower Rate histogram shows very nice stirations when colored by Prosper Rating. The borrower rate increases with decreasing prosper rating, we see two peaks for counts- first one around 0.15 (A and B rating borrowers) and second one around 0.3 (E and HR rating borrowers).
> + When colored by Income range, the plot looks pretty uniform and doesn't show much correlation. However, the Unemployed borrowers mostly borrowed at rate = 0.3.
> + The third plot shows most of the borrowers were employed.
> + Also, owning a home did not increase or decrease the borrowing rate for the given dataset

**The 4K Mystery**

![](EDA_ProsperLoanData_files/figure-markdown_github/Bivariate_Plots-1.png)

> Based on this boxplot, Lower loan amounts were borrowed by HR (High Risk), E, D rating borrowers and higher amounts were borrowed by borrowers with high prosper rating. Looks like most of the 4K loans were borrowed by E and HR rated borrowers. Tight distribution of E and HR also tells me that due to their rating, they are unable to borrow higher loan amounts

\*\* More about Prosper Rating\*\*

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-24-1.png)

> There is a relationship seen to credit score, high credit score meant high prospect ratings in general, but there are a few high credit score ouliers seen for low prospect ratings

**Debt To Income Ratio**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-25-1.png)

> Debt to Income shows some relationship to Prosper ratings, for the most part, the debt to income ratio increases with decreasing rating. We should also bring in income to understand the complete picture which we will do next

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-26-1.png)

> Debt to income ratio shows decreasing trend with increasing income range. We will revisit this relationship in multivariate section

**Borrowing and Lending**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-27-1.png)

> The Borrower Rate shows linear relationship to Prosper Rating, Borrower Rate decreases with improving Rating. Let us explore another variable: Lender Yield and determine its relationship to Prosper Rating.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-28-1.png)

> There is practically no visual difference in the plots for Lender Yield and Borrower Rate. Lender Yield also shows linear relationship to Prosper Rating

**Talking about Linear Relationships!**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-29-1.png)

> Lender Yield is strongly correlated to Borrower Rate with *r*<sup>2</sup> = 0.9984, As the borrower rate increases, the LenderYield also increases but high borrower rate comes with higher risk as well as most of the borrowers with high rate have low credit ratings.

Bivariate Analysis
------------------

> Here, we summarize what was found in the bivariate explorations and try to answer following questions

#### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?

> My feature of interest was borrower rate and it showed strong relationship to Prosper Rating. Borrower rate increased linearly with decreasing rating.

#### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?

> Lender Yield also had a very strong correlation to borrower rate with Lender Yield increasing linearly with increase in borrower rate. Also intersting was debt to ratio and its relationship to incoem range

#### What was the strongest relationship you found?

> Lender Yield had the strongest relationship to the Borrower Rate. Prosper Rating affected most of the responses like borrower rate, debt to income, lender yield etc

Multivariate Plots Section
--------------------------

> Continuing where we left off in the bivariate section, we will try to understand the risk in investing borrowers with high borrower rate.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-30-1.png)

> At the same borrower rate(for eg, at 0.2), estimated yield is in negative for HR and positive for B and A borrowers. It appears that the estimated yield is calculated purely based on rating and borrower rate

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-31-1.png)

> Estimated loss has a very weak relationship to Loan Amount for all except E and HR (notice flat lines). For E and HR, Loss decreases slighly over loan amount of $10,000, which makes sense as not may can't borrow that amount with those prosper ratings. With increasing Borrower Rate, Estimated loss also increased as high borrower rate means low prosper rating.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-32-1.png)

> Most Investors prefer to invest in low risk, low borrower rate loans. It is also seen that more investors invest in high loan amounts or higher loan amounts need more investors to fund them.

**Burried in debt?**

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-33-1.png)

> Connecting mean line shows a linear decreasing trend with increasing income range as expected. 95% of the borrowers had DebtToIncomeRatio less than 0.32, however, it ranges upto ~10 for the ones with income less than $25K. The plot also shows an interesting trend with ProsperRatings. Debt to Income Ratio increases for decreasing rating. The number of outliers also increase with decreasing ratings. It would be interesting to see the trend for the monthly payments for the borrowers, so we will do that next.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-34-1.png)

> Borrowers having high Monthly Income enabled them to borrow higher amounts and hence monthly payments increases. The relationship with Borrowed Amount and monthly loan payment is exponential and low rated borrowers, the borrower rate id high and hence high monthly loan payment.

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-35-1.png)

> We added a new column "TakeHomeIncome" which is the difference between monthly income and loan payment for a borrower and plotted it against DebtToIncomeRatio for all Prosper Ratings. Debt to Income Ratio increases with decreasing take home income. The borrowers with high Debt to Income Ratio and low take home income had low prosper ratings.

**Building a Model** &gt; Now we will use the variables with strongest correlations to build our linear model for estimated effective yield. Some of the variables I can think of would be credit score range, delinquencies, inquiries and debt to income ratio.

    ## 
    ## Call:
    ## lm(formula = EstimatedEffectiveYield ~ ProsperRating..numeric. + 
    ##     EstimatedReturn + TradesOpenedLast6Months + LoanCurrentDaysDelinquent + 
    ##     LoanMonthsSinceOrigination + LoanOriginalAmount + MonthlyLoanPayment + 
    ##     LP_CustomerPrincipalPayments + LP_ServiceFees + LP_GrossPrincipalLoss + 
    ##     PercentFunded + BorrowerAPR + CreditScoreRangeLower + CurrentCreditLines, 
    ##     data = dt)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.183817 -0.006526 -0.000183  0.009352  0.072746 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)                   6.632e-02  4.088e-03   16.223   <2e-16 ***
    ## ProsperRating..numeric.      -7.585e-03  2.155e-04  -35.197   <2e-16 ***
    ## EstimatedReturn               7.445e-01  6.019e-03  123.693   <2e-16 ***
    ## TradesOpenedLast6Months       7.709e-04  7.439e-05   10.363   <2e-16 ***
    ## LoanCurrentDaysDelinquent    -5.733e-06  6.701e-07   -8.556   <2e-16 ***
    ## LoanMonthsSinceOrigination   -1.334e-03  7.890e-06 -169.010   <2e-16 ***
    ## LoanOriginalAmount           -9.939e-07  3.473e-08  -28.621   <2e-16 ***
    ## MonthlyLoanPayment            1.087e-06  1.064e-06    1.021    0.307    
    ## LP_CustomerPrincipalPayments  1.016e-06  2.677e-08   37.961   <2e-16 ***
    ## LP_ServiceFees               -8.560e-05  1.746e-06  -49.013   <2e-16 ***
    ## LP_GrossPrincipalLoss         1.798e-06  5.343e-08   33.646   <2e-16 ***
    ## PercentFunded                -6.903e-02  3.428e-03  -20.135   <2e-16 ***
    ## BorrowerAPR                   4.656e-01  5.805e-03   80.216   <2e-16 ***
    ## CreditScoreRangeLower         7.061e-05  1.887e-06   37.426   <2e-16 ***
    ## CurrentCreditLines           -1.514e-04  1.406e-05  -10.772   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02037 on 83348 degrees of freedom
    ## Multiple R-squared:  0.9108, Adjusted R-squared:  0.9108 
    ## F-statistic: 6.082e+04 on 14 and 83348 DF,  p-value: < 2.2e-16

Multivariate Analysis
---------------------

#### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

> We understood the risk and estimated losses for investing in a borrower with certain rating. We also understood relationship between debt, income and monthly loan payments for our borrowers.

#### Were there any interesting or surprising interactions between features?

> Most Investors prefer to invest in low risk, low borrower rate loans despite the high lender yield for loans taken on higher borrower rate.

#### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.

> Yes, The model I created has Adjusted R2 of 0.9108. Strengths of the model is that it includes most of the contributing numerical factors affecting yield, the limitation is the assumption that all the factors are linear

Final Plots and Summary
-----------------------

### Plot One

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-37-1.png)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   600.0   660.0   700.0   699.8   720.0   880.0

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   619.0   679.0   719.0   718.8   739.0   899.0

### Description One

> Median Credit score range of a borrower is **680.0 to 699.0** and Mean ranges from 685.6 to 704.6. The **max score is 880 to 899** and there are 133 borrowers with very low credit score (0-19).

### Plot Two

![](EDA_ProsperLoanData_files/figure-markdown_github/unnamed-chunk-38-1.png)

### Description Two

> Most Investors prefer to invest in low risk, low borrower rate loans. It is also seen that more investors invest in high loan amounts or higher loan amounts need more investors to fund them.

### Plot Three

![](EDA_ProsperLoanData_files/figure-markdown_github/Plot_Three-1.png)

### Description Three

> Borrowers having high Monthly Income enabled them to borrow higher amounts and hence monthly payments increases. The relationship with Borrowed Amount and monthly loan payment is exponential and low rated borrowers, the borrower rate id high and hence high monthly loan payment.

------------------------------------------------------------------------

Reflection
----------

> We used univariate, bivariate and multivariate analysis for understanding Prosper Loan Data. The data set consisted of 113937 rows and 81 columns, we deleted a few columns and calculated and added a few of our own. The dataset consisted of both numerical and categorical variables and we used histogram and bar graphs for univariate, scatterplots and boxplots for bivariate analysis respectively and reflected upon interesting as welll as unusual features in the data. Borrower and Lender Yield had normalized frequency distribution and showed very strong correlation to each other. Other than that, most of the columns like borrower rate, lender yield, investors, debt to income ratio, estimated yield etc showed correlation to prosper ratings. Prosper Ratings are Ratings(AA-best to HR-worst) given to borrowers based on their loan and credit history and various other factors. Untill July 2009, Prosper used Credit Grade which wasn't very robust and then switched to Prosper Ratings. Good rating borrowers tend to have good cedit scores, less delinquencies, good income range and could borrow bigger loan amounts and were safer bet, so most of the investors invested in them. 95% of the borrowers had DebtToIncomeRatio less than 0.32(Range: 0 to 10), however, it ranges upto ~10 for the ones with income less than $25K. Debt to Income Ratio increased for decreasing Prosper rating. We constructed a linear model for predicting Estimated yield with accuracy of 0.8863. My major challenges while working with the dataset was number of columns to analyse. There were 81 different variables to understand. For future analysis, I would like to use other prediction models to predict variables like Prosper Rating or Returns on investments.

References:
-----------

<http://t-redactyl.io/blog/2016/05/creating-plots-in-r-using-ggplot2-part-11-linear-regression-plots.html>

<https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf>

<https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs>

<http://r4ds.had.co.nz/>
