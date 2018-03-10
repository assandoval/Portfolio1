---
title: "Difference of Income Among Sex and Race"
author: "Alex Sandoval"
date: "3/5/2018"
output:
  pdf_document:
    toc: yes
    toc_depth: '3'
  html_document:
    fig_height: 10
    fig_width: 10
    highlight: tango
    toc: yes
    toc_depth: 3
---

### **Introduction**
**Is there a significant difference in income between men and women? Does the difference vary depending on other factors (e.g educaiton, marital status, criminal history, drug use, childhood household factors, profession, etc.)?**

To answer this question, I looked at data from the National Longitudinal Survey of Youth, 1979 cohort. I began by cleaning the data and followed by creating tables and bar charts to explore the data. 

### **Data Summary**
```{r install.packages}
#Install packages


library(kableExtra)
library(plyr)
library(ggplot2)
library(knitr)
library(MASS)
options(scipen=4)

#Retrieve data
nlsy <- read.csv("http://www.andrew.cmu.edu/user/achoulde/94842/final_project/nlsy79/nlsy79_income.csv", header=TRUE)
```

```{r subset variables}
#Subset variables
nlsy.var <- c("R0000700", "R0001200", "R0214700", "R0214800", "R0217502", "T3977400", "R3401501")

#Recode variables
nlsy.fp <- nlsy[nlsy.var]
colnames(nlsy.fp) <- c("country.birth", "foreign.lang.spoken", "race", "sex", "fam.size", "income", "highest.grade")

str(nlsy.fp)
```

```{r convert variables to factors and recode}
#Convert variables to factors and recode 
nlsy.fp <- mutate(nlsy.fp, 
          country.birth = as.factor(mapvalues(country.birth, 
                                     c(1, 2, -3), 
                                     c("In the US", "In other country", NA))),
          foreign.lang.spoken = as.factor(mapvalues(foreign.lang.spoken, 
                                    c(1, 2, 3, 4,-4, -3, -2), 
                                    c("Spanish", "French", "German", "Other", "No Foreign Language", "No Foreign Language", "No Foreign Language"))),
          race = as.factor(mapvalues(race, 
                                    c(1, 2, 3), 
                                    c("Hispanic", "Black", "Non-Black, Non-Hispanic"))),
          sex = as.factor(mapvalues(sex,
                                    c(1, 2), 
                                    c("Male", "Female"))), 
          highest.grade = as.factor(mapvalues(highest.grade,
                                    c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 95, -3, -5), 
                                    c("None", "1st Grade", "2nd Grade", "3rd Grade", "4th Grade", "5th Grade", "6th Grade","7th Grade", "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade", "1st Year College", "2nd Year College", "3rd Year College", "4th Year College", "5th Year College", "6th Year College", "7th Year College", "8th Year College or More", NA, NA, NA))))


#Reorder grades variable
nlsy.fp$highest.grade <- factor(nlsy.fp$highest.grade , levels = c("None", "1st Grade", "2nd Grade", "3rd Grade", "4th Grade", "5th Grade", "6th Grade","7th Grade", "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade", "1st Year College", "2nd Year College", "3rd Year College", "4th Year College", "5th Year College", "6th Year College", "7th Year College", "8th Year College or More", NA, NA, NA))

#Remove non-positives
nlsy.fp$income[nlsy.fp$income < 0] <- NA

#Remove topcoded variables
nlsy.fp$income[nlsy.fp$income == 343830] <- NA

#Remove NAs in variables
nlsy.fp <- subset(nlsy.fp, !is.na(country.birth), select=country.birth:highest.grade) #Country of birth
nlsy.fp <- subset(nlsy.fp, !is.na(highest.grade), select=country.birth:highest.grade) #Highest grade completed
#Non-positive values from the income variable were removed due to invalid responses to the survey. The top 2 percent of the income variable were also removed to keep the data to a manageable size. The top 2 percent were so far removed from the rest of the data that its presence distorted graphs and charts. 

```

```{r clean summary}
#Summary of the clean data
summary(nlsy.fp)
```

```{r table average income by sex, results='asis'}
##average income by sex
in.sex<- ddply(nlsy.fp, ~ sex, summarize, 
               income.sex = mean(income, na.rm = TRUE))
in.sex
```

From an initial view of average income by sex, Males earned `r round(in.sex[2,2], 2)` and Females earned `r round(in.sex[1,2],2)` with a difference of `r round(in.sex[2,2] - in.sex[1,2], 2)` dollars. 

```{r table by gender and race, results='asis'}
#Table of the number of respondents broken down by gender and race
simpl.tbl <- addmargins(with(nlsy.fp, table(as.array(sex), as.array(race))))
kable(simpl.tbl, format =  "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```
I also wanted to analyze if a difference of income existed among races. But first, I wanted to know how many respondents were within each category. From the `r nrow(nlsy.fp)` respondents of the survey, there were `r sum(nlsy.fp[["sex"]] == "Male")` Male respondents and `r sum(nlsy.fp[["sex"]] == "Female")` Female respondents. Among the Males there were: `r sum(nlsy.fp[["race"]] == "Black" & nlsy.fp[["sex"]] == "Male")` Black respondents, `r sum(nlsy.fp[["race"]] == "Hispanic" & nlsy.fp[["sex"]] == "Male")` Hispanic respondents, and `r sum(nlsy.fp[["race"]] == "Non-Black, Non-Hispanic" & nlsy.fp[["sex"]] == "Male")` Non-Black, Non-Hispanic respondents. Among the Females there were: `r sum(nlsy.fp[["race"]] == "Black" & nlsy.fp[["sex"]] == "Female")` Black respondents, `r sum(nlsy.fp[["race"]] == "Hispanic" & nlsy.fp[["sex"]] == "Female")` Hispanic respondents and `r sum(nlsy.fp[["race"]] == "Non-Black, Non-Hispanic" & nlsy.fp[["sex"]] == "Female")` Non-Black, Non-Hispanic respondents. 


```{r income by race, results='asis'}
##Average income by race
in.race<- ddply(nlsy.fp, ~ race, summarize, 
                income.race = mean(income, na.rm= TRUE))
kable(in.race, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#Create a bar chart
ggplot(data = in.race, 
       aes(x = race, 
           y = income.race)) + 
      geom_bar(stat = "identity") +
      xlab("") +
      ylab("Average Income") +
      ggtitle("Average Income by Race")
```
After understanding how the data is broken up by gender, a table of average income by race was  calculated. Non-Black, Non-Hispanics, on average, earned the most with  `r round(in.race[3,2],2)`  dollars. Hispanic respondents earned the second highest with  `r round(in.race[2,2],2)` dollars and Black respondents earned `r round(in.race[1,2],2)` dollars.

```{r table income by sex and race, results='asis'}
#Average income by sex and race table
income.race.sex<- with(nlsy.fp, round(tapply(income, INDEX = list(race, sex), FUN = mean, na.rm= TRUE)))
kable(income.race.sex, format = "html")%>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```
By breaking the data between genders, Males at each of the three races earned more than Females. Non-Black, Non-Whites Males earned the most between the 6 groups with `r income.race.sex[3,2]` dollars, compared to `r income.race.sex[3,1]` dollars for Non-Black, Non-Hispanics Females. Hispanic males earned `r income.race.sex[2,2]` dollars compared to `r income.race.sex[2,1]` dollars Hispanic Females. Black Males earned `r round(income.race.sex[1,2],2)` dollars compared to `r income.race.sex[1,1]` dollars Black Females.

```{r barchart difference by income race and sex}
#Create a dataframe
income.race.sex.dif <- ddply(nlsy.fp, ~ race, summarize, 
                  in.gap = mean(income[sex == "Male"], na.rm = TRUE) - mean(income[sex == "Female"], na.rm = TRUE))
                
#Plot data frame
income.race.sex.dif.plot <- ggplot(data = income.race.sex.dif, aes(x = race, y = in.gap, fill = race)) +
  geom_bar(stat = "identity") +
  xlab("") + 
  ylab("Difference of Income") +
  ggtitle("Difference of Income between Men and Women") + 
  guides(fill = FALSE) 

income.race.sex.dif.plot
```
To get a better understanding of how large the difference is between the races, a bar chart of the *difference of average income* is plotted. The higher the bar, the larger the difference. There seems to be a larger difference in average income among Non-Black, Non-Hispanic respondents. 

```{r table with sd and sex, results='asis'}
# Create a data frame
base.df<- ddply(nlsy.fp, ~ race + sex, summarize, 
             mean.income = mean(income, na.rm= TRUE),
             size= length(income),
             standard.deviation= sd(income, na.rm=T),
             standard.error.mean.income= round(standard.deviation/ sqrt(size),2),
             lower = mean.income - standard.deviation, 
             upper= mean.income + standard.deviation)
kable(base.df, format = "html")%>%
  kable_styling(bootstrap_options = c("striped", "hover"))

```
The standard deviation at every race also shows that there is more variability between Males than Females. For example, the standard deviation of income for Black Males is `r round(base.df[2,5], 2)` dollars compared to Black Females of `r round(base.df[1,5], 2)` dollars. This shows that among Black Males, there is greater variability to earn income than Black Females. The standard error of the mean for Females at each of the 3 races shows that there is a significant difference between genders, for example the mean standard error for Hispanic Males income is `r round(base.df[4,6], 2)` dollars compared to the mean standard error for Hispanic Females income of `r round(base.df[3,6], 2)` dollars. The smaller the number, the more representative the value is of the true population. 


### **Methodology**
The variables I chose to answer this question were: 
- *Whether the respondent was born in the United States* I wanted to use this question to help determine if being an U.S. citizen had a positive effect on income. It is easier for U.S. citizens to get jobs than non U.S citizens due to sponsorship. 

- *If a foreign language was spoken at home during resondent's childhood?* Does being knowing another language or being around a foreign language improve one's chances of earning a higher income.

- *Respondent's race/etnicity* Race is going to be an important factor on the difference of income among sexes based upon years of racial segregation and prohibiting minorities from advancing higher employment opportunities.

- *Sex of Respondent* Is there a difference of income between sexes?

- *Family Size* Does having a bigger family improve income?

- *Income* Is there a difference of income between sexes?

- *Highest Grade Completed*- Does income increase based on the number of years of education?

Within the income variable, the top 2% of highest incomes were "top coded" which means that we do not see the actual incomes for the 2% of earners. For the top 2% of earners, the income variable is the average income of the 2% of earners. After exploring the bar charts and tables, I removed the top coded variables because the values were too high that it altered the charts. They also seemed to be outliers and not an indication as to whether they would contribute to the study.

After careful analysis, I  also removed all non positives from my data. I did recode some of the negative values based on the data dictionary, such as the foreign language question to 'no foreign language spoken'. The rest of the non-positive values were small quantities that it would not have impacted the rest of the study. 


There were `r sum(nlsy.fp[["sex"]] == "Female") - sum(nlsy.fp[["sex"]] == "Male")` more Female respondents in the study but out of the `r nrow(nlsy.fp)` total respondents, the distribution seemed fine. After breaking it down by race, the data seem to be reflect the current population by having more Non Black, Non Hispanic respondents, followed by Black respondents and ending with Hispanic respondents.

The graph titled "Difference of Income between Males and Females" was interesting to see because I expected there to be a difference but not large enough like the one between Non Black, Non Hispanic Males and Females. 

I was also surprised to see that in each of the 3 races, there was a noticeable difference of income between Males and Females. I had expected that there would be very little if any difference among the Black and Hispanic race.

The next section will go into the main findings and how these variables impact the income between Males and Females. 

### **Findings**
```{r barchart income by sex and race error bars}
#plot data
base.df.plot <- ggplot(base.df, 
                    aes(x= sex, 
                        y= mean.income,
                        fill= race)
                    ) +
                    geom_col(stat = "identity", position= "dodge") +
                    geom_errorbar(aes(ymin = lower, 
                                      ymax = upper), 
                                      width = .2, position= position_dodge(0.9)) +
                    xlab("") +
                    ylab("Average Income") +
                    ggtitle("Average Income Distribution by Gender Among Race")
base.df.plot
```

The error bars show that I am 95 percent confident that the percentage of Non Black, Non Hispanic respondents are between `r round(base.df[6,7], 2)` and `r round(base.df [6,8], 2)`. 

```{r male qqplot}
# qq plot
with(nlsy.fp, qqnorm(income[sex=="Male"], main = "Normal Q-Q Plot for Males"))
# add reference line
with(nlsy.fp, qqline(income[sex=="Male"], col = "red"))
```
To assess if the observed data follows a normal distribution, a QQ plot was performed. Among Males, the data is not perfectly normal. The upward curving suggests that there is a high positive skew and the bottom suggests that there are many low values. 

```{r female qqplot}
# qq plot
with(nlsy.fp, qqnorm(income[sex=="Female"], main = "Normal Q-Q Plot for Females"))
# add reference line
with(nlsy.fp, qqline(income[sex=="Female"], col = "red"))
```

The the QQplot for Females follows the same distribution as it is not perfectly normal. The top values are more off normal than Males which also suggests that there is high positive skewness. 

<font color="#157515"><b>
Sex T.test
</font></b>
```{r ttest}
income.sex.t.test <- t.test(income ~ sex, data = nlsy.fp)
income.sex.t.test
options(scipen=4)
```
To tests the averages between two groups, a t test was done. The p-value for this t test is `r round(income.sex.t.test$p.value, 4)` and the t statistic is `r round(income.sex.t.test$statistic, 2)`, which suggests that the null hypothesis of there being no difference among income by gender is rejected. This t- test suggests that  Males do have higher income. The confidence intervals are `r round(income.sex.t.test$conf.int[1], 2)` and `r round(income.sex.t.test$conf.int[2], 2)` which suggests that if this sample data was represented of the larger population, there is a 95% confidence that the gender difference of income is between those values. Next, the mean income for Females is `r round(income.sex.t.test$estimate[1], 2)` and the mean income for Males is `r round(income.sex.t.test$estimate[2], 2)`.

The next three t tests take a look based on race:

<font color="#157515"><b>
Non-Black, Non-Hispanic t.test
</font></b>
```{r ttest white}
income.sex.t.test2 <- t.test(income ~ sex, data = subset(nlsy.fp, race == "Non-Black, Non-Hispanic"))
income.sex.t.test2
```
Among the Non-Black, Non-Hispanic race, the p-value for the t-test is `r round(income.sex.t.test2$p.value, 4)` and the t statistic is `r round(income.sex.t.test2$statistic, 2)`, which suggests that the null hypothesis of there being no difference among income by gender is rejected. This t-test suggests that Non-Black, Non-Hispanic Males do have higher income. The confidence intervals are `r round(income.sex.t.test2$conf.int[1], 2)` and `r round(income.sex.t.test2$conf.int[2], 2)` which suggests that if this sample data was represented of the larger population, there is a 95% confidence that the gender difference of income is between those values. Next, the mean income for Non-Black, Non-Hispanic Females is `r round(income.sex.t.test2$estimate[1], 2)` and the mean income for Non-Black, Non-Hispanic Males is `r round(income.sex.t.test2$estimate[2], 2)`.

<font color="#157515"><b>
Hispanics t.test
</font></b>
```{r ttest hispanic}
income.sex.t.test3 <- t.test(income ~ sex, data = subset(nlsy.fp, race == "Hispanic"))
income.sex.t.test3
```
Among the Hispanic race, the p-value for the t-test is `r round(income.sex.t.test3$p.value, 4)` and the t statistic is `r round(income.sex.t.test3$statistic, 2)`, which suggests that the null hypothesis of there being no difference among income by gender is rejected. This t-test suggests that Hispanic Males do have higher income. The confidence intervals are `r round(income.sex.t.test3$conf.int[1], 2)` and `r round(income.sex.t.test3$conf.int[2], 2)` which suggests that if this sample data was represented of the larger population, there is a 95% confidence that the gender difference of income is between those values. Next, the mean income for Hispanic Females is `r round(income.sex.t.test3$estimate[1], 2)` and the mean income for Hispanic Males is `r round(income.sex.t.test3$estimate[2], 2)`.

<font color="#157515"><b>
Blacks t.test
</font></b>
```{r ttest black}
income.sex.t.test4 <- t.test(income ~ sex, data = subset(nlsy.fp, race == "Black"))
income.sex.t.test4
```
Among the Black race, the p-value for the t-test is `r round(income.sex.t.test4$p.value, 2)` and the t statistic is `r round(income.sex.t.test4$statistic, 2)`, which suggests that the null hypothesis of there being no difference among income by gender is rejected. This t-test suggests that Black Males do have higher income. The confidence intervals are `r round(income.sex.t.test4$conf.int[1], 2)` and `r round(income.sex.t.test4$conf.int[2], 2)` which suggests that if this sample data was represented of the larger population, there is a 95% confidence that the gender difference of income is between those values. Next, the mean income for Black Females is `r round(income.sex.t.test4$estimate[1], 2)` and the mean income for Black Males is `r round(income.sex.t.test4$estimate[2], 2)`.

```{r boxplot income by race and gender}
gender.income.boxplot <- ggplot( data = nlsy.fp, 
      aes(x = sex, 
          y = income, na.rm = T, 
          fill = race)) +
      geom_boxplot() +
      xlab("") +
      ylab("Income") +
      ggtitle("Income Distribution by Gender Among Race") 

gender.income.boxplot
```
This box plot shows a better representation of where the respondents average income is separated by gender and race. The average income among Females lies relatively close to each other with Black females earning `r round(income.race.sex[1,1], 2)` dollars and Non-Black, Non-Hispanic earning the most with `r round(income.race.sex[3,1], 2)` dollars. There are larger gaps between average incomes among Males. Black males average income is the lowest with `r round(income.race.sex[1,2], 2)`  dollars and at about the same level as Hispanic Females with `r round(income.race.sex[2,1], 2)`. The average income for Non Black, Non Hispanic males is the largest at `r round(income.race.sex[3,2], 2)` dollars and is over the third quartile of Black Females. There are some outliers in each box plot and most of them are above the $150,000 range. 

```{r bar chart by income gender and country}
#create a dataframe
sex.country.income<- ddply(nlsy.fp, ~ country.birth + sex, summarize, mean.income = mean(income, na.rm= TRUE))

#subset dataframe 
sex.country.income<- subset(sex.country.income, !is.na(country.birth), select= c(country.birth, sex, mean.income))

#plot dataframe
sex.country.income.plot <- ggplot(data = sex.country.income, 
      aes(x = country.birth, 
          y = mean.income,
          fill = sex)) + 
      geom_histogram(stat = "identity", position= "dodge") +
      xlab("") +
      ylab("Average Income") +
      ggtitle("Average Income by Country of Birth Among Gender")+
      scale_fill_brewer(palette = "Spectral")

sex.country.income.plot
```
The bar graph shows that Males continue to earn more than Females, even when the data is broken up by nationality. It is also interesting to note that Females not born in the the U.S. on average, earn `r round(sex.country.income[[3,3]]- sex.country.income[[1,3]], 2)`  more than Females born in the U.S. The difference is larger for Males. On average, Males born in another country earn `r round(sex.country.income[[2,3]]-sex.country.income[[4,3]], 2)` dollars more than Males born in the U.S.

```{r barchart by income gender and language}
sex.language.income <- ddply(nlsy.fp, ~ foreign.lang.spoken + sex, summarize, mean.income = mean(income, na.rm= TRUE))

sex.language.income.plot <- ggplot(data = sex.language.income, 
      aes(x = foreign.lang.spoken, 
          y = mean.income,
          fill = sex)) + 
      geom_histogram(stat = "identity", position= "dodge") +
      xlab("Foreign Language Spoken at Home") +
      ylab("Average Income") +
      ggtitle("Average Income by Foreign Language Spoken at Home Among Gender") +
      scale_fill_brewer(palette = "Spectral")

sex.language.income.plot
```
The same trend continues when looking at the difference of income among gender broken down by foreign language spoken at home. No foreign Language spoken at home, suggesting English only speaking, is the same trend we have been noticing. The biggest difference between genders is among the 'Other' language with a difference of `r round(sex.language.income[8,3]- sex.language.income[7,3], 2)` followed by a difference of German language of `r round(sex.language.income[4,3]- sex.language.income[3,3], 2)`. 

```{r jitter by income gender and language}
ggplot(nlsy.fp, aes(x= foreign.lang.spoken, 
                    y= income, 
                    color= sex)) + 
                geom_jitter(alpha = .25) +
                ylab( "Income") +
                xlab("Foreign Language Spoken at Home") +
                ggtitle("Income by Foreign Language Spoken at Home") 
```
Using the jitter function, we can see the depth of each language. There are significantly more dots in the 'No foreign Language' spoken than any others. Spanish comes second. The differences mentioned above regarding German and Other have less values than 'No Foreign Language' and Spanish. Within this graphic, Males continue to earn more than Females.

```{r plot income by grade}
##average income by race and grade
sex.grade.income <- ddply(nlsy.fp, ~ sex + highest.grade, summarize,
                       mean.income = round(mean(income, na.rm = TRUE),digits = 2))

#Reorder Grades
sex.grade.income$highest.grade <- factor(sex.grade.income$highest.grade , levels = c("None", "1st Grade", "2nd Grade", "3rd Grade", "4th Grade", "5th Grade", "6th Grade","7th Grade", "8th Grade", "9th Grade", "10th Grade", "11th Grade", "12th Grade", "1st Year College", "2nd Year College", "3rd Year College", "4th Year College", "5th Year College", "6th Year College", "7th Year College", "8th Year College or More",  NA, NA, NA))

sex.grade.income.plot <- ggplot(data = sex.grade.income, 
      aes(x = highest.grade, 
          y = mean.income,
          fill = sex)) + 
      geom_bar(stat = "identity", position= "dodge") +
      xlab("Highest Grade Completed") +
      ylab("Average Income") +
      ggtitle("Average Income by Highest Grade Completed Among Gender") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_fill_brewer(palette = "Spectral")

sex.grade.income.plot
```
By taking a look at education, there is a correlation with average income and years of education. It appears that the more years of education one has, the more income they earn. Still, the chart shows that Males on average, earn more than Females at every year of education. Although it seems the gap shrinks when Females have 7 or more years of college education. There are two majors jumps between both genders: (1) after receiving 12 years of education (graduating with a high school diploma) and (2) after receiving 4 years of college education (graduating with a Bachelor's). The disparity between the sexes seems to widen after graduating high school. It is interesting to note there is an outlier for the 3rd grade.

```{r scatter plot grade income sex}
#Scatter plot
ggplot(nlsy.fp, aes(x=as.numeric(highest.grade), 
                    y=income, 
                    color=sex)) + 
                geom_jitter(alpha = .25) +
                geom_smooth()+
                ylab( "Income") +
                xlab("Highest Grade Completed") +
                ggtitle("Income by Highest Grade Completed") 
```

This chart shows the same information but looks at the total values among Males and Females. Years 12 and 16, high school diploma and Bachelor's degree, respectively are clearly shown. You can also see the trend increasing for both Males and Females with the curvature of the lines. 

```{r regression income grade, results='asis'}
final.regression <- lm(income ~ ., data= nlsy.fp)
options(scipen=15)
kable(summary(final.regression)$coef, digits = c(3, 3, 3, 4))

final.regression.coef <- round(summary(final.regression)$coef, 4)
class(final.regression.coef)
attributes(final.regression.coef)
```

<font color="#157515"><b>
Interpretation
</font></b>

There are several statistical predictors of income, such as being born in the U.S., race, and education. The p- value for someone born in the U.S is `r final.regression.coef["country.birthIn the US","Pr(>|t|)"]`, the p-value for being Hispanic is `r final.regression.coef["raceHispanic","Pr(>|t|)"]`, the p-value for Non-Black, Non-Hispanics with a p value of `r final.regression.coef["raceNon-Black, Non-Hispanic","Pr(>|t|)"]`, the p-value for Males is `r final.regression.coef["sexMale","Pr(>|t|)"]`, the p-vale for receiving at least 2 years of college education is `r final.regression.coef["highest.grade2nd Year College","Pr(>|t|)"]`, the p-vale for receiving at least 3 years of college education is `r final.regression.coef["highest.grade3rd Year College","Pr(>|t|)"]`, the p-vale for receiving at least 4 years of college education is `r final.regression.coef["highest.grade4th Year College","Pr(>|t|)"]`, the p-vale for receiving at least 5 years of college education is `r final.regression.coef["highest.grade5th Year College","Pr(>|t|)"]`, the p-vale for receiving at least 6 years of college education is `r final.regression.coef["highest.grade6th Year College","Pr(>|t|)"]`, the p-vale for receiving at least 7 years of college education is `r final.regression.coef["highest.grade7th Year College","Pr(>|t|)"]`, and the p-vale for receiving at 8 years of college education or more is `r final.regression.coef["highest.grade8th Year College or More","Pr(>|t|)"]`. All of the coefficients with the exception of country.birthIn the US, are positive suggesting that there is a positive relationship. 

The `baseline` for this regression is Black Females who were not born in the U.S and who were raised speaking French as a child. The interpretations are made off of this baseline. Education is a large driver for income, followed by race and country of origin. 

- `8 years or more of college education`: The coefficient is `r round(final.regression.coef["highest.grade8th Year College or More","Estimate"], 2)`, which means that respondents that have 8 years of college education or more, their income, on average, is about `r round(final.regression.coef["highest.grade8th Year College or More","Estimate"], 2)` dollars higher than the base line. 

- `7 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade7th Year College","Estimate"], 2)`, which means that respondents that have 7 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade7th Year College","Estimate"], 2)` dollars higher than the base line. 

- `6 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade6th Year College","Estimate"], 2)`, which means that respondents that have 6 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade6th Year College","Estimate"], 2)` dollars higher than the base line. 

- `5 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade5th Year College","Estimate"], 2)`, which means that respondents that have 5 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade5th Year College","Estimate"], 2)` dollars higher than the base line.

- `4 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade4th Year College","Estimate"], 2)`, which means that respondents that have 4 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade4th Year College","Estimate"], 2)` dollars higher than the base line.

- `3 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade3rd Year College","Estimate"], 2)`, which means that respondents that have 3 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade3rd Year College","Estimate"],2)` dollars higher than the base line.

- `2 years of college education`: The coefficient is `r round(final.regression.coef["highest.grade2nd Year College","Estimate"], 2)`, which means that respondents that have 2 years of college education, their income, on average, is about `r round(final.regression.coef["highest.grade2nd Year College","Estimate"],2)` dollars higher than the base line.

- `Male Sex`: The coefficient is `r round(final.regression.coef["sexMale","Estimate"], 2)`, which means that on average, Males make about `r round(final.regression.coef["sexMale","Estimate"], 2)` dollars more than the baseline. 

- `Non Black, Non Hispanic`: The coefficient is `r round(final.regression.coef["raceNon-Black, Non-Hispanic","Estimate"], 2)`, which means that Non Black, Non Hispanics on average, make about `r round(final.regression.coef["raceNon-Black, Non-Hispanic","Estimate"], 2)` dollars more than the baseline. 

- `Hispanic`: The coefficient is `r round(final.regression.coef["raceHispanic","Estimate"], 2)`, which means that Hispanics on average, make about `r round(final.regression.coef["raceHispanic","Estimate"], 2)` dollars more than the baseline. 

- `Born in the U.S.`: The coefficient is `r round(final.regression.coef["country.birthIn the US","Estimate"], 2)`, which means that those born in the U.S. on average, make about `r round(final.regression.coef["country.birthIn the US","Estimate"], 2)` dollars less than the baseline. 

```{r interaction, results='asis'}
#interaction
regression.interact <- lm(income ~ sex * race, data = nlsy.fp)
kable(summary(regression.interact)$coef, digits = c(3, 3, 3, 4))

regression.interact.coef <- round(summary(regression.interact)$coef, 4)
class(regression.interact.coef)
attributes(regression.interact.coef)
```

<font color="#157515"><b>
More Interpretation
</font></b>

To look at specific variables, an interaction on sex and race was conducted. All p-values were statistically significant. 
On average Black Males earned `r round(regression.interact.coef["sexMale","Estimate"], 2)` dollars more than Black Females.
On average Hispanic Males earned `r round(regression.interact.coef["sexMale","Estimate"] + regression.interact.coef["sexMale:raceHispanic","Estimate"], 2)` dollars more than Black Females. 
On average Non Black, Non Hispanic Males earned `r round(regression.interact.coef["sexMale","Estimate"] + regression.interact.coef["sexMale:raceNon-Black, Non-Hispanic","Estimate"], 2)` dollars more than Black Females. 

### **Discussion** 
In summary, Males do earn more than Females. The difference varies once it is broken up my race. The difference is larger for Non-Black, Non-Hispanics, followed by Hispanics, and then Blacks. The disparity still exists when the data is broken up by education. Although, the more education one has, the more income they earn, the gap in income still exists. Furthermore, when the data is broken up into country of origin, the gap is still there. Although, it seems that being born in the country has a negative effect on income compared to those being born outside of the country. Family size was not a statistically significant predictor of income.There may be potential confounders for type of industry one works in or length of one's employment, I have confidence in my conclusion, but further analysis would have to be done to investigate those confounders before presenting the findings to policymakers. 

