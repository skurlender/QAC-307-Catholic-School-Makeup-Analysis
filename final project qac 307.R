school_data <- read.csv("~/Documents/QAC 307/cschool_pud.csv")

library(ggplot2)
library(gmodels)

names(school_data)[names(school_data) == "c5r2mtsc"] <- "MathScore"
names(school_data)[names(school_data) == "c5r2mtsc_std"] <- "MathScoreStd"
names(school_data)[names(school_data) == "w3income"] <- "FamilyIncome"
names(school_data)[names(school_data) == "p5hmage"] <- "MotherAge"
names(school_data)[names(school_data) == "p5hdage"] <- "FatherAge"
names(school_data)[names(school_data) == "w3momed"] <- "MotherHighestEd"
names(school_data)[names(school_data) == "w3daded"] <- "FatherHighestEd"
names(school_data)[names(school_data) == "w3momscr"] <- "MotherOPS"
names(school_data)[names(school_data) == "w3dadscr"] <- "FatherOPS"
names(school_data)[names(school_data) == "p5fstamp"] <- "FoodStampStatus"

ggplot(data = school_data) + 
  stat_summary(aes(x = catholic, y = MathScore),
               fun = "mean", geom = "bar", size = 2)+
  stat_summary(aes(x = catholic, y = MathScore),
               fun = "mean", geom = "bar", size = 3)+
  scale_color_manual(values = c("orchid4", "dodgerblue"))

model1 <- aov(MathScore~catholic, data = school_data)
summary(model1)
# p-value = 4.75e-16
plot(model1)

model2 <- aov(MathScore~catholic+FamilyIncome+p5numpla+MotherHighestEd+FatherHighestEd+catholic*FamilyIncome, data = school_data)
summary(model2)
# after controlling for all these factors, catholic is still significant and actually all are significant
# we also can see that there exists some interaction effect with catholicism and family income 

plot(model2)

tapply(school_data$MathScore, school_data$catholic, FUN = mean)
# those who attended a Catholic school had a slightly higher average math score


tapply(school_data$MathScore, school_data$w3inccat, FUN = mean)
tapply(school_data$MathScore, school_data$w3inccat, FUN = mean) - mean(school_data$MathScore)
# the lowest average score was a family income of $5000 or below, followed by $5,001 to $10,000,
# $10,001 to $15,000, and 15,001 to $20,000

# the highest average score was a family income of $200,000 or above, followed by $100,001 to $200,000, 
# $75,001 to $100,000, and $50,001 to $75,000

# overall, as income group increased, average score increased every time

tapply(school_data$MathScore, school_data$p5numpla, FUN = mean)
tapply(school_data$MathScore, school_data$p5numpla, FUN = mean) - mean(school_data$MathScore) #treatment effects

tapply(school_data$MathScore, school_data$MotherHighestEd, FUN = mean)
tapply(school_data$MathScore, school_data$MotherHighestEd, FUN = mean) - mean(school_data$MathScore)

tapply(school_data$MathScore, school_data$FatherHighestEd, FUN = mean)
tapply(school_data$MathScore, school_data$FatherHighestEd, FUN = mean) - mean(school_data$MathScore)

library(tableone)
library(Matching)

# treat - catholic, resp - mathscore

CatholicYes <- as.numeric(school_data$catholic == 1)
CatholicNo <- as.numeric(school_data$catholic == 0)

Treatment <- as.numeric(school_data$catholic == 1)
Response <- school_data$MathScore

# we already showed that going to catholic school is realted to mathscore

INC <- school_data$FamilyIncome
MathScore <- school_data$MathScore
WhiteYes <- as.numeric(school_data$race_white == 1)
BlackYes <- as.numeric(school_data$race_black == 1)
HispanicYes <- as.numeric(school_data$race_hispanic == 1)
AsianYes <- as.numeric(school_data$race_asian == 1)

mydata <- cbind(INC, MathScore, WhiteYes, BlackYes, HispanicYes, 
                AsianYes, Treatment, Response)
mydata <- data.frame(mydata)
mydata <- na.omit(mydata)

xvars <- c("WhiteYes", "BlackYes", "HispanicYes", "AsianYes", "INC")

table1 <- CreateTableOne(vars = xvars, strata = "Treatment", data = mydata, test = FALSE)
print(table1, smd = TRUE)

# the proportion of people who went to catholic school, 74% were white
# 59% of people who didn't go to catholic school were white

# the average income is far higher at catholic school

# this is not even close to a radnomized experiment



greedymatch <- Match(Tr = mydata$Treatment, M=1, X=mydata[xvars], replace = FALSE)
matched <- mydata[unlist(greedymatch[c("index.treated", "index.control")]), ]

matchedtab1 <- CreateTableOne(vars = xvars, strata = "Treatment", 
                              data = matched , test = FALSE)
print(matchedtab1, smd = TRUE)

# matching dropped 7000 people - dropped the poorer people


# we're not able to say that going to catholic school has a particular treatment effect for everyone
# but 

# these groups are too different to compare so i used matching

y_trt <- matched$Response[matched$Treatment == 1]
y_con <- matched$Response[matched$Treatment == 0]

t.test(y_trt, y_con, paired = TRUE)

# now there's only a -1 point difference bc some of that difference was an economic difference not a catholic school difference
# actually a negative -1.01 impact of catholic school



