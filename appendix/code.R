
## load library
library(dplyr)

## load data
#nhanes <- read_csv(here::here("data/nhanes_dataset.csv"))


## check characterisitcs of variables
colnames(NHANES)
summary(NHANES$BPSysAve)
hist(NHANES$TotChol)
hist(NHANES$BPSysAve)
cor(NHANES$BMI, NHANES$BPSysAve, use = "complete.obs")
hist(NHANES$DirectChol)
table(NHANES$Gender)
table(NHANES$Education)
range(NHANES$Age)
table(NHANES$Diabetes)
table(NHANES$PhysActive)
summary(NHANES$PhysActiveDays)
summary(NHANES$DiabetesAge)
summary(NHANESa$BMI)
summary(NHANES$Smoke100)
summary(NHANES$TotChol)



## continuous outcome
data <- NHANES %>%
  select(
    id = ID, w1 = Age, w2 = Gender, w3 = Education, w4 = Smoke100,
    a = PhysActive, # this is the exposure
    m = BMI, # this is the mediator
    y = DirectChol # this is the outcome
  ) %>%
  na.omit()


table(data$w3)
data$w3 <- relevel(factor(data$w3), ref = "College Grad")


fita <- lm(m ~ a + w1 + w2 + w3 + w4, data = data)
summary(fita)



fitb <- lm(y ~ a + m + w1 + w2 + w3 + w4, data = data)
summary(fitb)


fitc <- lm(y ~ a + w1 + w2 + w3 + w4, data = data)
summary(fitc)

direct_ma <- fitb$coefficients[2]
direct_ma

indirect_ma <- fita$coefficients[2] * fitb$coefficients[3]
indirect_ma


total_ma <- fitc$coefficients[2]
total_ma


indirect_mb <- fitc$coefficients[2] - fitb$coefficients[2]
indirect_mb

install.packages('mediation')
library(mediation) # Mediation package
set.seed(260524)

# Fit mediator model
lm_m <- lm(m ~ a + w1 + w2 + w3 + w4, data = data)
# Fit outcome model
lm_y <- lm(y ~ a + m + w1 + w2 + w3 + w4, data = data)
# Run mediation analysis
results <- mediate(lm_m, lm_y, treat = "a", mediator = "m", boot = TRUE, sims = 10) #can set to 500

summary(results)
plot(results)


library(dplyr)
## binary outcome
data <- NHANES %>%
  select(
    id = ID, w1 = Age, w2 = Gender, w3 = Education, w4 = Smoke100,
    a = PhysActive, # this is the exposure
    m = BMI, # this is the mediator
    y = Diabetes # this is the outcome
  ) %>%
  na.omit()

library(dplyr)
##if get an error, use below dplyr::select, to ensure use the selection function from the dplyr package
data <- NHANES %>%
  dplyr::select(ID, Age, Gender, Education, Smoke100, PhysActive, BMI, Diabetes)  %>%
  rename(
    id = ID, w1 = Age, w2 = Gender, w3 = Education, w4 = Smoke100,
    a = PhysActive, # Exposure
    m = BMI,        # Mediator
    y = Diabetes    # Outcome
  ) %>%
  na.omit()


##diabetes is coded as Yes and No
##y is a factor with two levels:This means that "No" is coded as 1 and "Yes" is coded as 2,
str(data$y)
levels(data$y)


## recode yes to 1, no to 0
data <- data %>%
  mutate(y = if_else(y == "Yes", 1, 0))

table(data$y)


table(data$w3)
data$w3 <- relevel(factor(data$w3), ref = "College Grad")


fita <- lm(m ~ a + w1 + w2 + w3 + w4, data = data)
summary(fita)



fitb <- glm(y ~ a + m + w1 + w2 + w3 + w4, data = data, family = binomial(link = "logit"))
summary(fitb)



fitc <- glm(y ~ a + w1 + w2 + w3 + w4, data = data, family = binomial(link = "logit"))
summary(fitc)

# Direct effect (log-odds scale)
direct_effect <- fitb$coefficients[2]
direct_effect

# Indirect effect (log-odds scale)
indirect_effect <- fita$coefficients[2] * fitb$coefficients[3]
indirect_effect

exp(direct_effect)
exp(indirect_effect)

library(mediation) # Mediation package
set.seed(260524)

# Fit mediator model
lm_m <- lm(m ~ a + w1 + w2 + w3 + w4, data = data)
# Fit outcome model
lm_y <- glm(y ~ a + m + w1 + w2 + w3 + w4, data = data, family = binomial(link = "logit"))
# Run mediation analysis
results <- mediate(lm_m, lm_y, treat = "a", mediator = "m", boot = TRUE, sims = 10) #can set to 500
summary(results)

test.TMint(results, conf.level=.95)


install.packages('medflex')
library(medflex)
data <- data %>%
  mutate(a = if_else(a == "Yes", 1, 0))


med.fit <- lm(m ~ a + w1 + w2 + w3 + w4, data = data)
expData <- neWeight(med.fit)
