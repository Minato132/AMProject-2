
library(MASS)
library(leaps)
library(knitr)
dat <- read.csv('data.csv', header = T)
M_e <- lm(Y ~ E1 + E2 + E3 + E4, data = dat)


# print(summary(M_e)
print("Adjusted R^2 value of Environmental Var")
print(summary(M_e)$adj.r.squared)


M_raw <- lm(Y ~ (E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20)^2, data = dat)
#plot(resid(M_raw) ~ fitted(M_raw), main = 'Residual Plot')
print("Adjusted R^2 value of Raw Data")
print(summary(M_raw)$adj.r.squared)
#boxcox(M_raw)

M_trans <- lm(I(Y^1.1) ~ (E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20)^2, data = dat)
print("Adjusted R^2 value of our Transformed Data")
print(summary(M_trans)$adj.r.squared)
#plot(resid(M_trans) ~ fitted(M_trans), main = 'Transformed Residual Plot')



# Stepwise Regression starts here
M <- regsubsets(model.matrix(M_trans)[,-1], I((dat$Y)^1.1),
                nbest = 1,
                nvmax = 5,
                method = 'forward',
                intercetp = TRUE
)
temp <- summary(M)
Var <- colnames(model.matrix(M_trans))


M_select <- apply(temp$which,
                    1,
                    function(x) paste0(Var[x], collapse = '+'))

table <- kable(data.frame(cbind(model = M_select,
                        adjR2 = temp$adjr2,
                        BIC = temp$bic,
                        caption = 'Model Summary'
)))

print(table)



M_main <- lm(I(Y^1.1) ~ E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20, data = dat)
temp <- summary(M_main)
table <- kable(temp$coefficients[abs(temp$coefficients[,4]) <= .001,], caption = 'Sig Coefficients')
#print(table)



# We choose the 2nd model as that is the one with the most significant model

M_2stage <- lm(I(Y^1.1) ~ (E3 + G15 + G20)^2, data = dat)
temp <- summary(M_2stage)
table <- kable(temp$coefficients[abs(temp$coefficients[,3]) >= 4,],
                caption = "2nd Step in Linear Regression Model"
)
#print(table)
print("R^2 of our second linear regression with the model")
print(summary(M_2stage)$adj.r.squared)

# Final Model
M_final <- lm(I(Y^1.1) ~ E3 + G15:G20, data = dat)
print('R^2 of our Final Model')
print(summary(M_final)$adj.r.squared)

#Check model
M_check <- lm(I(Y^1.1) ~ E3, data = dat)
temp <- summary(M_check)
print(temp)