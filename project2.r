library(MASS)
dat <- read.csv('data.csv', header = T)
M_e <- lm(Y ~ E1 + E2 + E3 + E4, data = dat)


# print(summary(M_e)
# print('Adjusted R^2 value')
print(summary(M_e)$adj.r.squared)


M_raw <- lm(Y ~ (E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20)^2, data = dat)
# plot(resid(M_raw) ~ fitted(M_raw), main = 'Residual Plot')
print(summary(M_raw)$adj.r.squared)
# boxcox(M_raw)

M_trans <- lm(I(Y^1.1) ~ (E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20)^2, data = dat)
print(summary(M_trans)$adj.r.squared)
#plot(resid(M_trans) ~ fitted(M_trans), main = 'Transformed Residual Plot')
