# script for cars data analysis code

# Load data
library(ggplot2)
library(corrplot)
library(leaps)

data(mtcars)
head(mtcars)

# Light preprocessing

# get correlation matrix before turning everything into factors
car_corrs <- cor(mtcars)

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mtcars)


# create factors
mtcars_fac <- mtcars
mtcars_fac$cyl <- as.factor(mtcars_fac$cyl)
mtcars_fac$vs <- as.factor(mtcars_fac$vs)
mtcars_fac$am <- factor(mtcars_fac$am)
levels(mtcars_fac$am) <- c("auto", "man")
#mtcars_fac$trans <- as.factor(ifelse(mtcars_fac$am == "auto", 1, 0)) # auto=1, manual = 0
mtcars_fac$gear <- factor(mtcars_fac$gear)
mtcars_fac$carb <- factor(mtcars_fac$carb)


# Basic exploratory data analysis

# Plot MPG vs trans
g <- ggplot(mtcars_fac, aes(x=am, y=mpg, fill=am)) + 
  theme(legend.position="none"
        , panel.background = element_rect(fill='grey')
        , plot.background = element_rect(fill='darkseagreen')
        , plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle('Mileage By Transmission Type') +
  labs(x="Transmission Type", y="MPG") +
  geom_violin(trim=TRUE) +
  scale_fill_brewer(palette="Blues") + 
  geom_boxplot(width=0.05) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5)

print(g)

# Plot MPG vs (trans x cyls)
g1 <- ggplot(mtcars_fac, aes(x=am, y=mpg, fill=am)) + 
  theme(legend.position="none"
        , panel.background = element_rect(fill='grey')
        , plot.background = element_rect(fill='darkseagreen')
        , plot.title = element_text(hjust = 0.5)
        ) +
  ggtitle('Mileage By Transmission Type and # Of Cylinders') +
  labs(x="Transmission Type", y="MPG") +
  facet_wrap(~cyl, nrow=1) +
  geom_violin(trim=TRUE) +
  scale_fill_brewer(palette="Blues") + 
  geom_boxplot(width=0.05) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .5)

print(g1)


# look at variable correlations to decide which variables to throw away
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(car_corrs, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#corrplot(car_corrs, method="number")

# all subsets method of variable selection
best.subset <- regsubsets(mpg ~ ., mtcars, nvmax=10)
best.subset.summary <- summary(best.subset)
best.subset.summary$outmat

# results
#         cyl disp hp  drat wt  qsec vs  am  gear carb
#1  ( 1 ) " " " "  " " " "  "*" " "  " " " " " "  " " 
#2  ( 1 ) "*" " "  " " " "  "*" " "  " " " " " "  " " 
#3  ( 1 ) " " " "  " " " "  "*" "*"  " " "*" " "  " " 
#4  ( 1 ) " " " "  "*" " "  "*" "*"  " " "*" " "  " " 
#5  ( 1 ) " " "*"  "*" " "  "*" "*"  " " "*" " "  " " 

# so tranny is only the 3rd important variable - weight being #1 and cyl/qsec (correlated) being #2
# so lets explore the smallest model with tranny type

best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
best.subset.by.adjr2

best.subset.by.cp <- which.min(best.subset.summary$cp)
best.subset.by.cp

best.subset.by.bic <- which.min(best.subset.summary$bic)
best.subset.by.bic

par(mfrow = c(2, 2), oma = c( 0, 0, 2, 0 ))
plot(best.subset$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(best.subset.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(best.subset.by.adjr2, best.subset.summary$adjr2[best.subset.by.adjr2], col="red", cex =2, pch =20)
plot(best.subset.summary$cp, xlab="Number of Variables", ylab="CP", type="l")
points(best.subset.by.cp, best.subset.summary$cp[best.subset.by.cp], col="red", cex =2, pch =20)
plot(best.subset.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(best.subset.by.bic, best.subset.summary$bic[best.subset.by.bic], col="red", cex =2, pch =20)
title( "Regression Variable Selection KPIs", outer = TRUE )

#coef(best.subset, 3)
