# script for cars data analysis code

# Load data
library(ggplot2)
library(corrplot)

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
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
levels(mtcars$am) <- c("auto", "man")
#mtcars$trans <- as.factor(ifelse(mtcars$am == "auto", 1, 0)) # auto=1, manual = 0
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)


# Basic exploratory data analysis

# Plot MPG vs trans
g <- ggplot(mtcars, aes(x=am, y=mpg, fill=am)) + 
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
g1 <- ggplot(mtcars, aes(x=am, y=mpg, fill=am)) + 
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
#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#corrplot(car_corrs, method="color", col=col(200),  
#         type="upper", order="hclust", 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=45, #Text label color and rotation
#         # Combine with significance
#         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#         # hide correlation coefficient on the principal diagonal
#         diag=FALSE 
#)

#corrplot(car_corrs, method="number")


