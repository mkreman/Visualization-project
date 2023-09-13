library('ggplot2')

attach(diamonds)
head(diamonds)

?diamonds

nrow(diamonds)
ncol(diamonds)

hist(diamonds$price)

hist(diamonds$price, probability = TRUE)
# bins p_0-p_1
# freq f_1  -> sum(f_i) = n
# relative pro. f_1/n = P_1
# relative freq density P_1/w_1, w_1 -> width of the class

hist(diamonds$price, probability = TRUE, main='')

hist(diamonds$price, probability = TRUE, main='', col='red')

hist(diamonds$price, probability = TRUE, main='', col='red', xlab='Price($)')
grid(col='blue')

hist(log(diamonds$price), probability = TRUE, main='', col='red', xlab='Price($)')

?faithful

View(faithful)

hist(faithful$waiting)
hist(faithful$eruptions)

plot(faithful, pch=20)

#################################################
min_w = min(faithful$waiting)
max_w = max(faithful$waiting)

min_e = min(faithful$eruptions)
max_e = max(faithful$eruptions)

plot(NULL,
     xlim=c(min_e, max_e),
     ylim=c(min_w, max_w),
     xlab='eruptions',
     ylab='waiting')

grid(col='blue')
points(faithful$eruptions, faithful$waiting, pch=20)

# 
data(package="ggplot2")$results[, 'Item']
# Avaliable datasets
data()
