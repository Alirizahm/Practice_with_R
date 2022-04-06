# QUestion
# Is the average weight of rats similar or not with the dataset, which is 25 kg?
# Find statistics summary from dataset (mean,median)
# Visualization using box plot
# Visualization using histogram
# One sample t-test to check whether the average weight of rats = 25 kg

# Create dataset
set.seed(1234)
my_data <- data.frame( name = paste0(rep("M_", 10), 1:10), weight = round(rnorm(10, 20, 2), 1))
my_data

# Find mean 
my_data.mean <- mean(my_data$weight)
my_data.mean

# Mean 19.25, so the average weight of rats is different

# Find median 
my_data.median <- median(my_data$weight)
my_data.median

# Median 18.9

# Summary my_data
summary(my_data)


# Visualization using boxplot
boxplot(my_data$weight)
boxplot(my_data$weight,
        main = "Rata-rata berat Tikus dalam kg",
        xlab = "Kg",
        ylab = "Berat Tikus",
        col = "Red",
        border = "Black",
        horizontal = TRUE,
        notch = FALSE
)


'''
The dataset is normally distributed, where there are no outliers that cross the line,
but the dataset leads to a negative skew. The dataset is mostly ranged from 18.2 - 21 with a median value of 18.9
'''

# Visualization using histogram
hist(my_data$weight)
berat <- my_data$weight
berat
hist(berat,
     main="Berat rata-rata Tikus",
     xlab="Berat Tikus dalam kg",
     xlim=c(14,23),
     col="Brown",
     freq=TRUE
)

'''
The dataset is normally distributed, the distance between the data is not
so far off, and it looks like there are no outliers, but the dataset looks to have a negative skew distribution
'''

# One sample t-test to check whether the average weight of rats = 25 kg or not.
# H0 = The average weight of rats is equal to 25 kg
# H1 = The average weight of rats is not equal to 25 kg
one.sample.t_test <- t.test(berat, mu= 25)
one.sample.t_test

# alpha 5 %
t <- one.sample.t_test$statistic
t_table_1 <- qt(p=0.025, df=9)
t_table_2 <- qt(p=0.975, df=9)

t_table_1
t_table_2

print((t< t_table_1) | (t> t_table_2))

# because t is in the critical region, then H0 is rejected so that the average weight of rats is not equal to 25 kg