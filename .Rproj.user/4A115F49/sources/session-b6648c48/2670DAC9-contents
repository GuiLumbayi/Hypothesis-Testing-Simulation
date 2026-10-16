#===========================================================================  
    ##Portfolio Project: Two-Sample Independent T-Test via Data Simulation##
        ##Concepts: Welch's T-Test, Data Simulation, Visualization##
#===========================================================================


                      #1. Simulating The Dataset
#----------------------------------------------------------------------
#Because the original assignment provided summary statistic, we'll generate
# normal distributions matching those exact parameters for reproducibility 


set.seed(42)#this ensures that the data generated will be identical everytime the script runs

n1 <- 100 #sample 1
n2 <- 110 #sample 2

sample1_data <- rnorm(n1, mean = 186, sd = 29)
sample2_data <- rnorm(n2, mean = 217, sd = 34)

#Combining into a clean dataframe to save as a downloadable CSV
portfolio_dataset <- data.frame(Value = c(sample1_data,sample2_data), 
                               Group = c(rep("Sample 1", n1),rep("Sample 2", n2))
                               )

# Export the dataset so that you can later add GitHub
write.csv(portfolio_dataset, "simulated_hypothesis_data.csv", row.names = FALSE)



                  #2. Original Math behind analysis
#----------------------------------------------------------------------
mu0 <- 0 # hypothesized difference
x1bar <- 186 # sample1 mean
x2bar <- 217 #sample2 mean
s1 <- 29 #sample1 std dev
s2 <- 34 #sample 2 std dev

#Calculating T-statistic
t_stat <- ((x1bar-x2bar)-(mu0))/sqrt((s1^2/n1)+(s2^2/n2))
print(paste("Calculated T-Statisitic:", round(t_stat,4)))

#Calculating Degrees of Freedom
DF <- (s1^2/n1 + s2^2/n2)^2 / (((s1^2/n1)^2 / (n1 - 1)) + ((s2^2/n2)^2 / (n2 - 1)))
print(paste("Calculated Degrees of Freedom:", round(DF, 4)))

# Calculating P-Value
pval <- pt(-abs(t_stat), df = DF)
print(paste("Calculated P-Value:", pval))



              #3. VERIFICATION USING R'S BUILT-IN T-TEST
#----------------------------------------------------------------------
# Running a standard t-test on our simulated data vectors to confirm alignment
r_test_output <- t.test(sample1_data, sample2_data, mu = 0, alternative = "two.sided", var.equal = FALSE)
print(r_test_output)



                        #4. DATA VISUALIZATION
#----------------------------------------------------------------------
# Adding a visual element to make findings more engaging
boxplot(Value ~ Group, data = portfolio_dataset,
        main = "Comparison of Simulated Groups",
        xlab = "Experimental Groups",
        ylab = "Values",
        col = c("#76b5c5", "#e28743"),
        border = "black")


