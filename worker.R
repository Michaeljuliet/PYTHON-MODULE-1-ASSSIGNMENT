set.seed(456)

# Step 2: Generate a list of 400 workers dynamically with varying salary ranges
workers_v2 <- data.frame(
  name = paste0("Worker_", 1:400),
  gender = sample(c("Male", "Female"), 400, replace = TRUE),
  salary = sample(8000:35000, 400, replace = TRUE)
)

# Step 5: Exception handling is implemented to manage potential errors
generate_payment_slips_v2 <- function(workers_v2) {
  payment_slips <- vector("list", nrow(workers_v2))
  
  for (i in 1:nrow(workers_v2)) {
    tryCatch({
      # Step 4: Conditional logic is slightly different
      if (workers_v2$salary[i] >= 15000 & workers_v2$salary[i] < 25000) {
        workers_v2$level[i] <- "B2"
      } else if (workers_v2$salary[i] > 9000 & workers_v2$salary[i] < 20000 & workers_v2$gender[i] == "Female") {
        workers_v2$level[i] <- "B5-F"
      } else {
        workers_v2$level[i] <- "Standard"  # Default level for other cases
      }
      
      # Generate the payment slip
      payment_slips[[i]] <- paste("Payment Slip for", workers_v2$name[i], ": Salary =", workers_v2$salary[i], 
                                  ", Level =", workers_v2$level[i])
    }, error = function(e) {
      print(paste("An error occurred for worker", workers_v2$name[i], ":", e))
    })
  }
  
  return(payment_slips)
}

# Step 3: Generate payment slips for all workers
payment_slips_v2 <- generate_payment_slips_v2(workers_v2)

# Display a sample of payment slips
print(payment_slips_v2[1:10])
