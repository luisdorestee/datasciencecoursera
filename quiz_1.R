# Set the working directory to where your R script is located
setwd("C:/Users/User/Desktop/RStudio/datascience") # Nos quitamos líos

pollutanmean <- function(directory = "datascience", pollutant, id) {
  # Create an empty vector to store the pollutant values
  pollutant_values <- numeric()
  
  # Loop through the specified id values
  for (i in id) { # Vamos por todo el range
    # Generate the file name with leading zeros (e.g., "001.csv", si i es 1, ...)
    file_name <- sprintf("%03d.csv", i) 
    
    # Construct the full file path
    file_path <- file.path(directory, "specdata", file_name) 
    
    # Check if the file exists
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      
      # Extract the specified pollutant column and remove NA values
      pollutant_column <- data[[pollutant]]    #Pillo la columna 'pollutant'
      pollutant_column <- pollutant_column[!is.na(pollutant_column)] #Quito los NA
      
      # Append the pollutant values to the vector
      pollutant_values <- c(pollutant_values, pollutant_column) #Concateno vector original con lo pillado en este loop
    } else {
      cat("File not found:", file_path, "\n")
    }
  }
  
  # Calculate the mean of the pollutant values
  mean_value <- mean(pollutant_values, na.rm = TRUE)
  
  return(mean_value)
}

# Example usage:
result <- pollutanmean(directory = ".", pollutant = "nitrate", id = 1:332) # "." hace referencia al repositorio actual
cat("Mean:", result, "\n") #Concatena e imprime
#Todos los ejemplos dan bien


############################################################################
setwd("C:/Users/User/Desktop/RStudio/datascience")

complete <- function(directory = "datascience", id) {
  # Create an empty data frame with columns "id" and "nobs"
  df <- data.frame(id = numeric(0), nobs = numeric(0)) #Creo el dataframe con el nombre de las columnas
  
  for (i in id) { 
    file_name <- sprintf("%03d.csv", i) 
    file_path <- file.path(directory, "specdata", file_name) 
    
    if (file.exists(file_path)) {
      print(paste("Processing file:", file_name))
      
      data <- read.csv(file_path)
      
      val_1 <- i # id number          #Guardo el número id (valor primera columna)
      n_column = data[["nitrate"]]    #Pillo los valores de las columnas de ambos pollutants
      s_column = data[["sulfate"]]
      val_2 <- sum(!is.na(n_column) & !is.na(s_column)) #Sumo el número de columnas que hay valores en ambas
      
      new_row <- data.frame(id = val_1, nobs = val_2) #Creo nueva fila para añadirla abajo
      df <- rbind(df, new_row)
      
    } else {
      cat("File not found:", file_path, "\n")
    }
  }
  return(df)
}

result <- complete(directory = ".", id = 1)
print(result)
#Todos los ejemplos dan bien


############################################################################
setwd("C:/Users/User/Desktop/RStudio/datascience")

corr <- function(directory, threshold) {
  # Create an empty vector to store correlations
  correlations <- numeric(0)
  
  # Get a list of all CSV files in the specified directory
  for (i in 1:332) { 
    file_name <- sprintf("%03d.csv", i) 
    file_path <- file.path(directory, "specdata", file_name)
    print(paste("Processing file:", file_name))
    data <- read.csv(file_path)
    
    # Check if the number of complete cases is greater than the threshold
    n_column = data[["nitrate"]]    
    s_column = data[["sulfate"]]
    val_2 <- sum(!is.na(n_column) & !is.na(s_column))
    print(val_2)
    if (val_2 > threshold) {
      valid_indx <- !is.na(n_column) & !is.na(s_column)
      final_n <- n_column[valid_indx]
      final_s <- s_column[valid_indx]
      
      # Calculate the correlation between sulfate and nitrate
      correlation <- cor(final_n, final_s)
      print(correlation)
      
      # Append the correlation to the vector
      correlations <- c(correlations, correlation)
    }
  }
  
  return(correlations)
}
result_corr <- corr(directory = ".", threshold = 5000)
head(result_corr)
summary(result_corr)
file_len <- length(result_corr)
cat("File length", file_len, "\n")