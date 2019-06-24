require(data.table)
load("brexit.rdata")

#backup
brexit_orig <- brexit
results_orig <- results

brexit_areas <- brexit$LAName
results_areas <- names(results)

missing_in_results_dataset <- brexit_areas[!(brexit_areas %in% results_areas)]
missing_in_brexit_dataset <- results_areas[!(results_areas %in% brexit_areas)]

# removing things that don't exit on brexit
results <- results[-which(results_areas %in% missing_in_brexit_dataset)]

i <- 1

merged_dataset <- list()

for (results_column in names(results)) {
  # flattening 1 level
  row <- as.list(unlist(results[[results_column]]))
  row_brexit <- as.list(brexit[brexit$LAName == results_column,])
  
  merged_dataset[[i]] <- append(row_brexit, row)
  
  i <- i + 1
}

numeric_columns <- 14:40
merged_dataset <- rbindlist(merged_dataset, fill=TRUE)
merged_dataset[, numeric_columns] <- lapply(numeric_columns, function(col) {
  as.numeric(merged_dataset[[col]])
})

write.csv(merged_dataset, "leke.csv")