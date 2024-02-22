library("niarules")

dataset <- "Abalone.csv"

# read dataset
data <- read_dataset(dataset)

# get features
features = extract_feature_info(data)

dim <- problem_dimension(features)

de <- differential_evolution(D = dim, NP = 10, F = 0.5, CR = 0.9, nfes = 10, features, data)

print (de$arules)

