library("niarules")

dataset <- "Abalone.csv"

# read dataset
data <- read_dataset(dataset)

# get features
features <- extract_feature_info(data)

dim <- problem_dimension(features)

de <- differential_evolution(
  d = dim,
  np = 30,
  f = 0.5,
  cr = 0.9,
  nfes = 1000,
  features,
  data
)

print_association_rules(de$arules)
