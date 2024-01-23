library(niarules)

# load dataset
file_path <- "/home/max/R/Abalone.csv"

# read dataset
data <- read_dataset(file_path)

# analyze features
features = extract_feature_info(data)

# print features
print_feature_info(features)
