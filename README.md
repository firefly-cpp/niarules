<p align="center">
  <img alt="logo" width="300" src=".github/logos/niarules.png">
</p>

<h1 align="center">
niarules - Numerical Association Rule Mining using Population-Based Nature-Inspired Algorithms
</h1>

<p align="center">
  <a href="https://github.com/firefly-cpp/niarules/actions?workflow=R-CMD-check">
    <img alt="R build status" src="https://github.com/firefly-cpp/niarules/workflows/R-CMD-check/badge.svg">
  </a>
  <a href="https://CRAN.R-project.org/package=niarules">
    <img alt="CRAN version" src="https://www.r-pkg.org/badges/version/niarules">
  </a>
  <a href="https://cran.r-project.org/package=niarules">
    <img alt="CRAN downloads" src="https://cranlogs.r-pkg.org/badges/grand-total/niarules?color=blue">
  </a>
  <a href="https://doi.org/10.32614/CRAN.package.niarules">
    <img alt="DOI" src="https://img.shields.io/badge/DOI-10.32614/CRAN.package.niarules-blue">
  </a>
</p>

<p align="center">
  <a href="#-about">📋 About</a> •
  <a href="#-detailed-insights">✨ Detailed insights</a> •
  <a href="#-installation">📦 Installation</a> •
  <a href="#-usage">🚀 Usage</a> •
  <a href="#-reference-papers">📄 Reference papers</a> •
  <a href="#-license">🔑 License</a>
</p>

## 📋 About

niarules is an R framework designed for mining numerical association rules through the utilization of nature-inspired algorithms for optimization. Drawing inspiration from both the [NiaARM Python package](https://github.com/firefly-cpp/NiaARM) and [NiaARM.jl package](https://github.com/firefly-cpp/NiaARM.jl), this repository introduces the capability to perform numerical association rule mining in the R programming language.

The current version of niarules included in this framework offers seamless functionality for automatic dataset loading and preprocessing. It facilitates the exploration of numerical association rules through the application of nature-inspired algorithms, ultimately presenting a comprehensive output that includes identified association rules. Aligning with the principles of the original NiaARM implementation, the process of numerical association rule mining is conceptualized as an optimization problem, and the solution is achieved using population-based nature-inspired algorithms integrated within this framework.

## ✨ Detailed insights
The current version includes (but is not limited to) the following functions:

- loading datasets in CSV format 📂
- preprocessing of data 🔄
- searching for association rules 🔍
- providing an output of mined association rules 📝
- generating statistics about mined association rules 📊
- providing the implementation of several state-of-the-art nature-inspired algorithms for optimization 🧬

## 📦 Installation

Install CRAN release version:

```R
install.packages("niarules")
```

## 🚀 Usage

### Basic run example

```R
library("niarules")
dataset <- "Abalone.csv"
# read dataset
data <- read_dataset(dataset)
# get features
features = extract_feature_info(data)
dim <- problem_dimension(features)
# Use Differential Evolution algorithm for discovering association rules
de <- differential_evolution(D = dim, NP = 30, F = 0.5, CR = 0.9, nfes = 1000, features, data)
print_association_rules(de$arules)
```
## 📄 Reference papers

Ideas are based on the following research papers:

[1] Stupan, Ž., & Fister Jr., I. (2022). [NiaARM: A minimalistic framework for Numerical Association Rule Mining](https://joss.theoj.org/papers/10.21105/joss.04448.pdf). Journal of Open Source Software, 7(77), 4448.

[2] I. Fister Jr., A. Iglesias, A. Gálvez, J. Del Ser, E. Osaba, I Fister. [Differential evolution for association rule mining using categorical and numerical attributes](https://www.iztok-jr-fister.eu/static/publications/231.pdf) In: Intelligent data engineering and automated learning - IDEAL 2018, pp. 79-88, 2018.

[3] I. Fister Jr., V. Podgorelec, I. Fister. [Improved Nature-Inspired Algorithms for Numeric Association Rule Mining](https://link.springer.com/chapter/10.1007/978-3-030-68154-8_19). In: Vasant P., Zelinka I., Weber GW. (eds) Intelligent Computing and Optimization. ICO 2020. Advances in Intelligent Systems and Computing, vol 1324. Springer, Cham.

[4] I. Fister Jr., I. Fister [A brief overview of swarm intelligence-based algorithms for numerical association rule mining](https://arxiv.org/abs/2010.15524). arXiv preprint arXiv:2010.15524 (2020).

## See also

[1] [NiaARM.jl: Numerical Association Rule Mining in Julia](https://github.com/firefly-cpp/NiaARM.jl)

[2] [NiaARM: Numerical Association Rule Mining in Python](https://github.com/firefly-cpp/NiaARM)

[3] [arm-preprocessing: Implementation of several preprocessing techniques for Association Rule Mining (ARM)](https://github.com/firefly-cpp/arm-preprocessing)

## 🔑 License

This package is distributed under the MIT License. This license can be found online at <http://www.opensource.org/licenses/MIT>.

## Disclaimer

This framework is provided as-is, and there are no guarantees that it fits your purposes or that it is bug-free. Use it at your own risk!
