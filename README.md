# niarules

# About

niarules is an R framework designed for mining numerical association rules through the utilization of nature-inspired algorithms for optimization. Drawing inspiration from both the [NiaARM Python package](https://github.com/firefly-cpp/NiaARM) and [NiaARM.jl package](https://github.com/firefly-cpp/NiaARM.jl), this repository introduces the capability to perform numerical association rule mining in the R programming language.

The current version of NiaARM.jl included in this framework offers seamless functionality for automatic dataset loading and preprocessing. It facilitates the exploration of numerical association rules through the application of nature-inspired algorithms, ultimately presenting a comprehensive output that includes identified association rules. Aligning with the principles of the original NiaARM implementation, the process of numerical association rule mining is conceptualized as an optimization problem, and the solution is achieved using population-based nature-inspired algorithms integrated within this framework.

## Detailed insights
The current version includes (but is not limited to) the following functions:

- loading datasets in CSV format,
- preprocessing of data,
- searching for association rules,
- providing an output of mined association rules,
- generating statistics about mined association rules,
- providing the implementation of several state-of-the-art nature-inspired algorithms for optimization.

## Installation

## Usage

### Basic run example

## Reference papers:

Ideas are based on the following research papers:

[1] Stupan, Ž., & Fister Jr., I. (2022). [NiaARM: A minimalistic framework for Numerical Association Rule Mining](https://joss.theoj.org/papers/10.21105/joss.04448.pdf). Journal of Open Source Software, 7(77), 4448.

[2] I. Fister Jr., A. Iglesias, A. Gálvez, J. Del Ser, E. Osaba, I Fister. [Differential evolution for association rule mining using categorical and numerical attributes](http://www.iztok-jr-fister.eu/static/publications/231.pdf) In: Intelligent data engineering and automated learning - IDEAL 2018, pp. 79-88, 2018.

[3] I. Fister Jr., V. Podgorelec, I. Fister. [Improved Nature-Inspired Algorithms for Numeric Association Rule Mining](https://link.springer.com/chapter/10.1007/978-3-030-68154-8_19). In: Vasant P., Zelinka I., Weber GW. (eds) Intelligent Computing and Optimization. ICO 2020. Advances in Intelligent Systems and Computing, vol 1324. Springer, Cham.

[4] I. Fister Jr., I. Fister [A brief overview of swarm intelligence-based algorithms for numerical association rule mining](https://arxiv.org/abs/2010.15524). arXiv preprint arXiv:2010.15524 (2020).

## License

This package is distributed under the MIT License. This license can be found online at <http://www.opensource.org/licenses/MIT>.

## Disclaimer

This framework is provided as-is, and there are no guarantees that it fits your purposes or that it is bug-free. Use it at your own risk!
