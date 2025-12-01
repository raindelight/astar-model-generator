# An Exact Algorithm for the Discovery of Mathematical Programming Models in MiniZinc

Poznan University of Technology Thesis Repository

## Abstract

This repository contains the source code and experimental data for the thesis titled "An exact algorithm for the discovery of Mathematical Programming models in MiniZinc."
The project implements a Model Generator written in Scala that automatically discovers Constraint Programming (CP) models.
By utilizing an A* search algorithm, the tool explores the space of possible mathematical constraints to find a model that maps given input data to provided example solutions.

## Motivation
Manual creation of Mathematical Programming models in MiniZinc is a complex, time-consuming task that requires significant domain expertise. It involves translating abstract requirements into strict mathematical constraints.

While recent advancements in AI (specifically Large Language Models) offer code generation capabilities, they suffer from hallucination and non-determinism.
In the context of mathematical optimization, a generated model must be correct and reproducible.

This project addresses these issues by:

- Automating Model Discovery: Removing the manual burden of writing constraints from scratch.

- Ensuring Determinism: Unlike stochastic ML models, this A* approach guarantees that the same input data and solution examples will always yield the same model structure.

- Interpretability: The output is standard, readable MiniZinc code.

## Usage


## Installation & Prerequisites
To build and run this generator locally, ensure you have the following installed:
1. Java SDK
2. sbt
```bash
git clone https://github.com/raindelight/astar-model-generator.git
cd astar-model-generator
sbt compile
```

If you want to download all minizinc-challenge models you can use helper scripts located in `scripts`. As the time goes these links may not work correctly.
```bash
# run this from project root
chmod +x scripts/download_models.sh
./scripts/download_models.sh
```
```bash
# if you are on windows
./scripts/download_models.ps1
```

## License
This project is licensed under the *MIT License*. See [LICENSE](LICENSE) file for details.

## Citation
If you use this work in your research, please cite the thesis as follows:
todo

