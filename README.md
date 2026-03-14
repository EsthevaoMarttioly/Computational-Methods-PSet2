# Computational Methods  - Problem Set2

Author: **Esthevao Marttioly Lopes Martins**  
Program: MSc Economics — FGV EESP  

This repository contains the code and results for **Problem Set 2** of the Computational Methods course.

The project implements numerical methods in R, including:

- function interpolation
- grid approximation methods
- root-finding algorithms

All code is written with reproducibility and defensive programming in mind.

## Project Structure

EsthevaoMarttioly_PSet2/

├── code/

├── output/

├── renv/                             # R environment files

│   ├── library/ 

│   ├── activate.R 

│   └── settings.json 

├── EsthevaoMarttioly_PSet2.Rproj     # R project file

├── renv.lock                         # R environment lock file

└── README.md

## Computational Environment

Certificate yourself that you open directly the .Rproj file.

To reproduce the environment:
renv::restore()

The analysis was conducted using R version 4.5.2 (2026-03-10) on a Windows 11 system. All the required R packages and their versions are listed in the renv environments file located in the `renv/` directory.

## Running the project

To reproduce the analysis:

* Open the file: EsthevaoMarttioly_PSet2.Rproj
* Restore package versions: renv::restore()
* Run the script: EsthevaoMarttioly_PSet2.R
* Output: The script generates the following figures in the output/ folder:
  * p2_function_g.png
  * p2_lifeexp_interp.png
  * p2_pareto_pdf.png




