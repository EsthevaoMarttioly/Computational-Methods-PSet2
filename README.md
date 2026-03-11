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

output/

renv/

.RData

.Rhistory

.Rprofile

EsthevaoMarttioly_PSet2.R

EsthevaoMarttioly_PSet2.Rproj

EsthevaoMarttioly_PSet2.pdf

README.md

## Reproducibility

Certificate yourself that you open directly the .Rproj file.

To reproduce the environment:
renv::restore()

It was built in RStudio Version 2026.01.1+403 and R Version R-4.5.2-win.

If needed, you can install packages using:

install.packages(c(
"tibble","tidyverse","ggplot2","pracma","stargazer"
))

## Running the project

To reproduce the analysis:

* Open the file: EsthevaoMarttioly_PSet2.Rproj
* Restore package versions: renv::restore()
* Run the script: EsthevaoMarttioly_PSet2.R
* Output: The script generates the following figures in the output/ folder:
  * p2_function_g.pdf
  * p2_lifeexp_interp.pdf
  * p2_pareto_pdf.pdf




