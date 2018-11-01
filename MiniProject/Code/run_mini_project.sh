# !/bin/bash
# Fernando Pedraza Perez
# This bash script generates the Miniproject write-up with necessary data wrangling, model fitting
# and generation of plots. This script should be located in a 'Code' directory with the following scripts:
# TPC_data.R, models.py, plot_polynomial and Mini_project.tex. A 'Data' directory should be at the same
# level as 'Code' and must contain the

# What does this script do?
echo -e " ############################# "
echo -e "\n This script will run the CMEE Miniproject \n"
echo -e "\n The Miniproject is composed of four parts:\n"
echo -e " I. An R script that performs data wrangling\n"
echo -e " II. A Python script that performs model fitting\n"
echo -e " III. An R script that generates plots of the results\n"
echo -e " IV. A Latex document which contains the write-up of the project \n"
echo -e " *Wrangled Data will be saved as a 'csv' file in the Data directory*\n"
echo -e " *Parameter estimates from model fitting will be saved as a 'csv' file in the Data directory*\n"
echo -e " *Plots will be saved as 'pdf' files in the Results directory\n*"
echo -e " The Miniproject will now be generated...\n"

##########################
# Section I
# Performing Data Wrangling
echo -e " The Data Wrangling is now being performed in R:\n"
Rscript TPC_data.R
echo -e " Data Wrangling has finished!\n"

##########################
# Section II
# Performing Model Fitting
echo -e "Model Fitting is now being performed in Python:\n"
python models.py
echo -e "Model Fitting has finished!\n"

##########################
# Section III
# Performing Model Fitting
echo -e "Plotting is now being performed in R:\n"
Rscript plot_polynomial.R
echo -e "Plotting has finished!\n"

##########################
# Section IV
# Generating Latex document
#echo -e "Now compiling the write-up in Latex:\n"
#pdflatex Mini_project.tex
#pdflatex Mini_project.tex
#bibtex Mini_project # REVISAR ESTO!****
#pdflatex Mini_project.tex
#pdflatex Mini_project.tex
#evince Mini_project.pdf &
## Cleanup
#echo -e " Removing auxiliary files\n"
#rm *∼
#rm *.aux rm *.dvi rm *.log rm *.nav rm *.out rm *.snm rm *.toc
echo -e " The write-up is compiled!\n"

echo -e " All tasks have been completed!\n"
echo -e " Exiting...\n"
echo -e " ############################# "
