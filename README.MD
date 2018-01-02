PROJECT 1: CLASSIFICATION

This goal of this project was to increasing familiarity with data classifications packages available in R. 

It works on 'Life Expectancy Dataset' (available on https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy, section List by CIA). It tries to identify continent of each entry based on life expectancy.

It uses the following classification techniques:
    1) K-nearest neighbor
    2) RIPPER
    3) C4.5
    4) SVM

Instructions to run file:

    1) Set working directory to current folder. (This is required as the script references to the excel sheet present in this folder. If different excel has to be loaded please change path in readData())
    
    2) Install the following packages (using install.packages(), code for this is present but commented in script to avoid reinstallation) 
	i) rJava (Required for RIPPER and C4.5)	
	ii) readxl (Required to read excel file)
	iii) Rweka (Required for RIPPER and C4.5) 
	iv) caret (Required for RIPPER and KNN)
	v) e1701  (Required for SVM)
	vi) zeallot (Required for multiple output) 
    3) Run script. User input for seed will be asked. Input required value.  
