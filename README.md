**Content**
This repository contains all relevant scripts for the submission of my master thesis. Several methods that generate prediction intervals are introduced and implemented. Consecutively, the results are are used to help identify particularly valuable customers.

**Execution of the scripts**
Every script and other file that is relevant for the submission of the thesis is located in the "Submission" folder of the CP_managerial_version branch.
To reproduce all results, first make sure, that the repository is up to date and the "plots", "Results" and "Covariates plots" folders are empty except empty.txt files and empty subfolders.
From here, one can run "Main function all.R" which will take around 20h, depending on the machine. For the most results, it is only necessary to run the lines until "source(paste0(getwd(), "/Main function one.r"))" (including) which will only take around 5h. For the stability analysis regarding different learning and prediction times, which is only a side section of this work, the last line, "source(paste0(getwd(), "/Main function periods.r"))" must be executed.
Be aware that Quantile Regression uses parallel computing, claiming several cores, what might influence the machine's performance during this time.

**Results**
The old results and plots, that I produced can be found in the folders "... - old". For the central results, the scripts do automatically compare those to the new results and save the comparison in a variable, called "repr", which should ideally hold only "TRUE" values. An exemption are computation times which differ from machine to machine and lead to "FALSE" entries for "CET_measures" and "PTS_measures". The new results and plots will be stored in the respective folders.

In case, an error occurs or you have any additional questions, feel free to reach out to me via e-mail.