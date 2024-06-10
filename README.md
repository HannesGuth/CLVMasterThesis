**Content**
- literature pdfs and notes on the literature
- code files containing different ways to construct PIs
- 1 main code, "Main function.r" which calls the necessary other files
- a Word draft for the thesis itself which will later be transformed to latex

**Use**
- in theory, you should only need to execute "Main function.r" to run all relevant codes, approx. 15min (be aware that I use parallel computing, claiming the number of cores - 2)
- when you want to look into the implementation of each method, see what files are called in "Main function.r"
- note:
  - "Coefficients.r" contains the ensemble method, former "Method 1"
  - the plotting and benchmarking functions are not up to date with all methods but should not give an error
  - you need to execute the plotting function manually to get the plots
 
**Next steps planned**
- implement and check if the managerial versions for Quantile regression and Conformal prediction work
- employ the real-world datasets (by now, everything is working with apparelTrans)
- enhance the literature review
- correct the plotting and benchmarking function