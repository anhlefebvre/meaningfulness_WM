# Effect of Meaningfulness on Working Memory

I used LabJS for experiment setup and R language to analyse data for the two experiments, which investigated the effect of semantic meaning on binding and item memory. 

Before starting the analysis, make sure there is a folder `Exp{number_of experiment}/Exp{number_of experiment}_data/data_raw.txt` in the working directory.

### Instruction

**Experiment setup**
1. Choose the corresponding experiment folder
2. Open Exp*_setup folder and make sure there is a .json file
3. Open https://labjs.felixhenninger.com/
4. Click on the dropdown arrow next to the disk symbol and select Open
5. Select the file .json in step 2 and click Run 


**Data analysis**
1. Choose the corresponding experiment folder
2. Run `data_preprocesisng.R` to check participants that violated exclusion criteria
3. Run `main.R` to calculate accuracy of participants' performance and to estimate parameters of M3 
4. Run `plot_results.R` to print out figures related to accuracy, selected reponse types, posterior distribution and M3 estimates