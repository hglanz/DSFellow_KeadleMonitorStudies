# DSFellow_KeadleMonitorStudies

## Readings and Resources

* https://en.wikipedia.org/wiki/Hidden_Markov_model

* https://www.cs.toronto.edu/~hinton/csc2515/notes/lec9timeseries.pdf

* https://www.math.unl.edu/~sdunbar1/ProbabilityTheory/Lessons/HiddenMarkovModels/Examples/examples.html

* https://towardsdatascience.com/markov-and-hidden-markov-model-3eec42298d75

## Goals/Tasks

1. Learn about and understand Hidden Markov Models
2. Reproduce at least one HMM example from the readings above.
* Specify the model, including parameter values.
* Simulate data from this model.
* Estimate the parameters of the model based only on the data.
* Assess closeness to actual parameter values.

3. Draw the diagram for and explain how the wearable monitor data can be modeled with an HMM.
4. Re-do 2. in the context of fake wearable monitor situation. 

* Specify a model that resembles (possibly in a much more simplified way) our wearable monitor situation, including model parameter values.
* Simulate data from this model.
* Estimate the parameters of the model based on the data.
* Assess closeness to actual parameter values.

5. Apply HMMs to the actual wearable monitor data.
6. Compare HMM model results and performance to the performance of other models.
7. Write up work and results.


## Files and Folders in this Repository

### Folders

CANNOLI-Master - Previous work from 2019 conducted by James Abundis (GitHub: jameslabundis), Jesse Bao (GitHub: [unknown]), and Natalie Keelan (GitHub: nataliepkeelan).
Original Repository: https://github.com/jameslabundis/CANOLLI

Capstone2020 - Previous work from 2020 conducted by Shobhit Asthana (GitHub: shobhitasthana), Austin Schwarz (GitHub: [unknown]), and Roy Zawadzki (GitHub: royzawadzki).
Original Repository: [unknown]

PipelineWork - Previous work from 2020 conducted by Jack Langston (GitHub: jlangston99). This repository also contains the groundtruth file, located within the "processed" folder.
Original Repository: https://github.com/jlangston99/KPHMonitor

Practice - Experimentation with Hidden Markov Models using practice data. This folder provides some insight into the early stages of the project, but it is unrelated to any work conducted on the heart monitor data itself.

### Files:

#### All files with the .ipynb extention should be run in the following order:

If your are running any of these packages for the first time (pandas, glob, etc.) then run this line for each package that has never been used before:
pip install [library] (i.e. pip install pandas)

Aggregation.ipynb - The first file to run while reproducing the data. This file takes in data downloaded directly from the data shared by Dr. Keadle (datasci-data). This file will require you to manually run the first chunk exactly once (all code before importing glob) in order to create the output directories. Additionally, this file will also require you to change the "path" variable to the location of the data on your computer (this variable is currently set to "E:/Data/Monitor Data/"). This path should contain the directories final_wrist_raw_csv, hip, and BioStampRC.
* If these two steps are followed, then this file should be able to run from start to finish without issues.
* This file will take a long time to run (one hour, give or take a half hour in either direction).
* The data produced by this file will be saved to your computer in the directories created in the first chunk. These directories will be present in the same directory as Aggregation.ipynb
* It will take a lot of memory to save this data on your computer, so you may want to store it on an external device or delete it when you are finished with the preprocessing, as the preprocessing step further filters and saves the data.

Wrist and Hip Filter.ipynb - Brandon write this part, unless if we merge this file with preprocessing then delete this part

preprocessing.ipynb - 
