# Reproductive skew can affect social information use

Here, we publish the soruce to the simulation model for our manuscript "Reproductive skew affects social information use"

The code in `base.r` starts with a sequence that allows running the code locally or on a computer cluster. 

When running locally it is necessary to change the working directory in line 40 to the path of the methods folder. 

The methods folder has all function descriptions that will be called from the main function (`run()`) of the model. 

The `run()` function requires simulation parameters, which are defined in the setup data frame. The run function uses these to setup each individual simulation, runs the model algorithm, records results, and returns these results in a summarised fashion. 
