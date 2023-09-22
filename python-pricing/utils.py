# import libraries
import numpy as np

# stack the dekads together for the overlapping case
numDekads = 36
stackedDekads = np.array(range(1, numDekads+1), dtype=int) # create an array of the index
stackedDekads = np.tile(stackedDekads, 2).astype(int) # repeat the entire array twice

# Map dekads to their respective months
dekadMonthMap = np.repeat(range(1, 13), 3)
dekadMonthMap = {dekad: month for dekad, month in zip(range(1, 37), dekadMonthMap)}