import pandas as pd
import numpy as np
from scipy.stats import zscore

# Sample DataFrame
df = pd.DataFrame({'value': [10, 12, 11, 13, 12, 100, 11, 9, 14]})

# Calculate Z-scores
df['zscore'] = zscore(df['value'])

print(df)
# Filter out outliers (Z-score > 3 or < -3)
df_no_outliers = df[(df['zscore'] > -3) & (df['zscore'] < 3)]

print(df_no_outliers)