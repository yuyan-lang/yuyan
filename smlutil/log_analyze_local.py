import pandas as pd
import sys

import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import matplotlib as mpl

import mplcursors



# mpl.rcParams['font.sans-serif'] = ['Kai']

zhfont = FontProperties(fname='/Users/zc/Downloads/优设标题黑.ttf')

lines = open(sys.argv[1]).readlines()
lines = [(l[1:20], l[21:])  for l in lines if l.startswith("[")]

times = [l[0] for l in lines]
msgs = [l[1]for l in lines]
print(times[:10])
print(msgs[:10])
df = pd.DataFrame({"timestamp":times, "message" : msgs})


df['timestamp'] = pd.to_datetime(df['timestamp'])

# Add a new column for the first 10 characters of the message
df['message_10'] = df['message'].str[:10]

# Sort the dataframe by timestamp
df.sort_values(by='timestamp', inplace=True)

# Add a new column for the time spent on the previous message
df['time_spent'] = df['timestamp'].diff().shift(-1)

# print(df)

# Group the dataframe by the first 10 characters of the message
grouped = df.groupby('message_10')

# Calculate the mean time spent on each message
mean_time_spent = grouped['time_spent'].sum()
# Set the maximum number of rows to display
pd.options.display.max_rows = None

# Set the maximum number of columns to display
pd.options.display.max_columns = None
pd.set_option('display.max_colwidth', 100)

# print(mean_time_spent)
total_time_spent = grouped['time_spent'].sum().reset_index(name='total_time_spent')
total_time_spent = total_time_spent.sort_values(by="total_time_spent", ascending=False)
print(total_time_spent)
# print(df)

# Get the top 10 groups by count
# top_10 = grouped.size().sort_values(ascending=False).head(10)

# # Plot a histogram of the top 10 groups
# top_10.plot(kind='bar')

# # Set the x-axis label
# plt.xlabel('Message (first 10 characters)', fontproperties=zhfont)

# # Set the y-axis label
# plt.ylabel('Frequency')

# # Show the plot
# plt.show()


# mplcursors.cursor()