[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/buLEa3MS)
# Data Skills 2 - R
## Fall Quarter 2024

## Homework 4
## Due: November 30 before midnight on Gradescope

__Question 1 Big Data (50%):__

From Canvas (Modules -> Problem Sets), download the nursing-home-inspect-data.zip file. This contains unredacted nursing home inspections acquired by [ProPublica](https://projects.propublica.org/nursing-homes/) through a Freedom of Information Act request as part of their investigation on nursing home quality.

1. Unzip the files (this can be done manually or in R) in a folder __outside__ of the one you will upload to Github. Your code should allow the user to input the name of the path where the unzipped CSV files are.
2. Load the CSVs together as one parquet. The code to do this should be generalized so that if the folder of CSVs is updated with more files, the code will be able to handle this. 
3. Then partition the data by state, and write the partitioned dataset to disk as a series of parquets. Again, save this to a folder  __outside__ of the one you will upload to Github. Load the partitioned datasets as a new parquet.
4. Write a function that takes as input a state name, and then times how long it takes to do the following: filter data by state, then create a summary table with the number of deficiencies for each severity level. The function will measure how long it takes to run it using the non-partitioned data loaded from CSV vs. the partitioned data loaded from the parquets you saved in step 3. (Hint: look into how to use Sys.time() to measure how long code takes to run). This function will print the time elapsed with and without partitioning, as well as the difference between the two.
5. Make a scatter plot of how much faster the code runs for partitioned data (e.g. 2x faster, 3x faster) vs. the size of filtered data. Let each state be one point on the scatter plot. The x-axis should be the total number of observations in the dataset after filtering for that state. Is there a relationship with the size of the filtered data? Note that this might take awhile to run! Save the scatter plot as a .png. 
6. Then create a new plot with 3 data series: the y-axis includes the total time without partitioning, total time elapsed with partitioning, and the difference in time between the two.  What is the relationship between the time saved by using parquets and the size of the filtered data? Describe this in your README file.


__Question 2 Python (50%):__ 
You receive a piece of code from a former colleague that is written in Python (program.py). Unfortunately, your team works in R and your former colleague has bad coding style, so the rest of the team can't understand what this code does. Your boss asks you to look at their Python code, figure out what it is doing, and then translate it into R code. 
1. Test the program out in Python. Add comments to the .py code to explain each step and save and upload it as program_commented.py. (Hint: it can be helpful to add in additional print statements to see what the code is doing at each step.)
2. Then, write in the README file a short description of what this code does.
3. Then, write a piece of R code that does the same thing as this Python code. Make sure that unlike your colleague, you use good coding style (i.e., useful function and variable names) so that future users can understand the code more easily.
