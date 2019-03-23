# Global_Trade_Alert

## Tasks
Based on the GTA dataset linked to above, please go through the following exercises.
As a general rule, please only process data inside R. Do not do any preparatory work on downloaded data in Excel or some other software before importing it into R.
Note that some instructions are intentionally vague. The data requests we receive are often not clearly formulated and thus demand independent analytical thinking before you start your code. There is not necessarily a single right answer to a vague request, so you'll have to choose a defensible one.

1) Create a line plot with four lines that depicts the number of interventions announced each year between 2009 and 2018. One line for each GTA evaluation (red, amber, green) plus one for their total. Use the colours red, amber and green for the evaluation lines and black for the total.
A) save as png.
B) save underlying data into an xlsx with two sheets. One sheet contains only the total. The other contains all individual evaluations. Name the sheets sensibly. Present the data on the sheets with one column per year and one row per evaluation.
C) Write a code that creates, labels and stores individual copies of the above png for each G20 member (i.e. the number of interventions per year implemented by G20 member X).

2) Create a scatter plot illustrating the relationship between the number of interventions per implementing country and its share of 2008 world GDP. Use WDI data for GDP (there is an R package that allows you to import WDI data). Label & choose axis length so a reader does not need any outside information. Include an exponential trend line in the graph. Save as png.

3) Please use the data of the CPB World Trade Monitor to produce an informative chart about the volume dynamics in world trade since the first crisis-related G20 summit held in November 2008. You may pre-format the relevant CPB data before you import it into R (e.g. copying an otherwise unchanged piece of one sheet into a CSV). You may not manipulate the data before importing (e.g. you may not re-base the data series in Excel). Please choose a suitable index value for your graph. Although the focus is to discern the dynamics of volume growth since November 2008, the graph may include data prior to that date.
