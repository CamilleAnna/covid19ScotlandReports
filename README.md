covid19ScotlandReports
Codes for automated covid19 Scotland &amp; UK AG briefing

~~~~~~~~~~~~~~~~~~~#
     READ ME       #
AG BRIEFING REPORT # 
~~~~~~~~~~~~~~~~~~~#

 ~~~~~~~~~~~~~~~~~~~#
    SCRIPTS:       #
~~~~~~~~~~~~~~~~~~~#

1) ./covid19/AG_briefing/AG_briefing_report_analysis_script.R:
# gets the data and runs the actual analyses (computing doubling time and doing bootstrap simulations (poisson error))
# Two outputs, in AG_briefing/output/
# AG_briefing_analysis_output_2020-XX-XX.RData --> RData object to use in the RMArkDown doc or to do any figure work in an independent R script (just load the object)
# Td_report_2020-XX-XX_t1.2020-XX-XX_t2.2020-XX-XX.csv --> csv of the Dt + 95%CI, for records


2) ./covid19/script/sourced_functions_doublingTime_reports.R
# sourced in both the 'analysis' script and the 'Rmarkdown' script. Contains:
# (i) Loaded packages
# (ii) some ggplot themes
# (iii) FUNCTIONS:
# data.cleaner() : used to smooth the scotland healthboard data that have negative datapoints. Takes a dataframe (date, numNewCases) as argument
# compute.td.m1.v2() : actually computes the doubling time. Takes a dataframe (date, cumNumCases) as argument (a bit annoying that the variables HAS TO BE NAMED 'cumNumCases', e.g. can be confusing when having death data to call the cumulative number of deaths 'cumNumCases',. This is a feature to be improved)
# sim.epi() : simulates epidemic curve to generate poisson error. Takes a dataframe (date, cumNumCases) as argument. Spits out that same dataframe with the number of observed new cases appended to it, observed ('numNewCases') and simulated ('V' variables). There will be as many 'V' columns as iteration of the bootstrap chosen, which is set by the argument 'its'. 
# Td.lapply() : simply applies the compute.td.m1.v2() over a list output. Convenient for shortening code at various points.

3) AG_briefing_report: Tha actual Rmarkdown doc.
# This sources the script (2) and loads the RData object output from script (1)
# Then mostly performs plotting commands within each figure chunk.
# The first chuck ('setup') Has MANY figures settings parameters. Most of the edit to do are done there. It's all about tweaking those in order to best display the data as the scale change over time.


# ~~~~~~~~~~~~~~~~~~~#
#    OTHER  FILES    #
# ~~~~~~~~~~~~~~~~~~~#

/covid19/AG_briefing/data: one folder per day.

Each contain (i) UK data from that Feifei uploads on the dropbox everyday (ii) the daily report that Giles sends, (iii) an edited version of that daily report ... the format of data has changed overtime and I decided to stick to my initial script, so I manually adjust the excel file everytime. Not ideal. To be improved.
Manual edits are:
- on all sheets: the last two days don't read well in R for some reason. Edit them to set them as 'text'
- sheet 1: remove the 0, 1 header
- sheet 2: remove the numbered header, change 'Health_boards' to 'date', remove the last two rows (Increase, pincrease)
- sheet 3: not using this one, start from raw cases numbers
- sheet 4: remove coma after 'Grampian', remove 'Dates'

/covid19/AG_briefing/input_files/popsizes_grouped.xlsx

Populations sizes used to compute incidence per 10k pop. in script


# ~~~~~~~~~~~~~~~~~~~~~~#
#  PRODUCING THE REPORT #
# ~~~~~~~~~~~~~~~~~~~~~~#

1) Get the UK and Scotland data, save in the 'data' folder, do the manual edits on the Scotland data and save under 'reviewed' (see as in previous days)
2) Open AG_briefing_analysis_script.R:
	- make sure the paths to data are correct
	- set the variable 'today' so that it matches the data you want to analyse
	- set the t1 and t2 variables to the dates you want to compute Td over. By default, t2 = today, t1 = t1 - 7
	- set 'its' to 1000 ... or less to run a quick test!
	- the set.seed(today) ensures that the bootstrap are replicable
	- run the script --> It should output the .RData and the .csv files in ./output
3) Open AG_briefing_report.Rmd in RStudio
	- Adjust title, date, etc in header
	- set output to 'pdf_document' or 'word_document' depending on desired output
	- ensure paths are correct
	- set the 'today' variable to match the date of the data to be analysed
	- Click 'knit' --> This will output the document in the same folder as the .Rmd script.
	- Check out the output, very likely, you'll need to adjust the figures parameters
	- go back the the Rmd file, tweak parameters: very general ones (font, axis text size etc) are in the first code chunk
	- figure specific parameters (basically x and y limits and tick marks) are within each respective code chunk



Usually, figure specific settings are (1) the interval between each tick mark and (ii) the range/limits
axti = axis ticks interval --> yaxti.f1 = y-axis ticks interval for figure 1, etc.
Example:
yaxti.f1 = 10 : y axix will display every 10 values
xaxti.f1 = 7  : x axis will display every 7 avlues (1 week interval in this case)
yseq = seq(0, round_any(max(d.death.sim.2$cumNumCases), 10, f = ceiling), yaxti.f1) : y axis will be a sequence from zero to the max observed cumNumCase, rounded to upper 10th
xseq = seq.Date(from = min(d.death.sim.2$date), to = max(d.death.sim.2$date), by = xaxti.f1) # x axis (will be a dates sequence from initial to final available time point and will display every xaxti.f1 days (which here is every 7 days)



# ~~~~~~~~~~~~~~~~~~~~~~#
#      TEXT UPDATES     #
# ~~~~~~~~~~~~~~~~~~~~~~#

The main body of the text mostly dynamically fetches the output in the RData file for numbers. So these should be updated automatically. It's worth double checking though.

One sentance not automatically updated: 'This is not significantly different from doubling time for previous 7 days (...)'


# ~~~~~~~~~~~~~~~~~~~~~~#
#     RMARKDOWN TIPS    #
# ~~~~~~~~~~~~~~~~~~~~~~#

&nbsp; : to jump a line, must have an empty line after, ! will only have effect on pdf output. Word output formatting very limited
\newpage : to insert a page break, must have an empty line after, ! will only have effect on pdf output. Word output formatting very limited

