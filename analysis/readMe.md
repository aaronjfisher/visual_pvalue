Files
----------------
* analyze_pvalue_data.Rmd contains code for our analysis
* coursera_user_responses_tidy.csv contains all user responses to the survey (see code book below)
* data_for_1plots_coursera.RData contains objects used in creating the plots shown to users.


Code Book for coursera_user_responses_tidy.csv
----------------

Each row of the csv corresponds to one question shown to a user, with:

* id = An arbitrary but unique identifier for the user
	* Note, 12 of the 2051 users who looked at the quiz decided not to submit any responses. The id values here range from 1 to 2039, all of which actually submitted responses.
* time = The time the user submitted the question. All question in a quiz have to be submitted at once, so these times will be the same for all the questions a submitted on the same attempt of the survey.
* style = The presentation style of the plot. n35, n100ref, and n200 are standard plots of 35, 100 and 200 data points. X coordinates were sampled from a standard normal distribution. For each sample, 10 extra datapoints were generated, and the 5 most negative and most positive X values were trimmed off.
* styleNum = Essentially an integer version of style, for internal use. 1 for n35, 2 & 3 both refer to n100ref (with 2 denoting significant plots and 3 denoting nonsignificant plots), 4 is for n200, 5 is for bestFit, 6 is for axesScale, 7 is for axesLabel, 8 is for outlier, and 9 is for lowess.
* trueSig = TRUE if and only if the p-value for the plots is <.05.
* datVer = For each combination of "trueSig" and "style" we made five plots. User accuracy in classifying the significance of these five plots should theoretically be very similar. The "datVer" variable corresponds to which of these 5 plots the user was shown.
* pval = p-value based on significance test of the slope coefficient in a linear regression.
* attemptNum = Some users did the quiz more than once. These users were shown a new set of randomly generated questions on each survey attempt. Due to the limited library of questions, some of these questions which could be ones that users have already seen (see firstTry). 
* firstTry = On repeated attempts of the quiz, it was possible that users could have see the exact same question twice (i.e., the same values for style, trueSig, and datVer). The firstTry variable is true if and only if the user had not already submitted a response for this survey question before. Approximately 92.4% of responses submitted were for first attempts of the survey.
* guessSig = TRUE if the user thought the plot was significant.
* correct  = TRUE when guessSig is equal to trueSig.
* spec = Equal to the "correct" variable (above) if the relation was not significant, NA otherwise.
* sense = Equal to the "correct" variable (above) if the relation was significant, NA otherwise.
* qNameCoursera = An single identifier that uniquely identifies the question, combining the information of "style," "trueSig," and "datVer". This identifier was built by the Coursera system, and ranges from Q-0 to Q-79.


Code Book for data_for_1plots_coursera.RData
----------------
This RData file contains eight R objects describing the library of plots that could be shown to users.

* `nreps` = 80, the size of the library.
* `nes` = a vector of length 80, telling the sample size used for each plot in the library.
* `pres` = a vector of length 80, telling the category of each plot. The naming convention is the same as in the style variable, in the "coursera_user_responses.csv" file.
* `pvals` = a vector of length 80, telling the p-value of each plot.
* `tvals` = a vector of length 80, telling the t-statistic of each plot.
The remaining two objects are for internal use.
* `xes` = a matrix of size (80 by 200). The rows contain the X coordinates for points shown in each plot of the library. In each row, there will be several NAs due to the fact that not all plots have the same sample size. For example, `xes[2,]` will have `nes[2]` non-NA values, and `200-nes[2]` NA values.
* `yes` = a matrix of size (80 by 200). The rows contain the Y coordinates for points shown in each plot of the library. In each row, there will be several NAs due to the fact that not all plots have the same sample size.
* `pbins` = for internal use. This variable is essentially a binned version of the `pres` object.


Notes
----------------
* If a user did not finish an attempt of the exercise set, we generally don't have a record of what plots they saw, but did not submit. These missing plots are included as in this dataset as rows with "NA" for the appropriate columns.
	* We have responses for 98.7% of the plots shown to users, not counting 12 users who looked at the quiz but did not fill out any of the responses. These 12 users are not included in our dataset.



Summary Statistics
----------------
* 2,039 users submitted responses.
* 19,593 responses were submitted, overall.
* About 94.4% of users finished their first attempt of the quiz.
* About 7.6% of submitted responses were for answers on the second attempt of the survey. About 1.72% were from the third attempt. The highest number of exercise set submissions for one user was 12.
