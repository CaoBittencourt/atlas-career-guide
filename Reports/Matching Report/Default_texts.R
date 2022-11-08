# -------- DEFAULT TEXTS FOR IMPUTATION --------------------------------------------------
# INTRODUCTION ------------------------------------------------------------
chr_text.intro <- "Hello, ___! Welcome to your professional matching report! In this document, you'll find solidly researched information on your compatibility with ___ different career paths.
\nOur state-of-the-art psychometric models and questionnaires were developed using publicly available data from the Bureau of Labor Statistics (BLS) and the Occupational Information Network (ONET). This database consists in a set of 161 job characteristics, such as entry level of education, required skills, abilities and other competencies (rated from 0 to 1), as well as typical job activities, job hazards, and so on.
\nOne of our goals at Atlas Research is to determine the career that best fits your professional profile. Therefore, we assess your most important competencies and job preferences, and compare these with the ___ occupations we've gathered from the BLS and ONET. Then, we arrange your professional matches best to worst and estimate a compatibility score as a percentage, indicating your similarity to each occupation.
\nDon't forget to register at https://www.go2atlas.com/ to view more detailed reports and gain access to our Mentorship Program. We hope you obtain value from this report. Have a great day!"

# COMMENT ON CIRCULAR BAR PLOT --------------------------------------------
chr_text.circular_plot <- "The figure above is a circular bar chart of professional compatibility scores, where each bar represents the percentage of similarity between your unique set of competencies and one of ___ occupations.
The compatibility metric ranges from 0 to 100%, as shown on the vertical axis in the middle of the graph.
Your recommended occupations are highlighted in purple, and those with lower compatibility in grey.
As this report aims to provide only a brief overview of your professional profile, we will not go into too much detail about specific career matches. 
See Table 1 for a sample of your best and worst matches."

# COMMENT ON TOP AND BOTTOM MATCHES TABLE ------------------------------
# chr_text.topbot.table <- "Having understood this, firstly we note that your best match is \"___\", with ___% compatibility; and the worst is \"___\", with ___%.
chr_text.topbot.table <- "Considering this, firstly we note that your best match is \"___\", with ___% compatibility; and the worst is \"___\", with ___%.
The median match is \"___\", with ___% compatibility, below which lies 50% of careers.
Thus, it is apparent that the scope of your recommended activities is ___, covering around ___ occupations."

# COMMENT ON PROFESSIONAL COMPATIBILITY CURVE (LINE CHART) ---------------------------------------------------
chr_text.compatibility.curve.intro <- "The same information can be conveyed with a line chart, like the one in Figure 2 below.
This diagram features, on the horizontal axis, your career matches ranked lowest to highest based on the respective similarity coefficients (on the vertical axis).
Therefore, each point that constitutes this line chart is your matching percentage with a given career.
And because connecting all the points on the graph yields a progression of professional compatibility, this resulting line is called the *professional compatibility curve*.
Again, the recommended occupations are highlighted in purple, and the unhighlighted ones (in grey) are not recommend, at least not in terms of basic similarity."

chr_text.compatibility.curve <- "Now, one important thing to comment on is the shape of your professional compatibility curve.
For the slope of the curve is an indirect measure of how spread apart is your professional profile.
If the curve is flat, then all values are identical.
But, in contrast, as the angle of the line increases to 45 degrees (as in the dashed black line), all observations become evenly spaced out and well distributed.
And since the distribution in question concerns the viability of different types of employment options, 
the slope of the professional compatibility curve, in conjunction with the range of recommended occupations, 
is a strong indicator of the broadness of your professional profile."

# COMMENT ON PROFESSIONAL COMPATIBILITY DISTRIBUTION ------------------------------------------------
chr_text.distribution.intro <- "It is straightforward to understand the above-mentioned distribution of viable careers by looking at a histogram of professional compatibility scores (Figure 3).
A histogram is a data visualization tool which enables us to clearly see the dispersion of a given variable. It works by segmenting data into a desired number of intervals called bins, denoted by the columns on the graph.
The height of each column is proportional to the frequency of data points within its bin. Hence, the tallest columns represent the most frequent intervals or classes of data, and as the columns decrease in height, so too the data points in a bin decrease in frequency."

chr_text.distribution <- "Figure 3 also displays an overlaid density curve of compatibility scores. 
A density plot is analogous to a histogram, and likewise it is used to evaluate a variables's dispersion and centrality.
However, where histogram bins are discrete in nature (they are *fixed* intervals of numeric data), densities are smooth, continuous and not limited by discrete bins.
The vertical axis is omitted for simplicity's sake, as densities are dimensionless and not interpretable in themselves.
\nAt any rate, it is now even more evident how your professional profile is distributed, as the outline of Figure 3 makes visible several key aspects of your overall employability.
For instance, higher variance of similarity scores makes the histogram and density to be shorter in height, but horizontally larger.
Less varied, or specialized, profiles, on the other hand, produce slimmer and taller histograms and densities.
The centrality of the distribution is very important as well: if compatibility scores concentrate at the lower end of the scale, 
then most occupations are a poor match and viable career paths will be limited; but if the distribution is centered toward the right end of the scale,
then the opposite is true. It bears mentioning that although graphically the 50% mark appears to be \"the middle\" of the scale,
this analysis is related to the distribution of *recommended* occupations, which is itself centered further to the right. 
\nStatistically, we can assess how dispersed is the compatibility curve by calculating the variance of your compatibility scores.
In addition, the skewness of the curve tells us if your profile is restricted to a few niche occupations or if it is less defined.
By doing these calculations, we find you have ___ matching percentages across all occupations.
This means that, after accounting for the variance of compatibility scores, ___."

# COMMENT ON CATEGORIES AND FACTORS  ----------------------------------------------------
chr_text.factors.intro <- "We now move to an analysis of your top and bottom career matches. 
However, before this can be done, we must clarify some terminology."

# chr_text.factors1 <- "The most important thing to understand are categories and factors.
chr_text.factors1 <- "Our psychometric questionnaire for professional profiling is based upon ___ different categories: ___. 
By this we mean that any given item in the questionnaire is associated with one of ___ \"very general types\" of attributes, or categories.
\nThese categories, in turn, are divided into individual factors.
In psychometrics, factor analysis is a technique to group variables according to how they correlate to one another.
These groups of variables are called factors, and all items in our professional profiling questionnaire have been empirically assigned to one of ___ factors.
Thus, categories are divided into factors, which are themselves divided into items.
This is how the Atlas Professional Profiling Questionnaire is organized."

chr_text.factors2 <- "Since explaining the intricate statistical procedures employed in the construction of our psychometric models is not the main focus of this brief report,
we limit ourselves to listing the categories of analysis, with their respective factors.
Firstly, the \"___\" category is composed of ___ factors: ___. 
Secondly, the \"___\" category splits into: ___. 
And finally, the category \"___\" contains ___ factors: ___.
These are the categories and factors covered by the ___ items in the model."

# COMMENT ON TOP MATCH -------------------------------------------------
chr_text.top.intro <- "Having understood this, we take a look at how your own professional profile compares to that of the occupation with which you're most similar:"

chr_text.top <- "Figure 4 gives us a glimpse of your professional compatibility with \"___\", your best career match.
Here, we immediately see that you're most similar with respect to the ___ factor___, and most dissimilar with respect to ___,
the differences comprised within ___ and ___ percentage points.
We also observe that you're underqualified for exercizing this occupation in ___ factor___, and overqualified in ___.
Lastly, your three biggest strengths are ___, while those of \"___\" are ___."

# COMMENT ON BOTTOM MATCH -------------------------------------------------
chr_text.bot.intro <- "Finally, the figure below features an analysis of your least compatible occupation, \"___\":"

chr_text.bot <- "Again, it is immediate to notice that the ___ factor___ ___ the highest point___ of similarity,
while the lowest ___ ___.
In this case, the differences range from ___ to ___ percentage points.
In terms of your current capacity to exercize this profession,
we assess you to be underqualified in ___ factor___, and overqualified in ___.
The three biggest strengths of \"___\" are ___, which have ___ in common with yours."

# FINISHING REMARKS --------------------------------------------------------------
chr_finishing.remarks <- "In this report, we went over your results in the Atlas Professional Profiling Questionnaire.
We began by examining a ranking of occupations, based on similarity of job-related attributes,
and underscoring the ___ career paths that constitute your ___ scope of recommended occupations. 
After that, we looked at the professional compatibility curve and its shape, 
also indicative of your degree of specialization. 
Investigating further the distribution of compatibility scores,
we found that you have ___ professional profile, meaning ___.
At last, we briefly analyzed your best and worst career matches, respectively: \"___\", and \"___\".
Concerning the first of these, you seem to be ___. 
And as regards the second, ___.
\nHaving considered all of this, we now conclude your career matching report.
Again, this is but a brief overview of your professional profile, 
and you can access much more detailed, in-depth reports
by registering at https://www.go2atlas.com/ today.
There, we offer a suite of varied career support programs,
including highly qualified curated mentorships, career roadmaps, and more.
Thank you for your time, ___. The Atlas Team wishes you well. 
May you find your best professional future ahead!"




