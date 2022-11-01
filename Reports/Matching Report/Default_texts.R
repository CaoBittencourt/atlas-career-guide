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
The slope of the curve is an indirect measure of how spread apart is your professional profile.
For if the curve is flat, then all values are identical.
But, in contrast, as the angle of the line increases to 45 degrees (as in the dashed black line), all observations become evenly spaced out and well distributed.
And since the distribution in question concerns the viability of different types of employment options, 
the slope of the professional compatibility curve, in conjunction with the range of recommended occupations, 
is a strong indicator of how broad or specialized is your professional profile."

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
In addition, the skewness of the curve tells us if your profile is more restricted to a few niche occupations or if it is less defined.
By doing these calculations, we find you have ___ matching percentages across all occupations.
This means ___."

# COMMENT ON TOP MATCH ----------------------------------------------------
# COMMENT ON BOTTOM MATCH -------------------------------------------------
# FINISHING REMARKS --------------------------------------------------------------