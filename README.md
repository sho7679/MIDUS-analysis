# MIDUS-analysis
Analysis of relationships between mid-life variables and depression in MIDUS (MacArthur Study of Successful Midlife Development)

Depression is a serious mental illness. As a healthcare worker, I have met many people suffering from depression for a wide range of reasons. To anticipate signs of depression for preventative treatment, I decided to analyze relationships between variables in mid-life development and Center for Epidemiological Studies Depression Inventory (CES-D) score, a measure of depressive symptoms.  Scores of 16+ are subthreshold depressive symptoms, so scores can be dichotomized into “depressive symptoms” and “no significance”. What is the relationship between CES-D category and level of self-rated health? Self-perception of health is indicative of depression since you are aware of your own symptoms: I expect individuals with positive ratings to have “no significance” scores. What is the relationship between CES-D score and whether or not individuals make sarcastic comments? Sarcasm can be perceived as hostility disguised by humor - an indicator of depression. Frequency of comments can be divided into “no sarcasm” and “some sarcasm”, so I expect “no sarcasm” individuals to have lower CES-D scores. What is the relationship between CES-D score and household income? Lower incomes indicate a lower standard of living, so poorer individuals should have an increased risk of depression and higher CES-D scores than middle and high income individuals. What is the relationship between CES-D score and Pittsburgh Sleep Quality Index (PSQI) score? High PSQI scores indicate a lower sleep quality, which is a symptom of depression. Therefore, I expect higher PSQI scores to have higher CES-D scores.

The dataset I used  is from The MacArthur Study of Successful Midlife Development (MIDUS) – a national sampling of American adults for data on multiple variables in adult life. I analyzed CES-D score in terms of household income, self-rated health, PSQI score, and frequency of sarcasm. I recoded missing values for household income (9999998), self-rated health (7), and PSQI score (98) to NA, then removed from the dataset. I created a new sarcasm variable with values containing the level name, then dichotomized it into “no sarcasm” (containing “almost never”) and “some sarcasm” (containing all other levels). I factored household income into “low”, “middle”, and “high” using data from Pew Research: <$42,00=low, $42,000-$125,000=middle, and >$125,000=high. I dichotomized CES-D score into “no significance” and “depressive symptoms” based on the cutoff score of 16. I ran a Fisher’s test to find association between CES-D category and self-rated health since the cell counts were <5. I used a 2 sample t test with equal variance to test for significant difference in CES-D scores between “no sarcasm” and “some sarcasm”. I used an ANOVA test to find significant differences between CES-D scores of income levels, then a Tukey test to find which pair of means were significantly different. I used a linear regression and correlation test to find the strength of relationship between CES-D and PSQI score, then graphed residuals to check constant variance. I used a 0.05 level of significance for all tests. I graphed the 4 variables against CES-D score or category in a bar plot, two boxplots, and scatter plot, respectively. 

Results showed a significant association between CES-D category and self-rated health (p=0.0005). See Table 2 in Appendix A. There are significantly higher CES-D scores for “some sarcasm” than “no sarcasm” (t = -4.7248, p<0.0001). See Table 3 in Appendix A. Middle income individuals have significantly lower CES-D scores than low income (p=0.02). See Table 4 in Appendix A. There is a significant positive linear relationship between CES-D and PSQI score: y=1.81+1.05x (p<0.001). The correlation coefficient is 0.467, which is moderately strong. See Table 5 in Appendix A.

Self-rated health and PSQI score are related since improved quality of sleep leads to positive perception of health. This is supported by my results since both variables have statistically significant associations with CES-D score: lower PSQI scores and positive health ratings are associated with lower CES-D scores. These hypotheses were supported, but are not practically significant since I used Fisher’s Test: the probability of Type I error is decreased, which decreases the test’s power. The assumptions for linear regression were violated: the variance of CES-D scores increased as PSQI score increased. The residuals did not follow a perfectly linear relationship and the correlation was only moderately strong. The relationship between sarcasm and CES-D score is not practically significant because “no sarcasm” only included one level whereas “some sarcasm” included four, and both group histograms were heavily right-skewed. The difference between middle and low income CES-D scores is practically significant since the confidence interval is very narrow. There is no significant difference between high and low income, implying that the very rich and very poor are more depressed than those with middle income. But, the assumption of normality in each group is not satisfied because they are all right-skewed. The study was limited because observations were limited to Americans with access to a phone and may not be independent because siblings were sampled. My results also cannot be extrapolated to other countries since only Americans were sampled.
