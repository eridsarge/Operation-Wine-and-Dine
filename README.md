# Operation-Wine-and-Dine

"This white goes great...with this red" was the level of sophistication I had during my most recent wine tasting with a group of friends.  Feeling as though it was time to become more cultured, I set out on this project to explore the chemical make up of different red wines to hopefully understand what makes some wines taste better than others.

My goals for this project were the following:
1) Hand on data exploration experience
2) Data Visualization practice and experimentation
3) Construct and tune a predictive model to help with wine selection

R packages used:
1) ggplot2
2) dplyr
3) tidyr
4) GGally

Hypothesis: There is an optimal combination of chemicals in red wines that can be used to predict whether a wine will taste better or worse than others

These variables were used in testing the hypothesis:
1) Alcohol
2) Suphates
3) pH
4) Density
5) Total Sulfur Dioxide 
6) Free Sulfur Dioxide
7) Chlorides
8) Residual Sugar
9) Citric Acid
10) Volatile Acidity
11) Fixed Acidity

Testing Methodology: Classified Wines based on quality as being either "Good" or "Bad" wines.  Wines with quality of 6 or higher were considered good, while 5 or lower as bad.  Then, created Binary logistic regression models in an effort to predict quality.

Project Outcome: Final Logistic Regression Model performed with over 70% accuracy in predicting wine quality.  Using regression coefficiencts, also learned that quality can be attributed to the following characteristics:
1) Less acidic wine
2) Wines with fewer chlorides (research tells me that high chlorides can cause a salty taste)
3) Lower sulfur dioxide (some people can be allergic, including my aunt)
4) Higher amounts of sulphates
5) Higher Alcohol content, more booze means better quality

cheers!
