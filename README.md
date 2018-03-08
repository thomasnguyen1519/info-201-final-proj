# info-201-final-project

## Educational Institution Evaluation
**This is the repository for group AB3 (Mario, Tom, Johnny) of INFO201 Winter 2018.**
The purpose of this web application is to help the parents, students, and the government evaluate the colleges in each US state given their generalized intentions. For example, parents and students may use this web app to evaluate potential schools. Some generalized factors that they may consider are the tuition, the cost of living, the financial support, the earning after graduation. The US government may have a different use case, as explainedd later. By going through this app, clients will gain a basic idea about university education from a quantitative perspective. In particular, the intended goal of the application was to answer the following questions:

- What is the cost effectiveness of the university within each state (for the students)?
- What is the satisfaction of the degree amongst the students of the universities?
- What is the cost effectiveness of the university within each state (for government)?
- Should there be an evenness of the education resources when distribution within each state?

Data used in this web application comes from the the US government's public, open datasets. It contains a multitude of variables describing various statistics on the vast demographic of the collgiate population. The data categories ranged from "Number of students not working and not enrolled 7 years after entry" to "Religous affiliation of the institution". We only used what varibles that we believed were significant and necessary to answering our target questions. Therefore, we only used a select few of the variables from the dataset.

## Special Instructions
- Explanations about the app are located in the app itself or the presentation should be self-explanatory
- Run 'test.R' before you render the app. You need to populate the R workspace environment with the necessary variables for the Shiny UI and server to be able to render them.
- GitHub will not host more than 100MB of data for a project so you will need to download the dataset and extract MERGED2013_14_PP.csv into the CollegeScorecard_Raw_Data folder

## Sources of Data
- Dataset: https://catalog.data.gov/dataset/college-scorecard/resource/2a7f670e-0799-436a-9394-df0a9b3ba7c5
- Dictionary for the Dataset: https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx
