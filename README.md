### Project Proposal: Liver Disease Diagnosis and Predictor Development

**Data:** For this project, I will utilize the Indian Liver Patient Dataset (ILPD) sourced from the UCI Machine Learning Repository. The dataset comprises medical records from 2011 to 2022, capturing various liver function tests and demographic details such as age and gender. Key parameters include Total Bilirubin, Direct Bilirubin, Alkaline Phosphatase, Alanine Aminotransferase, Aspartate Aminotransferase, Total Proteins, Albumin, and Albumin and Globulin Ratio, along with a binary selector for liver disease presence.

Link - https://archive.ics.uci.edu/dataset/225/ilpd+indian+liver+patient+dataset

**Target:** The primary objective of this project is to identify significant predictors of liver disease from the dataset and to develop a predictive model that can effectively determine the likelihood of liver disease based on these indicators.

**Process:**
1. **Data Preparation:** Initial data preprocessing to handle missing values, encode categorical variables, and normalize skewed data distributions.
2. **Exploratory Data Analysis (EDA):** Conduct thorough exploratory analysis to visualize and understand data characteristics, relationships between variables, and identify patterns or trends that suggest potential liver disease.
3. **Feature Engineering:** Develop new features based on insights gained during EDA to enhance the model's predictive capability.
4. **Model Training and Validation:** Implement various machine learning models, including Logistic Regression, Random Forest, and Support Vector Machines. Models will be evaluated and fine-tuned using cross-validation to optimize performance.
5. **Development of an R Shiny Application:** Build an interactive application to visualize the EDA results and display model predictions and comparisons dynamically.

**Expected Challenges:**
- Dealing with imbalanced data as liver disease instances might be less frequent.
- Selecting and tuning the appropriate machine learning models to handle potential biases and achieve accurate predictions.
- Ensuring the R Shiny application is user-friendly and effectively communicates the findings to users with varying levels of expertise.

**Presentation Link:** https://uofi.box.com/s/35l3u6p33g8q7ez77l2tgmofhba4t8k9