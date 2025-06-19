# 📊 CSV Data Analyzer – R Shiny App

An interactive R Shiny application that allows users to upload a CSV file, clean the data, perform exploratory data analysis, clustering, and generate association rules.

---

## 🔍 Features

- ✅ Upload and preview CSV data
- ✅ Remove missing values and duplicates
- ✅ View data structure and summary
- 📈 Generate visualizations:
  - Payment method distribution
  - Spending by age and city
  - Boxplot of total spending
- 🧠 Perform K-Means Clustering
- 📋 Generate Association Rules (via Apriori)

---

## 🛠️ Requirements

Make sure you have R and the following packages installed:

```r
install.packages(c("readr", "dplyr", "shiny", "ggplot2", "arules"))
