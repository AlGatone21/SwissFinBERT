# Swiss Stock Market Sentiment Analysis

This repository contains the code and resources used for the Master's thesis titled **"Can Large Language Models (LLMs) predict the Swiss Stock Market?"** by Alessio Gattoni, submitted for the Master Degree in Banking and Finance (MBF) at the University of St. Gallen.

## Overview

This project explores the potential of Large Language Models (LLMs) to predict the Swiss stock market by constructing sentiment indices from financial news articles. The study validates three main hypotheses related to the correlation between sentiment indices and stock returns, and compares the performance of smaller, fine-tuned models versus larger, general-purpose models in financial sentiment analysis.

## Contents

- **Data Scraping**: Scripts used to scrape financial news articles from various Swiss news sources.
- **Data Labeling**: Code for labeling the sentiment of news articles using different LLMs.
- **Model Fine-Tuning**: Scripts for fine-tuning the SwissFinBERT model on a dataset of Swiss financial news.
- **Regression Analysis**: Scripts for performing multivariate OLS and panel regressions to analyze the relationship between sentiment indices and stock returns.
- **Backtesting**: Code for backtesting a trading strategy based on the sentiment indices.

## Important Note

The texts of the articles used in this project will not be made public due to copyright reasons. Only the code and the methodology are shared in this repository.

## Contact

For any questions or further information, please contact Alessio Gattoni at alessio.gattoni@student.unisg.ch.
