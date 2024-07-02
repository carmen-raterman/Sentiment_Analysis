# Sentiment Analysis Project

This project performs sentiment analysis on literary texts using R and tidyverse. My analysis includes tokenization, word frequency analysis, sentiment scoring, and visualization of results using various lexicons and word clouds.

## Overview

The main steps of the analysis are:

1. **Loading Required Libraries**: Utilize libraries such as `tidyverse`, `tidytext`, `janeaustenr`, `gutenbergr`, and others.
2. **Text Tokenization**: Convert text data into tokens for analysis.
3. **Removing Stop Words**: Eliminate common filler words from the analysis.
4. **Word Frequency Analysis**: Count and visualize the most common words.
5. **Downloading Additional Books**: Fetch additional texts from Project Gutenberg.
6. **Comparing Word Frequencies**: Compare word usage across different authors.
7. **Sentiment Analysis**: Analyze sentiment using various sentiment lexicons.
8. **Visualization**: Plot results using `ggplot2` and create word clouds.

## Project Structure

- **Tokenization and Tidy Data**:
  - Tokenize text and convert it to tidy data format.
  - Example with Emily Dickinson's poem "Because I Could Not Stop for Death".

- **Analyzing Jane Austen's Books**:
  - Tokenize and clean the text from Jane Austen's novels.
  - Remove stop words and count word frequencies.
  - Visualize the most common words.

- **Comparing Authors**:
  - Download texts from H.G. Wells and the Brontë Sisters.
  - Compare word frequencies across these authors and Jane Austen.
  - Plot the results using `ggplot2`.

- **Sentiment Analysis**:
  - Use sentiment lexicons (AFINN, Bing, NRC) to score sentiments in the texts.
  - Analyze sentiment trajectories across novels.
  - Compare sentiment analysis results using different lexicons.

- **Advanced Text Analysis**:
  - Create word clouds to visualize common words.
  - Explore bigrams and trigrams for more complex phrase analysis.
