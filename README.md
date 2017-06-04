# Otto Classification Challenge

This project contains very basic implementations of the classification challenge of Otto in Kaggle, which can be accessed [here](https://www.kaggle.com/c/otto-group-product-classification-challenge).

## Algorithms
Since this started as a class term project, some of the very basic algorithms have been used. The algorithms used and their test scores are in the table below; note that the lower the score the better.

| Algorithm     | Score | Rank  |
| ------------- |------:| -----:|
| Naive Bayes   | 8.27  | 3244 |
| Multinomial Logistic Regression | 0.83 | 2702 |
| Neural Network | 0.60734 | 2320 |
| Decision Tree | 1.1818 | 2798 |
| Random Forest (500 trees) | 0.5584 | 1918 |

We got out best score with the random forest implementation, where we used 500 trees for classification. Note that there are no optimizations or improvements over the implementations, those are just basic examples of each algorithm.

## Usage
Each of these classification methods share the same imaginary interface. All the methods take the training data and the test data as the result, all of them report an error rate to the console, and create a submission file for the challenge. An example usage of the methods is as follows:

```R
# Collect the training data.
trainingData <- fread('data/train.csv')
trainingData$id <- NULL

# Collect the test data.
testData <- fread('data/test.csv')
testData$id <- NULL

# Classify the test data using the Naive-Bayes classifier.
naiveBayesClassifier(trainingData, testData)
```

Examples can be found in `train.R` file, which uses the `classifiers.R` and `helpers.R` files to generate the results.

## License
The project is licensed under MIT License.