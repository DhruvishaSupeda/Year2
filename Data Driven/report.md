# OCR assignment report
## Feature Extraction (Max 200 Words)
[For feature extraction, I have used Principal Component Analysis to get 40 dimensions, then taken the first ten features from those. I use the full array of the feature vectors, and work out the covariance of these vectors. I use these covariances to make an eigenvector, which I then store in the model, along with the mean of the full matrix of feature vectors. This is so I do not repeat this stage with the testing data, and I can take the eigenvector and mean from the training stage. I then project the data onto the first 40 principal components. The first ten vectors of this is the 10 dimensional feature vector.]
## Classifier (Max 200 Words)
[To classify the pages, I use k-Nearest Neighbour. Normal nearest neighbour is used to sort the neighbours in terms of how close they are to the feature vector, then the k nearest neighbours are sorted depending on how many of that class appear. The one that appears the most is the one who's class is given to the feature vector, and these labels are returned. I used k-nearest neighbour as it is more accurate than nearest neighbour, and is more likely to classify the feature vector correctly. As the value of k increases, the accuracy of the less noisy pages decreases, and the opposite for the more noisy pages. I used a value of k that would not decrease the percentages on the first pages too much, while increasing the last pages, and have decided to use 10 for k.
I attempted two ways of k-nearest neighbour, and adapted some code on a website (sourced in system.py) to make it work using more of numpy, and increase the percentage correct.]
## Error Correction (Max 200 Words)
[I have tried to implement some based spelling rules to error correct the labels. I have implemented the rules u after q, if there is "ig" the next letter will be n, and certain letters will be doubled after a vowel. This is to stop any misclassification of these letters. I chose these rules as they do not have many exceptions compared to other rules, so it is more likely to be correctly classified. I avoided some rules that have many exceptions, such as "i before e except after c", as there are so many exceptions it would misclassify too many labels to be worth it. Putting all of these rules in did not effect the results in a negative way, so I believe they are useful to keep in. More spelling rules and exceptions could be added to make it more accurate.]
## Performance
The percentage errors (to 1 decimal place) for the development data are
as follows:
- Page 1: [96.3%]
- Page 2: [95.7%]
- Page 3: [81.9%]
- Page 4: [55.8%]
- Page 5: [35.2%]
- Page 6: [26.9%]
## Other information (Optional, Max 100 words)
[-]
