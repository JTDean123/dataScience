# Yelp
Prediticing Yelp Restaurant Rating

How often do you use Yelp? I find myself using it often whenever I am in a new town and in desperate need of a food suggestion, because heaven forbid I eat at a restaurant that only gets a 3 star rating when there is a 4 star option down the street. Yelp has millions of users and reviews, and if a restaurant gets a significant amount of bad reviews, or even just a handful, this can have huge financial implications for the owners. As such, Yelp is ultimately driven by it's users and the reviews they submit. Yelp has kindly released a treasure chest of their data to the public for analysis and exploration:
https://www.yelp.com/dataset_challenge/dataset  

The possibilities with this data set are nearly endless, and here we focus on individual restuaruant reviews. In particular, we aim to answer the following question:

Can we predict the rating of a restaurant on Yelp using a restaurant's attributes?  

This problem can be reformulated as one of binary classification, as we will see, and from this angle we have a multiple supervised machine learning algorithms at our disposal. Here we will focus on implementing a Support Vector Machine (SVM) model to predict Yelp restaurant rating from the data provided in the public data set.
