* Select number of clusters by BIC

#### Recsys
* Split training / test. In test, test only for users that exist in training.


* Three baselines:
    - Recommender/ranking by temporal order 
    - Recommender/ranking by inverse order
    - Recommender/ranking by degree (Barabasi)
    - Recommender by Learnin to Rank? (discriminative instead of generative model) system that predicts whether a post is going to be chosen. Use the score to chose the post with highest score. Or rank by score and compare to model-based ranking