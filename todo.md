* Select number of clusters by BIC
    * BIC : 2lnML + n_clusters*ln(n_puntos)
    * MDL: http://www.cs.bilkent.edu.tr/~saksoy/courses/cs551/slides/cs551_parametric2.pdf 

#### Recsys
* Split training / test. In test, test only for users that exist in training.


* Three baselines:
    - Recommender/ranking by temporal order  x
    - Recommender/ranking by inverse order x
    - Recommender/ranking by degree (Barabasi) x
    - Recommender by Learnin to Rank? (discriminative instead of generative model) system that predicts whether a post is going to be chosen. Use the score to chose the post with highest score. Or rank by score and compare to model-based ranking
   - Single parameter models (to understand whether a single parameter can explain the predictions). Use same estimators as in the full model so that what what we show is the contribution per parameter.