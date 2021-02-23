# Modele-de-melange-Clustering-CoClustering-ConsensusClustering.
Le But de ce projet est de prouver la robustesse de l’approche Consensus Clustering qui se base sur plusieurs itérations
de la méthode de classification choisie (Clustering) sur des sous-échantillons du jeu de données.
Dans la première étape j'ai sélectionné les 5 meilleurs partitions en modifiant à chaque fois le modèle utilisé
par la fonction mclust. Ensuite grâce à la fonction Rmixmod j'ai obtenue une partition consensuelle
avec le même nombre de classes et à chaque fois je compare les résultats en terme de critère BIC, Accuracy,
NMI et ARI afin d’appercevoir les améliorations apportés.
Dans la deuxième étape j'ai traité un ensemble de données décrivant des documents avec un algorithme
basé sur l’information mutuelle en utilisant trois méthodes (CoClust, CoClustInfo, Cluster-Ensembles).
Enfin la derrière étape j'ai utilisé une autre méthode du Co-Clustering : la modularité qui se basent sur
deux algorithmes : CoClustInfo, CoClustSpecMod.
