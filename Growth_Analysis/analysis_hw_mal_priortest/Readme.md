# Analysis of male height using priors derived from posterior estimates of Matsigenka females. Tests robustness of results to changes in informative priors.

<br/>
Steps to reproduce the analysis:

1) Create an analysis folder on your machine. Name it whatever you want. Place it inside the project folder you created [previously](../README.md). 

2) Inside this analysis folder, put the file ``RunAll.R``

3) Also inside the analysis folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the files ``Berkeley.csv`` and ``Matsigenka.csv``.

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your analysis folder. Then run its parts in order in R.

Fitting the models in Stan with the numbers of chains and samples used in the manuscript can take several days to run. However, you can usually get fairly reasonable quick estimates with only two chains of 1000 samples each. Within ``RunAll.R`` you can change the numbers of chains and samples.

Figures will appear in the ``Plots`` folder.
