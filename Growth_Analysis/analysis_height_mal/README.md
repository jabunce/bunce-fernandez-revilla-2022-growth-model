# Analysis of male height using the 3-component composite model, the 5-component composite model, the JPA-1 model, and the SITAR model

This makes Appendix Figures A.5, A.8, A.10, and A.17 - A.21.

<br/>
Steps to reproduce the analysis:

1) Create an analysis folder on your machine. Name it whatever you want. Place it inside the project folder you created [previously](../README.md). 

2) Inside this analysis folder, put the file ``RunAll.R``

3) Also inside the analysis folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the files ``Berkeley.csv`` and ``Matsigenka.csv``.

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your analysis folder. Then run its parts in R.

Within ``RunAll.R``, you can run sections for the 3- and 5-component composite models, JPA-1 model, and SITAR model independently, and in any order.

Fitting the the 3- and 5-component composite models and the JPA-1 model in Stan with the numbers of chains and samples used in the manuscript can take several days to run. However, you can usually get fairly reasonable quick estimates with only two chains of 1000 samples each. Within ``RunAll.R`` you can change the numbers of chains and samples.

Figures will appear in the ``Plots`` folder.
