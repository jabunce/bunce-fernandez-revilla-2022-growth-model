# Makes composite figures in manuscript after sub-analyses are completed

<br/>
Steps to reproduce the analysis:

1) Complete the eight sub-analyses for female and male height and weight in the [parent folder](../README.md). 

2) Create a folder on your machine named exactly ``combined_plots``. Place it inside the project folder you created [previously](../README.md). 

3) Inside this ``combined_plots`` folder, put the file ``RunAll.R``

4) Also inside the ``combined_plots`` folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

5) Inside the ``Data`` folder, put the files ``Berkeley.csv`` and ``Matsigenka.csv``.

6) Inside the ``Code`` folder, put all the other files.

7) Open the file ``RunAll.R``. Inside it, set the path to your project folder [parent folder](../README.md), not to the folder ``combined_plots``. Then run its parts in order in R, following the instructions within.

Plots will appear in the ``Plots`` folder.
