- [Description of *Text-as-data analysis of floor debate in Mexico's chamber of deputies* repository](#org71d4721)
- [Files in the repository and how to cite them](#org0405f92)
- [Acknowledgements](#org01faa9f)

Last revision: 2022-06-11


<a id="org71d4721"></a>

# Description of *Text-as-data analysis of floor debate in Mexico's chamber of deputies* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains data and code to replicate analysis in the Mexico chapter of a recent book on legislative debate worldwide. The main script used for analysis is `code/legdeb.r`. Script `code/data-prep.r` turned raw speeches into structured data in `R`-format files `data/speech-day-6x.RData` (daily aggregates) and `data/speech-periodo-6x.RData` (period aggregates).


<a id="org0405f92"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document) provided you give proper credit to this source. The cite is Eric Magar (2021) "Mexico: Parties and Floor Access in the Cámara de Diputados" in *Politics of Legislative Debates* edited by Hanna Bäck, Marc Debus, and Jorge M. Fernandes, Oxford: Oxford University Press, pp. 572-93 (pdf available [here](https://github.com/emagar/leg-debate/tree/master/paper/legdeb04.pdf)). BibTeX entry:

```
@incollection{magar.debate.2021,
        author = {Magar, Eric},
        title = {Mexico: Parties and Floor Access in the C\'amara de Diputados},
        booktitle = {Politics of Legislative Debates},
        editor = {B\"ack, Hanna and Debus, Marc and Fernandes, Jorge M.},
        publisher = {Oxford University Press},
        address = {Oxford},
        pages = {572--93},
        doi = {10.1093/oso/9780198849063.003.0028},
        isbn = {9781421415543},
        year = 2021
}
```


<a id="org01faa9f"></a>

# Acknowledgements

I acknowledge financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. I am grateful to Ana Lucía Enríquez Araiza, Sonia Kuri Kosegarten, Vidal Mendoza Tinoco, and Eugenio Solís Flores, for research assistance. The author is responsible for mistakes and shortcomings in the study.
