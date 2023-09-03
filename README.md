- [Description of *Text-as-data analysis of floor debate in Mexico's Chamber of Deputies* repository](#orga3615e4)
- [Files in the repository and how to cite them](#orgfc873f9)
- [Codebook](#orga144f39)
  - [Variables in files data/speech-periodo-\*.RData](#org96ad2e6)
  - [Variables in file data/all-dips-list.RData](#org860a210)
- [Acknowledgements](#org7709069)

Last revision: 2022-09-03


<a id="orga3615e4"></a>

# Description of *Text-as-data analysis of floor debate in Mexico's Chamber of Deputies* repository

-   Author: Eric Magar
-   Email: emagar at itam dot mx

The repository contains data and code to replicate analysis in the Mexico chapter of a recent book on legislative debate worldwide. The main script used for analysis is `code/legdeb.r`. Script `code/data-prep.r` turned raw speeches into `R`-format structured data in files `data/speech-day-6x.RData` (daily aggregates) and `data/speech-periodo-6x.RData` (period aggregates).


<a id="orgfc873f9"></a>

# Files in the repository and how to cite them

You are free to download and modify the data (see the LICENSE document) provided you give proper credit to this source. The cite is Eric Magar (2021) "Mexico: Parties and Floor Access in the Cámara de Diputados" in *Politics of Legislative Debates* edited by Hanna Bäck, Marc Debus, and Jorge M. Fernandes, Oxford: Oxford University Press, pp. 572-93 (pdf available [here](https://github.com/emagar/leg-debate/tree/master/paper/legdeb04.pdf)). BibTeX entry:

```
@incollection{magar.2021,
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


<a id="orga144f39"></a>

# Codebook


<a id="org96ad2e6"></a>

## Variables in files data/speech-periodo-\*.RData

See the published text for details.

-   `leg` = Legislature numeral 60, 62 or 64.
-   `agg` = aggregation unit: member-legislative period.
-   `sel.agg` = Legislature + year + legislative period, eg. 62y2-1 for observations corresponding to the first ordinary period of the second year of the 62nd Legislature.

-   `edo` = member's state abbreviation (may differ from commonly used abbreviations, eg. Chiapas is \`cps', not \`chis' so that sorting alphabetically preserves the order set by *edon*)
-   `id` = member's id: state + district + p/s for propietario/suplente.
-   `birth` = member's birth year.
-   `postulo` = electoral party or coalition.
-   `part` = legislative party.
-   `dsup` = dummy equal 1 if member was elected as a substitute (*suplente*), 0 otherwise (*propietario*).
-   `cabecera` = member's district administrative head.
-   `lider` = member's leadership post, if any.
-   `prescom` = dummy equal 1 if member was a committee chair, 0 otherwise (*propietario*).
-   `repite` = member's previous Congressional experience (sequential Legislatures if prior deputy, \`sen' if prior senator).
-   `doath` = dummy equal 1 if member took the oath of office (*toma de protesta*), 0 otherwise.
-   `nom` = member's name.
-   `dv.nword` = absolute total words member spoke in the period.
-   `dv.nword.sh` = relative total words member spoke in the period, ie. `dv.nword` / `ev.pot.sh`.
-   `ev.pot.dys` = session days the member served in the legislative period.
-   `ev.all.dys` = total session days in the legislative period.
-   `ev.pot.sh` = by share of the period the member served, ie. `ev.pot.dys` / `ev.all.dys`.
-   `dfem` = dummy equal 1 if member was a woman, 0 otherwise.
-   `ptysh` = share of seats controlled by the member's party.
-   `dpastleg` = dummy equal 1 if member had prior Congressional experience, 0 otherwise.
-   `age` = member's age at the start of the Legislature.
-   `dpan` = dummy equal 1 if member's party was PAN, 0 otherwise.
-   `dpri` = dummy equal 1 if member's party was PRI, 0 otherwise.
-   `dmorena` = dummy equal 1 if member's party was Morena, 0 otherwise.
-   `dleft` = dummy equal 1 if member's party was PRD, 0 otherwise.
-   `doport` = dummy equal 1 if member's party was an opportunistic party, 0 otherwise.

-   `dchair` = dummy equal 1 if member was committee chair, 0 otherwise.
-   `dleader` = dummy equal 1 if member held a leadership post (*coordinador*), 0 otherwise.
-   `dsmd` = dummy equal 1 if member elected in single-member district, 0 otherwise.
-   `dpresoff` = dummy equal 1 if member was chamber president, 0 otherwise.
-   `dv.nspeech` = number of speeches member made in the period.


<a id="org860a210"></a>

## Variables in file data/all-dips-list.RData

File includes object \`all.dips', which is a list of dataframes, one per Legislature. So, eg., all.dips$leg60 is a dataframe of all members of the 60th Legislature.

-   `leg` = Legislature numeral.

-   `pila` = member's first name and middle names, if any.
-   `patmat` = member's last names (patronym and matronym).
-   `id` = member's id: state + district + p/s for propietario/suplente.
-   `birth` = member's birth year.
-   `gen` = member's gender, M or F.
-   `postulo` = electoral party or coalition.
-   `part` = legislative party.
-   `edo` = member's state.
-   `dsmd` = dummy equal 1 if member elected in single-member district, 0 otherwise.
-   `dsup` = dummy equal 1 if member was elected as a substitute (*suplente*), 0 otherwise (*propietario*).
-   `cabecera` = member's district administrative head.
-   `yrin1`, `moin1`, `dyin1` = year month day member first started serving in the Legislature.
-   `yrout1`, `moout1`, `dyout1` = year month day member first took a leave of absence (*licencia*), if any.
-   `yrin2`, `moin2`, `dyin2` = year month day member returned from first leave of absence, if any.
-   `yrout2`, `moout2`, `dyout2` = year month day member took second leave of absence (*licencia*), if any.
-   `yrin3`, `moin3`, `dyin3` = year month day member returned from second leave of absence, if any.
-   `yrout3`, `moout3`, `dyout3` = year month day member took third leave of absence (*licencia*), if any.
-   `lider` = member's leadership post, if any.
-   `prescom` = dummy equal 1 if member was a committee chair, 0 otherwise (*propietario*).
-   `repite` = member's previous Congressional experience (sequential Legislatures if prior deputy, \`sen' if prior senator).
-   `doath` = dummy equal 1 if member took the oath of office (*toma de protesta*), 0 otherwise.
-   `ptysh` = share of seats controlled by the member's party.
-   `nom` = member's name.


<a id="org7709069"></a>

# Acknowledgements

I acknowledge financial support from the Asociación Mexicana de Cultura A.C. and CONACYT's Sistema Nacional de Investigadores. I am grateful to Ana Lucía Enríquez Araiza, Sonia Kuri Kosegarten, Vidal Mendoza Tinoco, and Eugenio Solís Flores, for research assistance. The author is responsible for mistakes and shortcomings in the study.
