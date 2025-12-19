![](images/NZILBB2.png)

# Acquiring a Vowel System in Motion: Preschoolers and the NZE Expanded Short Front Vowel Shift

Joshua Wilson Black (a) and Lynn Clark (a, b)

a: New Zealand Institute of Language, Brain and Behaviour, University of Canterbury.

b: Department of Linguistics, University of Canterbury.

_Under review_

---

### Abstract

The New Zealand English (NZE) vowel system exhibits both variability and complex
systematicity. Sound changes across the vowel system in NZE have been well
documented for some time (e.g. Gordon et al. 2004). Recent work shows that
vowels involved in the NZE short front vowel shift (i.e. TRAP, DRESS, & KIT)
covary with other vowels (especially FLEECE and NURSE; see Brand et al. (2021))
and that this covariation remains intact even as individual vowels in this
cluster stabilise or change direction (Hurring et al. 2025). In this study, we
explore what happens when children are acquiring their accent in such a complex
environment. We do this by applying acoustic phonetic methods to a corpus of
preschool children (n=124, 3;11-5;5) from Christchurch, New Zealand and a
community corpus of adult talkers (n=345). We consider the vowels associated
with this cluster of covariation which we call the Extended NZE Short Front
Vowel Shift. We find that children produced more conservative vowels than
expected, sometimes diverging from community changes in progress. We consider
vernacular reorganisation, developmental effects, hyperarticulation, and priming
as explanations for the patterns that we see in the children’s developing vowel
spaces.

### How to use the GitHub repository

The main entry point to this project is the OSF.io page at <https://osf.io/cz3jt/>.

The simplest way to use this material is to view the supplementary files using your web browser. The benefit of this is that you can easily see the commentary and the code and do not have to run it yourself (which takes a long time). The disadvantage is, correspondingly, that you cannot step through each step in your own R session or see what happens if you modify the code. The web versions of the supplementary materials are available at:

1. [Filtering and Formant Tracking](https://nzilbb.github.io/vs-in-motion/markdown/SM1_formants.html)
2. [Modelling](https://nzilbb.github.io/vs-in-motion/markdown/SM2_modelling.html)

This repository (the GitHub repository) provides access to the majority of the
code and data for the project. To fully retrace our steps, you will also need:

1. **Acquire prefit models from the OSF repository.** Models are stored here at OSF as a '.rds' file, which is openable by R using the base R `readRDS` function or `readr::read_rds`. The models reported in the paper and supplementary material are also available in OSF storage. Place models in a directory called 'models' in the project directory (i.e. the base directory of the git repository, if you cloned the repository from GitHub, or the directory which was generated when you unzipped the ZIP file you downloaded from GitHub.

2. **Acquire `praat.zip` from the OSF repository and unzip it in the project directory.** `SM1_formants.html`, linked above, describes a process for validating formant tracking settings. The file `praat.zip` contains the files produced in this process and is necessary for `SM1_formants.html` to be produced from the `qmd` file. 

