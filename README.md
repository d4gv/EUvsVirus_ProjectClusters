EUvsVirus_ProjectClusters
==============================

Collect project descriptions from EUvsVirus Hackathon devpost, and perform text analysis & clustering.

Project Organization
------------

    ├── LICENSE
    ├── README.md          <- The top-level README for developers using this project.
    ├── ETHICS.md          <- ETHICS checklist for this project.
    │
    ├── environment.yml          <- Anaconda environment for reproducing the analysis environment, e.g.
    │
    ├── data	<- Don't use this directory, as we have a DropBox folder for that.
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── docs               <- A default Sphinx project; see sphinx-doc.org for details.
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries.
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         the creator s initials, and a short `-` delimited description, e.g.
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting.
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`.
    │
    └── src                <- Source code for use in this project.
         ├── __init__.py    <- Makes src a Python module.
         │
         ├── scraping           <- Scripts to scrape DevPost site.
         │
         ├── clustering       <- Scripts to cluster Hackathon projects.
         │
         ├── presentation	  <- Scripts to visualize and present insights.
         │
         └── test		<- Testscripts.

--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
