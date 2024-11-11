

# -- load data
data <- load_data(path = "E:/MyWork/CV/Career path",
                  filename = "career_path_data_en.csv")

# -- apply pipelines
data_2 <- data_pipeline(data)
company <- company_pipeline(data_2)
category <- category_pipeline(data_2)
section <- section_pipeline(data_2)
timeline <- timeline_pipeline(data_2)

# -- set title & description
title_fr <- "Profil<br>Technico-fonctionnel"
desc_fr <- "<b>Coordination de Projets Data</b><br>
        En interaction étroite avec les équipes techniques<br>
        <i>(capacité à parler le même langage et à comprendre les contraintes)</i><br><br>
        <b>Développement d'outils, Analyse de données</b><br>
        Utilisation des compétences techniques pour accélérer certaines tâches"

title_en <- "Technico-functional<br>Profile"
desc_en <- "<b>Data Project Management</b><br>
        In close interaction with technical teams<br>
        <i>(ability to speak the same language and understand constraints)</i><br><br>
        <b>Tool development & Data analysis</b><br>
        Use of technical skills to speed up tasks by developing specific tools"

# -- build plot
p <- career_path(data_2, 
                 profile_title = title_en, 
                 profile_description = desc_en)

# -- display
print(p)
