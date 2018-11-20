latexdiff ./initial_submission/pmassicotte_et_al_2018.tex ./revision_1/pmassicotte_et_al_2018.tex > ./tracking_changes_revision_1/pmassicotte_et_al_2018_with_tracking_changes.tex
latexdiff ./initial_submission/introduction.tex ./revision_1/introduction.tex > ./tracking_changes_revision_1/introduction.tex
latexdiff ./initial_submission/methods.tex ./revision_1/methods.tex > ./tracking_changes_revision_1/methods.tex
latexdiff ./initial_submission/results.tex ./revision_1/results.tex > ./tracking_changes_revision_1/results.tex
latexdiff ./initial_submission/discussion.tex ./revision_1/discussion.tex > ./tracking_changes_revision_1/discussion.tex
latexdiff ./initial_submission/conclusions.tex ./revision_1/conclusions.tex > ./tracking_changes_revision_1/conclusions.tex

cd ./tracking_changes_revision_1/
xelatex pmassicotte_et_al_2018_with_tracking_changes.tex


latexdiff ./revision_1/pmassicotte_et_al_2018.tex ./revision_2/pmassicotte_et_al_2018.tex > ./tracking_changes_revision_2/pmassicotte_et_al_2018_with_tracking_changes.tex
latexdiff ./revision_1/methods.tex ./revision_2/methods.tex > ./tracking_changes_revision_2/methods.tex
latexdiff ./revision_1/introduction.tex ./revision_2/introduction.tex > ./tracking_changes_revision_2/introduction.tex
latexdiff ./revision_1/results.tex ./revision_2/results.tex > ./tracking_changes_revision_2/results.tex
latexdiff ./revision_1/discussion.tex ./revision_2/discussion.tex > ./tracking_changes_revision_2/discussion.tex
latexdiff ./revision_1/conclusions.tex ./revision_2/conclusions.tex > ./tracking_changes_revision_2/conclusions.tex

cd ./tracking_changes_revision_2/
xelatex pmassicotte_et_al_2018_with_tracking_changes.tex
