# MZES Mitarbeiter\*innen-Befragung 2025

## Hinweise für Bilendi

*Stand:18.06.2025:* **Aktuell sind nur Schritte 1 + 2 ausführbar.**

### I. Vorbereitung

1. Arbeiten Sie stets im RProject (`mzes-bilendi.RProj`). Beim Start-up wird u.a. die Ordnerstruktur des Projekts erzeugt und benötigte Packages installiert und geladen.
1. Legen Sie den Datensatz **im Stata-Format** (`.dta`) in den Ordner `dat` und passen Sie in der Datei `.Rprofile` den Wert des Objekts `bilendi_dta_path` an. Nach der erstmaligen Änderung muss die R-Sitzung neu gestartet werden, damit die Änderung effektiv wird.
1. Führen Sie mittels `r/preprocess-data.R` die initiale Datenaufbereitung durch.

### II. Auswertung der qualitativen/offenen Freitextfragen

1. Führen Sie bitte das Skript `r/get-randomized-responses.r` aus. Das Skript erzeugt zwei CSV-Datei, die gemäß Datenschutzkonzept alle validen (nicht-fehlenden) Freitextantworten innerhalb einer Fragegruppe zufallssortiert und frei von sonstigen identifizierenden Informationen (Beobachtungsnummer, sonstige Angaben auf Beobachtungsebene) erfasst:
    - `csv/open-text/pre-anonymization.csv` (als Referenz)
    - `csv/open-text/post-anonymization.csv` (zur aktiven manuellen Anonymisierung)
1. **Bitte arbeiten Sie in der Datei `csv/open-text/post-anonymization.csv`, um das besprochene manuelle Anonymisierungsverfahren umzusetzen:**
    1. "Um zu gewährleisten, dass Freitextantworten keine Rückschlüsse auf einzelne Personen zulassen, werden die offenen Antworten zunächst vom beauftragten Unternehmen auf Anonymität geprüft und, falls nötig, anonymisiert."" **Bitte vermerken Sie im Zuge dieses Schritts manuelle Änderungen zur Gewährleistung der Anonymität in der Spalte** `anonymized_by_respondi`. 
    1. "Anschließend werden die offenen Antworten an zwei Beschäftigte des MZES übermittelt und erneut auf Anonymität überprüft. Die Übermittlung der Daten erfolgt dabei ohne Verknüpfung zu sonstigen personenbezogenen Daten, sondern lediglich in Form eines Textdokuments, das die offenen Antworten zu einer gegebenen Frage in zufälliger Reihenfolge ohne Bezug zu weiteren quantitativen Angaben (wie bspw. persönlichen Merkmalen) beinhaltet. Sollte die Gefahr der Deanonymisierung aufgrund des Inhalts der Freitextangaben bestehen, werden die betroffenen Freitextangaben weiter anonymisiert. Sollte dies nicht möglich sein, wird die betroffene Freitextantwort gelöscht." **Bitte kennzeichnen Sie zur Erleichterung dieses Schritts alle Antworten, bei denen Sie implizite (d.h. sich ggf. aus dem Kontext ergebende) deanonymisierende Inhalte vermuten, in der Spalte** `flagged_by_respondi`.

### III. Auswertung der quantitativen/geschlossenenen Fragen

1. Führen Sie bitte das Skript `r/get-univariate-summaries.r` aus. Die dadurch erzeugten univariaten Häufigkeitverteilungen werden unter `csv/univariate` gespeichert.
1. Führen Sie bitte das Skript `r/get-bivariate-summaries.r` aus. Die dadurch erzeugten bivariaten Häufigkeitverteilungen (Häufigkeitesverteilungen nach Subgruppen) werden unter `csv/bivariate` gespeichert. Dabei werden gemäß Datenschutzkonzept Auswertungen unterbunden, bei denen eine Subgruppengröße von $N=5$ unterschritten wird. Die Subgruppengröße wird in der Spalte `subgroup_size` ausgegeben, die Spalte `value` wird mit dem Hinweis "Subgroup too small" versehen wenn `subgroup_size < 5`.
1. Führen Sie bitte das Skript `r/get-regression-results.r` aus. Die dadurch erzeugten Regressionsergebnisse (b-Vektoren, Varianz-Kovarianz-Matritzen, Fallzahl, R2) werden unter `csv/regression` gespeichert. Dabei werden gemäß Datenschutzkonzept Auswertungen unterbunden, bei denen eine Fallzahl von $N=5$ unterschritten wird. 

### IV. Export

Für den Export senden Sie bitte folgende Ordner/Auswertungsdateien an die hier verlinkten Rezipient*innen:

1. Die manuell anonymisierte Fassung der Datei `csv/open-text/post-anonymization.csv` an [Alexander Wenz](mailto:alexander.wenz@mzes.uni-mannheim.de) und [Ruben Bach](mailto:ruben.bach@mzes.uni-mannheim.de)
1. Den Inhalt folgender Ordner an [Frieder Rodewald](mailto:frieder.rodewald@uni-mannheim.de)
    - `csv/univariate`
    - `csv/bivariate`
    - `csv/regression`
