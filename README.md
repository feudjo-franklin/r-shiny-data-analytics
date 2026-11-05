# Case Study: Data Analytics & Forecasting App (R Shiny)
Dieses Projekt entstand im Rahmen meines Masterstudiums an der TU Berlin (Fachgebiet Qualitätswissenschaft). Ziel war die Entwicklung einer interaktiven Data-Science-Applikation zur Analyse und Prognose von Ersatzteilbedarfen für eine fiktive Autowerkstattskette in Nordrhein-Westfalen.

## Zielsetzung
Auf Basis von drei Jahren historischer Produktions-, Zulassungs- und Logistikdaten (2014–2016) sollte eine Applikation entwickelt werden, die Werkstätten dabei unterstützt, den Bedarf an Ersatzteilen für das kommende Jahr (2017) zu kalkulieren.

## Tech Stack
**Sprache**: R

**App-Framework**: R Shiny

**Datenverarbeitung**: tidyverse, data.table, lubridate

**Visualisierung**: Plotly, Leaflet (interaktive Karten), ggplot2, DT (DataTables)

**Reporting**: R Markdown (HTML-Output)

## Features der Applikation
**Interaktive Filterung**: Ein Balkendiagramm zeigt die monatlichen Ausfälle pro Ersatzteil, gefiltert nach spezifischen Städten und Teilenummern.

**Forecasting**: Visualisierung des Ausfallverlaufs und Prognose für das 1. Quartal 2017 (sowohl gesamt als auch pro Stadt).

**Geodaten-Visualisierung**: Eine interaktive Karte (Leaflet) markiert alle Werkstattstandorte. Beim Anklicken eines Standorts werden Pop-Ups mit den empfohlenen Bestellmengen für das ausgewählte Teil angezeigt.

**Daten-Exploration**: Eine tabellarische Ansicht (DT) des zugrundeliegenden, bereinigten Datensatzes zur Nachvollziehbarkeit der Ergebnisse.

## Datenaufbereitung & Logik
Die Rohdaten (Einzelteile, Komponenten, Fahrzeuge, Zulassungen, Geodaten) lagen in unterschiedlichen Formaten (CSV, TXT) vor. Sie wurden nach den Prinzipien von Tidy Data bereinigt, transformiert (Pivotierung von relationalen Bestandteilstabellen) und in einem einzigen Datensatz zusammengeführt. Ein besonderer Fokus lag auf der korrekten Verknüpfung der relationalen Tabellen (Fahrzeuge -> Komponenten -> Einzelteile), um Datenlecks zu vermeiden.

## Ausführung
Repository klonen:
git clone https://github.com/DEIN_USERNAME/r-shiny-case-study.git

Benötigte R-Packages installieren (die App installiert fehlende Pakete beim ersten Start automatisch, oder manuell):
install.packages(c("shiny", "tidyverse", "data.table", "ggplot2", "leaflet", "lubridate", "DT"))

Die Applikation starten:

Öffne die Datei Case_study_App_07.R in RStudio.

Klicke auf den Button **"Run App"** (oben rechts im Editor).

Alternativ in der R-Konsole:
shiny::runApp("Case_study_App_07.R")

## Dokumentation
Eine detaillierte Beschreibung des Analyseprozesses, der Datenaufbereitung und der Ergebnisse ist in der bereitgestellten** R Markdown Datei** (.Rmd und .html) zu finden.

## Datensatz (Download)
Da der vollständige Datensatz (ca. 10 GB) zu groß für ein GitHub-Repository ist, steht das komplette Projekt (inklusive aller Daten und Skripte) zum Download auf Google Drive zur Verfügung:

Hier klicken, um das komplette Projekt herunterzuladen (Google Drive)
https://drive.google.com/file/d/1w5XFIOMha1GGeDppeTMPqQswlhMUY8Sn/view?usp=sharing

---
Hinweis: Nach dem Download einfach die ZIP-Datei entpacken und das Projekt in RStudio öffnen (z.B. über die .Rproj-Datei oder direkt die Case_study_App_07.R). Alle benötigten Daten sind bereits im Ordner data/ enthalten.

*Hinweis: Dieses Projekt dient ausschließlich zu Demonstrationszwecken als Teil meines Portfolios. Die verwendeten Daten sind anonymisiert.*