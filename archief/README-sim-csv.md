# SIM CSV → FSH Logical Models
Doel: technische uitgangsmodellen genereren; informatieanalisten doen alle semantische modellering zelf.

## Standaardworkflow
Bronnen zijn de drie `informatiemodel Santeon*.csv`-bestanden in `../HipsETL/SIM/`.
Een andere bronmap kan via `--source PAD`; de bron wordt uitsluitend gelezen.
Alle modellen worden automatisch ontdekt, zonder modelregister of uitzonderingen.
Uitvoer: `input/fsh/generated-sim/SIM<Model>Source.fsh`. De bestaande IG-configuratie wordt gebruikt.
SUSHI neemt deze modellen automatisch mee. Het volledige IG-buildcommando staat onderaan;
open daarna `output/CLMs.html` of `output/artifacts.html`.

Conversieregels gelden identiek voor ieder model:
- Elk CSV-veld wordt één afzonderlijk element met dezelfde veldnaam.
- FHIR-primitieven blijven gelijk; `coding.system/code/display` worden `uri/code/string`.
- Beide identifier-brontypen blijven `string`; er wordt niets opgesplitst of gegroepeerd.
- `is_nullable=FALSE` geeft `1..1`, `TRUE` geeft `0..1`: technische aanwezigheid, geen klinische verplichtheid.
- Beschrijvingen worden letterlijk bewaard; stringlengtes worden waar toepasselijk overgenomen.
- Overige bronmetadata en ValueSet-relaties staan als commentaar in FSH; geen bindings of FHIRPath-regels.
- Volgorde: bronvolgnummer, daarna veldnaam. Dubbele volgnummers wijzigen geen veldidentiteit.
- CSV-recordnummer, bestandshashes en expliciete `model;field`-mappings behouden traceerbaarheid.

Er is geen automatische semantiek, vergelijking met handmatige modellen of GBB/FHIR-profielmapping.
Onbekende typen, ongeldige vlaggen/getallen, dubbele velden en ongeldige bronverwijzingen geven technische fouten.
De tests controleren dekking, uniforme conversie, reproduceerbaarheid, fouten en overschrijfbeveiliging.

Handmatig semantisch onderhouden modellen staan in `input/fsh/sim/`. Geef een afgeleid model een eigen
Logical-naam en Id om dubbele definities te voorkomen. De generator schrijft uitsluitend
in `input/fsh/generated-sim/` en weigert gewijzigde of niet-gegenereerde bestanden te overschrijven. De controlehash
bovenaan ieder gegenereerd bestand vervangt een apart eigendomsmanifest. Verouderde uitvoer vraagt expliciete review.
SUSHI-compilatiebestanden en tijdelijke logs blijven buiten versiebeheer.
De bestaande bronmodel-identifiers en canonieke URLs blijven gelijk bij de integratie in de IG.

## Scope IBD
- `archief/ibd_scope_op_basis_van_yaml.csv` is gebaseerd op de indicator-YAML's in `HipsETL/indicatoren/sb-ibd`.
- Meegenomen: de 88 indicatoren met een analyse (waardegedreven zorg, kwartaalrapportage, zorgpaddeelnemer), plus `PopulatieVolwassenen` als cohortdefinitie.
- (Nog) niet meegenomen: de 17 indicatoren zonder analyse, namelijk kinder-IBD (9) en losse lab- en medicatie-indicatoren (8).
- Resultaat: 9 modellen en 73 velden. Per veld staat in de CSV welke indicatoren het gebruiken en welke filters gelden.
- De `${ZA_...}`-variabelen uit de YAML's zijn in de CSV uitgeschreven naar NZa-codes volgens `l_ZACodeSets` in `HipsETL/indicatoren/utilities/load_general_functions_and_metadata_id.r`.