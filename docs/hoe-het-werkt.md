# Hoe indicatoren en datasets in SIM on FHIR werken

Dit document legt uit hoe een indicator of dataset wordt vastgelegd en hoe daaruit automatisch een FHIR-export komt. Je hoeft geen code te lezen. Het is genoeg om te weten welk bestand je aanpast.

## In één alinea

Je beschrijft **wat je wilt weten** (een indicator, een dataset, een meting) in kleine tekstbestanden. Een script leidt daar drie dingen uit af: het **FHIR-model** (welke codes en velden zijn toegestaan), het **exportverzoek** (welke patiënten en welke gegevens worden opgehaald) en de **pagina's** die je in de IG ziet. Je past dus nooit een profiel of een zoekvraag met de hand aan: je past de definitie aan en laat het script de rest maken.

## De bouwstenen

| Bouwsteen | Wat het beschrijft | Waar je het vindt |
|---|---|---|
| **Meting** (ObservationDefinition) | Eén meting: de code, wat voor soort uitslag (getal of code), de eenheid, de toegestane antwoorden | `input/fsh/definitions/ObservationDefinitions.fsh` |
| **Indicator** (KPI-definitie) | Welke gegevens een indicator nodig heeft, als FHIR-zoekvragen | `input/fsh/definitions/Kpis.fsh` |
| **Dataset** | Eén export per ziektebeeld: wie in het cohort zit, welke indicatoren erbij horen, welke extra gegevens | `input/fsh/datasets/IBD.fsh` (één bestand per dataset) |
| **ValueSet** (lijst van codes) | Welke codes bij een cohort, meting of antwoord horen | lokaal in `input/fsh/valuesets/`, of extern in ART-DECOR of op de NTS (zie hieronder) |
| **Configuratie externe ValueSets** | Welke externe lijsten we gebruiken en op welke versie ze vastgepind zijn | `input/fsh/definitions/external-valuesets.yaml` |

De structuur van een indicator en een dataset (welke velden ze mogen hebben) staat in `input/fsh/definitions/DefinitionModels.fsh`. Daar hoef je normaal niets aan te veranderen.

## Een indicator

Een indicator zegt niet "teller" of "noemer". Dat is bewust: die rol hoort bij de berekening achteraf. De indicator zegt alleen **welke gegevens nodig zijn**, als een lijst van zoekvragen op FHIR-resources.

Voorbeeld, K3.6.3 (calprotectine voor een scopie), ingekort:

```
Verrichting (Procedure):  code uit lijst 'scopie-verrichtingen', datum vanaf 2018
Meting (Observation):     code = calprotectine (LOINC 38445-3), datum vanaf 2018
Nabewerking:              alleen een meting die in de 90 dagen vóór de scopie valt
```

In het bestand staat dat als zoekparameters, zoals in FHIR zelf:

```
code:in   = {vs:local-verrichting-codes-nza}
code      = {od:od-calprotectin.code}
date      = ge2018-01-01
```

Wat tussen accolades staat, is een verwijzing die het script oplost:

| Schrijfwijze | Wordt |
|---|---|
| `{vs:<naam>}` | de web-URL van die ValueSet |
| `{od:<meting>.code}` | de code van die meting (`systeem\|code`) |
| `{od:<meting>.unit}` | de eenheid van die meting |
| `{today}`, `{today-18y}`, `{today-90d}` | de datum van de exportdag, met een verschuiving. De exporter vult de echte datum in. |

**Nabewerkingen** (`postFilter`) zijn dingen die FHIR niet kan zoeken: "alleen de laatste meting per patiënt" of "een meting binnen 90 dagen vóór een verrichting". Die staan als tekst bij de indicator, zodat iedereen ze kan lezen. Ze zitten niet in de exportvraag: ze worden toegepast nadat de gegevens zijn opgehaald, in de berekening.

## Een dataset

Een dataset is één export voor één ziektebeeld. Hij bestaat uit drie delen:

1. **Cohort**: wie zit erin. Elk criterium is een zoekvraag; een patiënt moet aan **alle** criteria voldoen. Voor IBD: 18 jaar of ouder, met een actief DBC-zorgtraject van zorgtype 11 of 21 vanaf 2018 en een IBD-specialisme-diagnose.
2. **Indicatoren**: de lijst van indicatoren waarvan de gegevens worden vrijgegeven.
3. **Extra gegevens** (`include`): gegevens die bij geen enkele indicator horen maar wel in de export moeten, zoals het Patient-model met een filter op geslacht.

## Wat eruit komt

Uit alle definities samen maakt het script:

```
definities  ──►  script  ──►  profielen        één per resourcetype, met alleen de toegestane codes en velden
                         ──►  Group            het cohort van de dataset
                         ──►  Parameters       het exportverzoek: _type, _typeFilter, _elements
                         ──►  pagina's         overzicht en één pagina per dataset
                         ──►  menu             het uitklapmenu Datasets
```

- **Het model volgt de definities.** Een profiel staat alleen codes toe die een indicator of dataset vraagt, en sluit alle velden die niemand nodig heeft. Voeg je een indicator toe, dan verandert het profiel mee.
- **Eén lijst per resourcetype.** Losse codes uit een indicator, de codes van de metingen en de ValueSets uit ART-DECOR of NTS worden samengevoegd tot één gegenereerde lijst, bijvoorbeeld voor Observation. Die lijst is een resultaat en wordt niet met de hand onderhouden.
- **Eén profiel per resourcetype, niet per meting.** Wat bij welke meting hoort (getal of code, welke eenheid, welke antwoorden) wordt per meting als regel in dat profiel opgenomen, afgeleid uit de meting.
- **Zoekvragen worden samengevoegd.** Metingen die alleen in hun code verschillen worden één `_typeFilter` met een lijst van codes: `Observation?code=38445-3,228273003&date=ge2018-01-01`.

## ValueSets: lokaal, ART-DECOR of NTS

Waar een lijst woont, hangt af van wie hem beheert.

| Soort | Beheer in | Hoe je ernaar verwijst |
|---|---|---|
| Landelijke of gedeelde klinische lijsten | ART-DECOR | de URL, in de configuratie met een vastgepinde versie |
| Lijsten op basis van een SNOMED-zoekvraag, of op de NTS | NTS, als verwijzing | de URL, in de configuratie als `reference` |
| Twee of drie codes voor één doel | vast in de definitie | `code=systeem\|code,systeem\|code` |
| Eigen lijsten van dit project | `input/fsh/valuesets/` | `{vs:<naam>}` |

Een externe lijst wordt **niet** gekopieerd. In `external-valuesets.yaml` staat per lijst de bron en de versie. Zo kun je later controleren of er een nieuwere versie is.

**Antwoorden bij een meting** kunnen op twee manieren: als lijst bij de meting zelf (bijvoorbeeld uit ART-DECOR) of als `value-concept` in een indicator. Het script voegt beide samen tot één antwoordenlijst per meting.

## Wat je doet bij...

**Een nieuwe indicator.** Voeg in `Kpis.fsh` een indicator toe met zijn zoekvragen. Noem hem in `datasets/<naam>.fsh` onder `kpi`. Draai het script.

**Een nieuwe meting.** Voeg in `ObservationDefinitions.fsh` een meting toe (code, soort uitslag, eenheid of antwoordenlijst). Gebruik hem in een indicator met `{od:<meting>.code}`. Een meting die in geen enkele indicator staat, komt niet in het model.

**Een nieuwe dataset.** Maak een bestand in `input/fsh/datasets/` met cohort en indicatoren. Zet de nieuwe pagina onder `datasets.md` in `sushi-config.yaml`. Het menu vult het script zelf.

**Een externe ValueSet.** Registreer hem eenmalig:

```
py -3 _syncValueSets.py add <url> --key <naam>
```

Controleer later of de versie nog klopt:

```
py -3 _syncValueSets.py check      # meldt wat verouderd is
py -3 _syncValueSets.py update     # zet verouderde lijsten op de nieuwste versie
```

Voor de NTS zijn `NTS_USERNAME` en `NTS_PASSWORD` nodig als omgevingsvariabelen. Ze staan nergens in de repository.

**Alles opnieuw genereren en bouwen:**

```
py -3 _generateFromDefinitions.py     # schrijft input/fsh/generated/ en de pagina's
sushi .                               # controleert de FSH
_genonce.bat                          # bouwt de IG (duurt ongeveer zeven minuten)
```

Wat het script schrijft (`input/fsh/generated/`, `datasets.md`, `<dataset>.md`) pas je nooit met de hand aan: de volgende keer wordt het overschreven. Met `py -3 _generateFromDefinitions.py --today 2026-10-06` zie je alle zoekvragen met de datums al ingevuld.

## Hoe een export daarna verloopt

De export is een reeks gewone FHIR-aanroepen, na elkaar:

1. Het cohort als Group aanmaken (`PUT [base]/Group/ibd-dataset-cohort`).
2. Wachten tot de Group bestaat.
3. De export starten op die Group (`POST [base]/Group/ibd-dataset-cohort/$export`), met de gegenereerde Parameters als body.
4. Wachten tot de export klaar is.
5. De bestanden ophalen.

Dat kan niet in één Bundle: de export loopt asynchroon. FENIX kan de reeks wel achter één aanroep van zichzelf verbergen. Wie de reeks gebruikt, vult eerst de datums (`{today-18y}`) in.

## Wat het systeem niet doet

- **Relaties tussen gegevens** ("binnen 90 dagen vóór de scopie") en **"laatste per patiënt"** staan als tekst bij de indicator en worden in de berekening toegepast, niet in de export.
- **De rol van een criterium** (teller, noemer) staat niet in de definitie.
- **De-identificatie** hoort bij de export, maar staat in een eigen regelset, zie de pagina De-identification. Elk veld in `_elements` en elke zoekfilter in `_typeFilter` moet door die regels gedekt zijn.
- **Antwoorden uit een lijst die niet te downloaden is** (een SNOMED-zoekvraag) worden pas door de terminologieserver gecontroleerd als gegevens worden gevalideerd.

## Woordenlijst

| Term | Betekenis |
|---|---|
| **Profiel** | Een beperking op een FHIR-resource: welke velden en codes zijn toegestaan |
| **ValueSet** | Een lijst van toegestane codes |
| **Cohort** | De groep patiënten waarvoor een dataset wordt opgehaald |
| **Group** | De FHIR-resource waarin het cohort staat |
| **Parameters** | De FHIR-resource met het exportverzoek |
| **`_typeFilter`** | Een zoekvraag die bepaalt welke records worden opgehaald |
| **`_elements`** | De lijst van velden die worden vrijgegeven |
| **ART-DECOR** | Het systeem waarin landelijke en gedeelde codelijsten worden beheerd |
| **NTS** | De Nationale Terminologieserver van Nictiz |
