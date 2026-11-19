# Modelleerworkflow en documentatieconventies SIM-on-FHIR

## 1. Doel en uitgangspunten

Dit document beschrijft de vaste werkwijze voor het modelleren van SIM-on-FHIR-bouwblokken en het vastleggen van herkomst, harmonisatie, delta-analyses, transformaties en besluiten.

**Uitgangspunten**

- Het **Santeon Informatiemodel (SIM)** is inhoudelijk leidend. Het Logical Model (LM) beschrijft de geselecteerde SIM-gegevens en hun betekenis.
- Het Santeon **FHIR R4-profiel** legt de gestandaardiseerde doelrepresentatie en toepasselijke constraints vast, zoveel mogelijk aansluitend op FHIR en Nictiz.
- Een verschil met Nictiz is niet automatisch een fout; afwijkingen worden beoordeeld en waar nodig onderbouwd.
- Iedere inhoudelijke toelichting en conversieregel heeft **één beheerlocatie**. Elders wordt ernaar verwezen.
- **Hergebruik vóór nieuwbouw:** bestaande Santeon-mappings en transformaties worden eerst beoordeeld; alleen ontbrekende of noodzakelijke conversies worden toegevoegd.
- De huidige afbakening is IBD. Voor andere zorgpaden kan dezelfde werkwijze worden hergebruikt, met opnieuw vastgestelde scope en inhoudelijke constraints.

## 2. Stappenplan per bouwblok

Doorloop voor ieder bouwblok dezelfde zes stappen. **Het SIM is inhoudelijk leidend.**

1. **Scope vaststellen**  
   Bepaal welke SIM-elementen en relaties nodig zijn op basis van de IBD-indicatoren, definities, `sb-ibd` en HIPS ETL.  
   *Resultaat: afgebakende IBD-scope.*

2. **Santeon Logical Model opstellen**  
   Modelleer de geselecteerde SIM-gegevens in FSH en leg de herkomst vast via **mapping 1: Bron (SIM CSV)**. Beschrijf ook de vertaling naar Santeon FHIR via **mapping 3: LM → FHIR (technische vertaling, harmonisatie en conversie)**.  
   *Resultaat: Santeon LM met herkomst en beoogde FHIR-vertaling.*

3. **Semantische delta-analyse uitvoeren**  
   Vergelijk het LM met het Nictiz GBB-afsprakenmodel. Leg overeenkomsten, verschillen en onderbouwing vast in **mapping 2: LM → GBB**.  
   *Resultaat: gedocumenteerde semantische delta.*

4. **Santeon FHIR-profiel uitwerken**  
   Vertaal het LM naar FHIR R4 en definieer de noodzakelijke cardinaliteiten, datatypes, ValueSets en constraints. Hergebruik bestaande Santeon-conversies waar mogelijk.  
   *Resultaat: FHIR-profiel in FSH.*

5. **Technische delta-analyse uitvoeren**  
   Vergelijk het Santeon FHIR-profiel met Nictiz nl-core. Leg relevante technische verschillen vast in **mapping 1 bij het FHIR-profiel: FHIR → nl-core**. Verwijs naar de LM-toelichting bij een semantische oorzaak.  
   *Resultaat: gedocumenteerde technische delta.*

6. **Besluiten verwerken en consistentie valideren**  
   Beoordeel openstaande delta's, verwerk besluiten, controleer LM ↔ FHIR en valideer met SUSHI, FHIR-validator en IG Publisher.  
   *Resultaat: bijgewerkte en op samenhang gecontroleerde modellen; technische validatie en resterende TODO’s zijn expliciet zichtbaar.*

**Vaste mappingvolgorde:** LM = **1. Bron (SIM CSV) → 2. Nictiz GBB → 3. Santeon FHIR**; FHIR-profiel = **1. Nictiz nl-core**.

**Iteratief waar nodig:** leidt stap 3 of 5 tot een wijziging, pas dan het betreffende model aan en controleer de samenhang opnieuw. De uitwerking en delta's zijn pas volledig afgerond als de relevante controles zijn uitgevoerd.

## 3. Verantwoordelijkheid en bron van waarheid

| Onderdeel | Beheerlocatie | Verantwoordelijkheid |
|---|---|---|
| SIM-herkomst | LM → SIM CSV-mapping | Herleidbaarheid naar oorspronkelijke SIM-attributen. |
| Semantische delta | LM → Nictiz GBB-afsprakenmodel-mapping | Verschillen in betekenis, definities, structuur, cardinaliteiten en terminologie. |
| Technische vertaling, harmonisatie en conversie | LM → Santeon FHIR-mapping | Welk SIM-element naar welk FHIR-element gaat; waarom harmonisatie of conversie nodig is. |
| FHIR-representatie | Santeon FHIR-profiel | Datatypes, cardinaliteiten, ValueSets, constraints, slicing, extensions en andere profielafspraken. |
| Technische delta | Santeon FHIR → Nictiz nl-core-mapping | Verschillen tussen de FHIR-profielen, zonder semantische analyse te dupliceren. |
| Concrete waardetransformatie | Bestaande Santeon-mapping / ETL- of transformatiespecificatie; zo nodig ConceptMap | Broncode → doelcode, uitzonderingen, omgang met onbekende waarden en versiebeheer. |
| Modelleerstatus | `// Status:` onderaan ieder LM- en FHIR-FSH-bestand | Onafhankelijke, handmatig gekozen status per definitie; de generator toont per pagina de eigen status. |
| Openstaande controles | `// TODO:` in het betreffende LM- of FHIR-FSH-bestand | Open controles bij het relevante element of de mapping; generator telt de TODO’s. |
| Besluitvorming en opvolging | Azure DevOps, indien nodig | Afwegingen, eigenaarschap, opvolging en besluitvorming; definitieve keuzes worden ook verwerkt in de betreffende specificatie. |

Een FHIR-profiel beschrijft de eisen aan een resource die aan dat profiel moet voldoen. Afspraken over **welke resources, wanneer en via welke interface** een ziekenhuis moet aanleveren, horen daarnaast in een aparte aanlever- of implementatiespecificatie.

### 3.1 Hergebruik van bestaande conversies

1. Controleer of in het SIM, HIPS, ETL1, ETL2 of een andere beheerde Santeon-specificatie al een geschikte conversie bestaat.
2. Verwijs vanuit de LM → FHIR-mapping naar de benodigde conversie; leg de volledige vertaaltabel niet opnieuw vast in FSH-comments.
3. Voeg alleen een nieuwe, centraal beheerde transformatiespecificatie of FHIR ConceptMap toe als de bestaande conversie ontbreekt of niet voldoet.
4. Spreek bij FHIR-aanlevering expliciet af **waar** de conversie plaatsvindt: bij het ziekenhuis of centraal bij Santeon. Een ConceptMap alleen voert geen conversie uit.

**Voorbeeld `Geslacht`:** `SIMPatient.Geslacht` gebruikt de SIM/ZIB-codelijst; `PatientSan.gender` gebruikt FHIR AdministrativeGender. De LM → FHIR-mapping vermeldt dat een terminologieconversie nodig is; de concrete broncode-doelcodetabel staat in een bestaande of nieuwe beheerde transformatiespecificatie. Het FHIR-profiel definieert de toegestane doelwaarden.

## 4. Standaard FSH-structuur en naamgeving

De bestanden `SIMPatient.fsh` en `PatientSan.fsh` zijn het **referentiepatroon voor structuur en schrijfstijl**. De inhoudelijke constraints worden per bouwblok vastgesteld; kopieer deze niet automatisch naar andere modellen.

### 4.1 Logical Model (FSH)

**Vaste volgorde:**

1. Modeldefinitie: `Logical`, `Parent`, optionele verklarende comment, `Id`, `Title`, `Description`.
2. `// IBD-scope`: actieve elementen inclusief cardinaliteit, datatype en definitie.
3. `// Buiten IBD-scope - nog niet meegenomen`: eventueel uitgecommentarieerde, niet-geïmplementeerde SIM-elementen.
4. `// Terminologie`: actieve bindingen; niet-gebruikte bindingen uitcommentariëren.
5. **Mapping 1:** herkomst – Bron (SIM CSV).
6. **Mapping 2:** semantische delta-analyse – Nictiz GBB-afsprakenmodel.
7. **Mapping 3:** technische vertaling, harmonisatie en conversie – Santeon FHIR-profiel.
8. **Status en openstaande controles:** onderaan één `// Status:` en bij voorkeur de `// TODO:`-controles; plaats elementspecifieke TODO’s waar nodig direct bij het betreffende onderdeel.

| Volgorde | Voorbeeldnaam | Doel | Statusindicatoren |
|---|---|---|---|
| Mapping 1 | `SIMPatientFromSIMCSV` | Herkomst uit SIM CSV (weergavelabel: Bron). | Nee |
| Mapping 2 | `SIMPatientToGBB` | Semantische delta met GBB-afsprakenmodel. | Ja |
| Mapping 3 | `SIMPatientToFHIR` | Technische vertaling, harmonisatie en benodigde conversies naar `PatientSan`. | Nee |

**Vaste kopteksten:**

```fsh
// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

// ============================================
// MAPPING 2: Semantische delta-analyse - SIMPatient naar Nictiz GBB-afsprakenmodel
// ============================================

// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMPatient naar Santeon FHIR PatientSan
// ============================================
```

### 4.2 Santeon FHIR-profiel (FSH)

**Vaste volgorde:**

1. Profieldefinitie: `Profile`, `Parent`, optionele verklarende comment, `Id`, `Title`, `Description`.
2. `// IBD-scope`: actieve, inhoudelijk onderbouwde FHIR-constraints.
3. `// Buiten IBD-scope - nog niet meegenomen`: uitleg over niet-gemodelleerde gegevens.
4. `// Terminologie`: relevante FHIR-ValueSets en bindings.
5. **Mapping 1:** technische delta-analyse – Nictiz nl-core FHIR-profiel.
6. **Status en openstaande controles:** onderaan één `// Status:` en bij voorkeur de `// TODO:`-controles; plaats technische TODO’s waar nodig direct bij het betreffende element of de delta-mapping. De profielstatus staat los van die van het Logical Model.

| Volgorde | Voorbeeldnaam | Doel | Statusindicatoren |
|---|---|---|---|
| Mapping 1 | `PatientSanToNlCore` | Technische delta met Nictiz `nl-core-Patient`. | Ja |

**Vaste koptekst:**

```fsh
// ============================================
// MAPPING 1: Technische delta-analyse - PatientSan naar Nictiz nl-core
// ============================================
```

Een standaard FHIR-element dat buiten de huidige IBD-scope valt, wordt **niet automatisch uitgesloten met `0..0`**. Uitsluiting vraagt een expliciete inhoudelijke of technische onderbouwing. De standaarddefinities en bindings worden geërfd van het parent-profiel; relevante bindings kunnen omwille van leesbaarheid expliciet worden herhaald zonder de betekenis te wijzigen.

### 4.3 Vaste vorm van iedere mapping

1. Koptekst: `MAPPING [nummer]: [functie] - [bron] naar [doel]` (waar toepasselijk).
2. `Mapping`, `Id`, `Title`, `Description`.
3. `Source` en `Target`.
4. `// IBD-scope`: actieve elementmappings.
5. `// Buiten IBD-scope - nog niet meegenomen` of `// Buiten IBD-scope - nog te beoordelen`: uitsluitend waar relevant, met uitgecommentarieerde mappingregels of aandachtspunten.

**Naamgeving:** `Id` blijft stabiel; `Title` noemt kort het doelmodel, bijvoorbeeld `Nictiz GBB-afsprakenmodel`, `Santeon FHIR PatientSan` of `Nictiz nl-core-Patient`. De `Description` vermeldt het doel, bij delta-analyses ook de gebruikte referentieversie en legenda.

**Mappingcomments:** kort, elementspecifiek en gericht op een relevante overeenkomst, afwijking, benodigde conversie of open vraag. Gebruik bij voorkeur consistente formuleringen zoals `Equivalent:`, `Verschil in ...:` en `Nog te beoordelen:`.

## 5. Delta-statussen en documentatie van verschillen

Gebruik statusindicatoren **alleen** bij de LM → GBB- en FHIR → nl-core-delta-analyses:

| Indicator | Betekenis |
|---|---|
| 🟢 | **Beoordeeld:** equivalent of compatibel, zonder relevante afwijking. |
| 🟡 | **Verschil of onzekerheid:** verschil vastgesteld of nader te beoordelen. |

Gebruik **geen rood** als standaardstatus. Groen betekent dat de relevante vergelijking daadwerkelijk is uitgevoerd; een gelijke elementnaam alleen is niet voldoende bewijs. Een afwijking hoeft niet te worden opgelost als zij inhoudelijk gerechtvaardigd is.

### 5.1 Voorkom dubbele delta-uitleg

- **LM → GBB:** hier hoort de semantische onderbouwing thuis.
- **LM → FHIR:** hier hoort de noodzakelijke harmonisatie of conversie thuis.
- **FHIR-profiel:** hier staan de concrete FHIR-constraints die de validator kan controleren.
- **FHIR → nl-core:** hier staan uitsluitend de technische profielverschillen. Een semantische oorzaak wordt zo nodig via een verwijzing naar de LM-mapping ontsloten.
- **Transformatiespecificatie:** hier staan de concrete vertaaltabellen en uitvoeringsregels.

De mappingcomment beschrijft **wat** verschilt of moet worden vertaald, niet telkens opnieuw **hoe** de volledige ETL dit uitvoert. Definities en besluiten worden niet gekopieerd over verschillende documenten.

### 5.2 Referentieversie

Leg de gebruikte versie van het Nictiz GBB-afsprakenmodel en het nl-core-profiel expliciet vast in de `Description` van de betreffende delta-mapping. Het `SIMPatient`/`PatientSan`-voorbeeld gebruikt `1.0.0-alpha.2`; bij andere bouwblokken wordt de daadwerkelijk vergeleken versie vermeld.

## 6. Consistentiecontrole tussen LM en FHIR

Het Logical Model is inhoudelijk leidend. Controleer vóór afronding van ieder bouwblok:

| Controle | Vraag | Vastlegging / vervolg |
|---|---|---|
| Scope | Zijn alle actieve LM-elementen binnen de vastgestelde scope afgedekt in de LM → FHIR-mapping en het profiel? | Ontbrekende of afwijkende keuzes aanvullen of onderbouwen. |
| Betekenis | Behouden de FHIR-elementen de semantiek van de SIM-elementen? | Semantische keuzes in LM → GBB; vertaling in LM → FHIR. |
| Cardinaliteit | Zijn minimale en maximale aantallen consistent of bewust aangescherpt? | Constraints in FHIR; motivering bij relevante verschilpunten. |
| Datatype en precisie | Is omzetting tussen SIM- en FHIR-datatypes verantwoord, zonder informatie te verzinnen of betekenis te verliezen? | LM → FHIR-toelichting en eventuele transformatiespecificatie. |
| Terminologie | Zijn SIM-ValueSets en FHIR-bindings correct; is conversie nodig en beheerd? | Bindings in LM/FHIR, noodzaak bij LM → FHIR, codetabel centraal. |
| Referenties en structuur | Kloppen mappingpaden, choice-elementen, slicing en referentieprofielen? | FSH-mappings en profielregels controleren. |
| Delta-status | Zijn 🟢-claims volledig gecontroleerd en zijn 🟡-punten concreet geformuleerd? | Delta-mapping bijwerken. |
| Dubbelingen | Staat de onderbouwing op precies één beheerlocatie? | Overige teksten beperken tot verwijzingen. |
| Buiten scope | Is duidelijk wat niet is meegenomen, zonder standaard-FHIR-elementen onnodig te verbieden? | Scopecommentaar en profielconstraints. |
| Technische validatie | Compileren FSH en IG en valideren de profielen? | SUSHI, FHIR-validator en IG Publisher draaien. |

### 6.1 Referentievoorbeeld SIMPatient ↔ PatientSan

| SIMPatient (LM) | PatientSan (FHIR) | Vertaling / aandachtspunt |
|---|---|---|
| `Identificatienummer` – `1..* Identifier` | `identifier 1..*`, `identifier.value 1..1` | De FHIR-waarde is aanvullend verplicht; controleer dat dit aansluit op de aanlevering. |
| `Geslacht` – `0..1 CodeableConcept` | `gender 0..1`, AdministrativeGender (`required`) | Terminologieconversie vereist; concrete codemapping centraal beheren. |
| `Geboortedatum` – `0..1 date` | `birthDate 0..1` | Directe representatie. |
| `DatumOverlijden` – `0..1 date` | `deceased[x] only dateTime` | Dagprecisie behouden; geen onbekend tijdstip aanvullen. |

Dit voorbeeld bepaalt de **documentatieconventie**, niet de inhoudelijke constraints voor andere bouwblokken.

### 6.2 Modelleerstatus en openstaande controles

Beheer de **status afzonderlijk per Logical Model en FHIR-profiel**. Plaats één `// Status:`-comment **altijd onderaan het eigen FSH-bestand, na de mappingdefinities**, in de vaste sectie `Status en openstaande controles`. Plaats de openstaande `// TODO:`-controles bij voorkeur in dezelfde sectie, maar zet een elementspecifieke TODO waar nodig direct bij het betreffende element of de mapping. Er is geen aparte statusconfiguratie in de generator nodig.

| Status (`// Status:`) | Betekenis |
|---|---|
| `Not reviewed` | Model of profiel is aanwezig, maar nog niet getoetst aan de modelleerconventie. Standaard bij ontbrekende statuscomment. |
| `In progress` | De betreffende definitie en/of bijbehorende mappings worden uitgewerkt of herzien. |
| `Ready for validation` | Inhoudelijke modellering en documentatie zijn uitgewerkt; controles of technische validatie kunnen nog openstaan. |
| `Validated` | De afgesproken inhoudelijke beoordelingen en technische validaties zijn afgerond en hun resultaat is gecontroleerd. |

**Afspraken:**

- Leg `// Status:` **precies één keer en altijd onderaan ieder afzonderlijk LM- en FHIR-bestand** vast, na de mappings. De generator toont de LM-status op `CLMs.md` en de FHIR-status op `profiles.md`; neem de status nooit automatisch over van het gekoppelde model.
- Leg `// TODO:`-controles bij voorkeur onderaan in de vaste sectie vast. Als een controle bij één element of mapping hoort, mag de TODO direct daarbij staan; de generator telt TODO’s onafhankelijk van hun locatie in het betreffende bestand. Gebruik niet `// TODO: -` als er geen openstaande controle is; laat de regel dan weg.
- De generator leest de status uit de betreffende FSH-bron en telt de werkelijke `// TODO:`-regels **per definitie**. Hij leidt de status niet af uit mappings, TODO’s of de status van een gekoppelde definitie.
- Verwijder afgeronde TODO’s nadat de uitkomst is verwerkt in het model, de mapping of de relevante specificatie.
- `Ready for validation` laat openstaande TODO’s toe en is geen bewijs van volledige validatie. `Validated` mag alleen na aantoonbaar uitgevoerde en beoordeelde relevante controles; alleen een statuscomment is geen bewijs.
- Houd **modelleerstatussen** (`Not reviewed` enz.) gescheiden van **delta-indicatoren** (🟢/🟡 per elementmapping).

**Voorbeeld `SIMPatient` en `PatientSan`:** beide hebben een eigen `// Status:`. `SIMPatient` kan bijvoorbeeld `Ready for validation` hebben terwijl `PatientSan` nog `In progress` is. Identifier-slicing en nl-core obligations blijven als `// TODO:` in `PatientSan.fsh` totdat ze zijn beoordeeld. Dit zijn voorbeelden, geen automatisch toegekende statussen.

**Voorbeeld onderaan `SIMPatient.fsh`:**

```fsh
// ============================================
// Status en openstaande controles
// ============================================

// Status: Ready for validation
```

**Voorbeeld onderaan `PatientSan.fsh`:**

```fsh
// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: Controleer identifier-slicing met Nictiz.
// TODO: Beoordeel aanvullende nl-core obligations.
```

## 7. Besluiten, validatie en afronding

1. Leg openstaande controles als `// TODO:` in het betreffende FSH-bestand vast en werk de **eigen** `// Status:` van het LM of FHIR-profiel bij.
2. Gebruik Azure DevOps indien afstemming, eigenaarschap, opvolging of formele besluitvorming nodig is.
3. Verwerk definitieve besluiten in de **betreffende bron van waarheid**: Logical Model, FHIR-profiel, mapping of transformatiespecificatie.
4. Controleer opnieuw de samenhang tussen SIM, LM-mappings, FHIR-profiel en Nictiz-delta’s.
5. Valideer FSH met SUSHI, beoordeel de gegenereerde FHIR-artefacten met een validator en bouw de IG met de IG Publisher.
6. Verwijder afgehandelde TODO’s en ken `Validated` **per afzonderlijke definitie** pas toe na aantoonbare beoordeling van de afgesproken controles.

**Kernprincipe:** *documenteer een keuze waar die inhoudelijk ontstaat, definieer eisen waar ze worden afgedwongen, hergebruik bestaande conversies en verwijs elders naar de centrale specificatie.*
