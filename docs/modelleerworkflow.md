# Modelleerworkflow SIM-on-FHIR

Gebruik deze werkwijze om een SIM Logical Model of Santeon FHIR-profiel aan te
passen of toe te voegen. `SIMPatient.fsh` en `PatientSan.fsh` zijn de
referentievoorbeelden voor de opbouw.

## 1. Bepaal de scope

Begin bij de indicator of het zorgpad waarvoor gegevens nodig zijn. Bepaal
welke SIM-elementen en relaties nodig zijn en bundel ze per bouwblok.
Controleer eerst of je een bestaand bouwblok kunt uitbreiden.

Leg de scope vast in het Logical Model onder `// IBD-scope`. Vermeld zo nodig
welke elementen buiten de huidige scope vallen.

## 2. Pas het Logical Model aan

Werk in `input/fsh/sim/`. Leg per element de betekenis, cardinaliteit, het
datatype en de eventuele terminologiebinding vast. Onderhoud de mappings in
deze volgorde.

| Mapping | Wat leg je vast? |
|---|---|
| 1. SIM CSV | Uit welk SIM-attribuut het element komt. |
| 2. Nictiz GBB | De semantische vergelijking met het GBB-afsprakenmodel en de gebruikte referentieversie. Gebruik 🟢 voor beoordeeld en passend, en 🟡 voor een verschil of open beoordeling. |
| 3. Santeon FHIR | Het FHIR-doelpad per SIM-element en de benodigde conversie of invulafspraak. Verwijs voor concrete vertaaltabellen naar de beheerde ETL- of transformatiespecificatie. |

Gebruik een specifiek doelpad als de aanleverafspraak een specifiek type
kiest, bijvoorbeeld `Observation.effectiveDateTime`. Als een mapping nog
ontbreekt, licht kort toe waarom.

## 3. Pas het FHIR-profiel aan

Werk in `input/fsh/fhir/`. Kies bij een nieuw profiel een passende parent.
Gebruik het toepasselijke Nictiz nl-core-profiel als parent waar dat past.
Gebruik anders de passende FHIR R4-basisresource.

Voeg alleen een profielregel toe voor een afgesproken FHIR-eis die de parent
nog niet afdekt. Denk aan cardinaliteit, toegestane datatypes,
terminologiebindingen of referentiedoelen. Voeg Must Support toe als daar een
concrete ondersteuningseis voor is afgesproken. Leg de manier waarop de ETL
een veld vult vast in Mapping 3 van het Logical Model.

Voorbeeld: in `ObservationSan` staat de aanlevering van `effectiveDateTime`
in de mapping. In `ObservationSanTabakGebruik` staan de meetcode en de
toegestane vorm van de rookstatus als profielregels.

Leg het specifieke FHIR-doelpad vast in Mapping 3 van het Logical Model,
bijvoorbeeld `Observation.effectiveDateTime`. De generator neemt dit pad over
in Mapping 1 van het profiel. Voeg alleen een profielregel toe als je ook
wilt afdwingen welke typen het profiel accepteert.

| Volgorde in het profiel | Wat leg je vast? |
|---|---|
| Profieldefinitie | Parent, scopecomment en eventuele aanvullende profielregels. |
| Mapping 1 | De gegenereerde SIM-herkomstmapping. Onderhoud bronpaden en conversietoelichtingen in Mapping 3 van het Logical Model. |
| Mapping 2 | De handmatige technische vergelijking met het passende Nictiz nl-core-profiel, inclusief referentieversie. Gebruik `Equivalent` na een afgeronde vergelijking zonder relevant verschil en 🟡 met uitleg bij een verschil of open beoordeling. Ontbreekt de vergelijking, vermeld dan kort waarom. |
| Status en openstaande controles | Eén `// Status:` per bestand en concrete `// TODO:`-regels bij open punten. |

Neem bij een nieuw profiel met een SIM-herkomstmapping de onderstaande
markeringen op vóór Mapping 2. De generator vult het blok.

```fsh
// BEGIN GENERATED SIM MAPPING
// END GENERATED SIM MAPPING
```

Leg een TODO vast waar de vraag wordt opgelost. Een vraag over de
ETL-invulling hoort bij het Logical Model. Een vraag over een FHIR-beperking
of een technische delta hoort bij het profiel.

## 4. Genereer en valideer

Draai vanuit de root van de repo de generator en SUSHI.

```powershell
py _generateFromDefinitions.py
sushi .
```

Gebruik `python3 _generateFromDefinitions.py` als je met `python3` werkt.

1. Controleer de diff. Mapping 1 staat in het profielbestand zelf, tussen
   de markeringen. Controleer of de generator de verwachte doelpaden en
   toelichtingen heeft overgenomen. Beoordeel waarschuwingen en overgeslagen
   mappings.
2. Draai de IG-build volgens de README. Controleer de gewijzigde modellen,
   profielen en mappings in de IG en beoordeel nieuwe QA-meldingen.
3. Controleer bij een nieuwe of gewijzigde indicator of de betrokken
   bouwblokken samen de benodigde gegevens leveren en de selectie werkt.
4. Werk de status per Logical Model en per profiel bij. Gebruik `Validated`
   zodra de relevante inhoudelijke en technische controles zijn afgerond.