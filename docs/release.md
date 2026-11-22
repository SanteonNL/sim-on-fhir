# Een release maken

Dit document legt uit hoe je een versie van SIM on FHIR uitbrengt en hoe je een FHIR-package met een versienummer maakt. Het werkt zoals GoReleaser in onze Go-projecten (bijvoorbeeld `fenix`): **de git-tag is het versienummer**. Je past het versienummer nergens met de hand aan.

## In één alinea

Je zet een tag op een commit, bijvoorbeeld `v0.2.0-rc.1`. Het script [_release.py](../_release.py) leest die tag, zet het versienummer tijdens de build in `sushi-config.yaml` en zet dat daarna weer terug. Het FHIR-package krijgt zo het juiste versienummer, in de bestandsnaam en in het `package.json` erin. Het resultaat komt in de map `dist/`.

## Welk versienummer krijg je?

| Situatie | Versienummer | Label in de IG |
|---|---|---|
| Tag `v0.2.0` | `0.2.0` | release |
| Tag `v0.2.0-rc.1` | `0.2.0-rc.1` | release candidate |
| Geen tag op de commit | `0.1.0-snapshot-<commit>`, bijv. `0.1.0-snapshot-871a5ca` | ci-build |

Zonder tag komt het basisnummer uit `version:` in `sushi-config.yaml`, met `-snapshot-` en de korte commit-code erachter. Zo zie je aan de naam altijd dat het geen echte release is, en van welke commit het komt.

Een tag moet de vorm `vX.Y.Z` of `vX.Y.Z-rc.N` hebben. Een andere vorm (zoals `v0.2`) geeft een foutmelding.

Wil je weten welk versienummer de huidige commit krijgt?

```powershell
py -3 _release.py version
```

Zonder commando (`py -3 _release.py`) toont het script een overzicht van alle commando's en het versienummer van de huidige commit.

## Stap voor stap: een release candidate lokaal maken

**1. Commit al je werk.** Een tag hoort bij een commit. Wat niet gecommit is, zit niet in de release.

```powershell
git status          # moet leeg zijn (op __pycache__ na)
```

**2. Zet de tag.** Kies het volgende nummer: `rc.1` voor de eerste kandidaat, `rc.2` na een fix, en zonder `-rc` voor de definitieve versie.

```powershell
git tag -a v0.2.0-rc.1 -m "sim-on-fhir v0.2.0-rc.1"
```

**3. Maak het package.** Er zijn twee manieren:

| | Snel package (aanbevolen) | Volledige build |
|---|---|---|
| Commando | `py -3 _release.py pack` (met snapshots: `pack --snapshot`) | `py -3 _buildIG.py` (sneller: `--no-tx`) |
| Duur | ongeveer 30 seconden | vele minuten |
| Wat het doet | alleen SUSHI | SUSHI + HL7 IG Publisher + website |
| In `dist/` | `santeon.sim-on-fhir-<versie>.tgz`, `checksums.txt` | dezelfde `.tgz` (volledig), `sim-on-fhir_<versie>_site.zip`, `checksums.txt` |
| Inhoud package | profielen, ValueSets en instances zoals SUSHI ze maakt (*differentials*; met `--snapshot` ook snapshots) | snapshots, narratives en ValueSet-expansies |

Het snelle package is genoeg voor de meeste tools: validators en servers rekenen de snapshots zelf uit. Heeft een tool de snapshots in het package nodig, gebruik dan `py -3 _release.py pack --snapshot`: SUSHI schrijft ze dan zelf uit (een paar seconden extra). Kies de volledige build als je de website nodig hebt of het officiële package van de IG Publisher.

**4. Controleer het resultaat.**

```powershell
dir dist
```

Je ziet bijvoorbeeld `santeon.sim-on-fhir-0.2.0-rc.1.tgz`. `checksums.txt` bevat de SHA-256 van elk bestand, zodat een ontvanger kan controleren dat het bestand niet veranderd is.

**5. Definitieve versie.** Is de release candidate goedgekeurd, tag dan dezelfde commit (of een latere met fixes) zonder `-rc`:

```powershell
git tag -a v0.2.0 -m "sim-on-fhir v0.2.0"
py -3 _release.py pack
```

## Via GitHub (later)

Push je een tag naar GitHub (`git push origin v0.2.0-rc.1`), dan bouwt de workflow [publish-ig.yml](../.github/workflows/publish-ig.yml) de volledige IG en maakt een GitHub Release met de bestanden uit `dist/`. Een tag met `-rc.N` wordt een *pre-release*.

**Werkt nu nog niet.** ART-DECOR (`decor.nictiz.nl`) geeft aan de servers van GitHub een 404 voor de vastgepinde externe definities (`careplan-category`, `alcohol-use-status`, `luscii-prems`). Lokaal werkt het wel. Tot dit opgelost is (bijvoorbeeld door die drie bestanden in de repository op te nemen), maak je releases lokaal zoals hierboven.

Let op: een tag die je naar GitHub hebt gepusht, verplaats je niet meer. Heb je een fout gemaakt, maak dan een nieuwe tag (`rc.2`).

## Wat je doet bij...

**"tag 'v0.2' is not vMAJOR.MINOR.PATCH ..."** — De tag heeft de verkeerde vorm. Verwijder hem (`git tag -d v0.2`) en zet een tag als `v0.2.0` of `v0.2.0-rc.1`.

**Het package heeft een snapshot-versie terwijl je een tag hebt gezet** — De tag staat niet op de commit waar je nu bent. Controleer met `git describe --tags --exact-match`. Heb je na het taggen nog iets gecommit, zet dan een nieuwe tag.

**`sushi-config.yaml` staat als gewijzigd na een afgebroken build** — Het script zet het bestand normaal zelf terug. Na een afgebroken build herstel je het met `git checkout -- sushi-config.yaml`.

**`sushi not found`** — Installeer SUSHI: `npm install -g fsh-sushi`.

## Woordenlijst

| Term | Betekenis |
|---|---|
| **Tag** | Een vast label op één commit, bijv. `v0.2.0`. Bepaalt het versienummer. |
| **Release candidate (rc)** | Een kandidaat-versie die nog getest wordt, bijv. `0.2.0-rc.1`. |
| **Snapshot** (versie) | Een build zonder tag; het versienummer bevat de commit-code. |
| **FHIR-package** (`.tgz`) | Het bestand met alle profielen, ValueSets en instances, zoals FHIR-tools het inlezen. |
| **Differential / snapshot** (profiel) | Een differential beschrijft alleen wat een profiel verandert; een snapshot is het volledige uitgeschreven profiel. |
| **`dist/`** | De map waarin de releasebestanden komen. Staat niet in git. |
