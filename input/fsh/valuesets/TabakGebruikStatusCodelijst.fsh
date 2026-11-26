// Aliassen $SCT en $NullFlavor staan in AlcoholGebruikStatusCodelijst.fsh.

ValueSet: TabakGebruikStatusCodelijst
Id: TabakGebruikStatusCodelijst
Title: "TabakGebruikStatusCodelijst"
Description: "Codelijst voor de rookstatus van een patiënt, zoals gebruikt in het Santeon Informatiemodel (SIM-versie 2024-04-24)."

* $NullFlavor#OTH "Other"
* $SCT#449868002 "Rookt dagelijks tabak"
* $SCT#428041000124106 "Rookt soms tabak"
* $SCT#43381005 "Rookt passief tabak"
* $SCT#8517006 "Rookte vroeger"
* $SCT#405746006 "Niet-roker, maar rookgedrag in verleden onbekend"
* $SCT#266919005 "Heeft nooit gerookt"

// Afwijking van Nictiz zib TabakGebruik v3.2 (2020):
// "rookt soms" is daar 230059006 (Rookt soms sigaretten);
// het SIM gebruikt 428041000124106 (Rookt soms tabak).
