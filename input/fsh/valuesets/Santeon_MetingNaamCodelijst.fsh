Alias: $LOINC = http://loinc.org

ValueSet: Santeon_MetingNaamCodelijst
Id: Santeon-MetingNaamCodelijst
Title: "Santeon_MetingNaamCodelijst"
Description: "ValueSet met metingnamen die binnen het Santeon Informatiemodel worden gebruikt."

//* ^status = #active
//* ^experimental = false

* $LOINC#38445-3 "Calprotectin [Mass/volume] in Stool"
* http://snomed.info/sct#228273003

// IBD-scope op basis van de indicator-YAML's (volwassenen, indicatoren met analyse)

// BMI, lichaamsgewicht en lengte
* $LOINC#39156-5
* $LOINC#29463-7
* $LOINC#8302-2

// Laboratoriumbepalingen (K5, K5_window)
* $LOINC#59260-0
* $LOINC#787-2
* $LOINC#6690-2
* $LOINC#26464-8
* $LOINC#777-3
* $LOINC#26515-7
* $LOINC#1743-4
* $LOINC#1742-6
* $LOINC#77144-4
* $LOINC#30239-8
* $LOINC#1920-8
* $LOINC#6768-6
* $LOINC#77141-0
* $LOINC#2324-2
* $LOINC#1798-8
* $LOINC#3040-3
* $LOINC#14804-9
* $LOINC#14682-9
* $LOINC#77140-2
* $LOINC#14631-6
* $LOINC#1988-5

// Genotypering (nudt15, tpmt, thiopurines_genotypering)
* $LOINC#93195-6
* $LOINC#79713-4

// Biological-spiegels (P3)
* $LOINC#86897-6
* $LOINC#90779-0
* $LOINC#39803-2
* $LOINC#74117-3

// Montreal-classificatie (moncu_e, moncu_s, monzvc_a, monzvc_b, monzvc_l)
* https://santeon.nl/sct#MCU_E
* https://santeon.nl/sct#MCU_S
* https://santeon.nl/sct#MCD_A
* https://santeon.nl/sct#MCD_B
* https://santeon.nl/sct#MCD_L

// Physician Global Assessment (pga)
* https://santeon.nl/sct#MayoEndoscopicScore
// Rookstatus (rok); vaste Observation.code in ObservationSanTabakGebruik,
// conform Nictiz zib-TobaccoUse
* http://snomed.info/sct#365980008
