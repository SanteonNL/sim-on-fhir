// =====================================================================
// sb-ibd.fsh
// The sb-ibd dataset: a reference to its Group requirement (in DataRequirements.fsh) and the data requirements whose data it
// releases. One file per dataset in this folder; each dataset becomes one
// export on one Group. Generated from this file: the Group <id>-cohort, the
// Parameters <id>-export, one profile per resource type (<Id><Resource>,
// derived from the SAN profile) and the page <id>.html.
// =====================================================================

Instance: sb-ibd
InstanceOf: DatasetDefinition
Usage: #definition
Title: "sb-ibd"
Description: "Dataset for adult IBD patients."
* id = "sb-ibd"
* version = "0.1.0"
* title = "sb-ibd"
* description = "Gegevensset voor volwassen patiënten met inflammatoire darmziekten (IBD): verrichtingen (waaronder scopieën), calprotectinemetingen en de Luscii PREMs."
* group = "sb-ibd-group"

* requirement[+] = "k312-scopie"
* requirement[+] = "k363-calprotectin-before-scopie"
* requirement[+] = "luscii-prems"

