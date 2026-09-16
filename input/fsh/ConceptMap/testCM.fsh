Instance: ConceptMapTest
InstanceOf: ConceptMap
Usage: #definition

* url = "https://ig.santeon.nl/ibd/ConceptMap/ConceptMapTest"
* name = "ConceptMapTest"
* title = "ConceptMap Test"
* status = #draft

//* group[+].source = "https://ig.santeon.nl/ibd/CodeSystem/TestMetingCodes"
//* group[=].target = "http://loinc.org"

* group[+].element[+].code = #A
* group[=].element[=].target[+].code = #B
* group[=].element[=].target[=].equivalence = #equivalent