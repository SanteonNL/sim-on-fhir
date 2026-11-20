Alias: $NZaZorgactiviteit = https://declaratie.nza.nl/zorgactiviteit

ValueSet: IbdVerrichtingCodesNZa
Id: ibd-verrichting-codes-nza
Title: "Verrichting Type Codes NZa (sb-ibd)"
Description: """
NZa-zorgactiviteiten die de sb-ibd-dataset selecteert: operaties (abces, colectomie, ileocaecale resectie,
hemicolectomie), verpleegdagen, dagbehandelingen, verkeerde beddagen, fysieke consulten, teleconsulten, scopieën,
MRI's, echo's, biopten en CT-scans.

Bron: HipsETL datasetDefinities/dataset_sb-ibd.yaml, datamodel Verrichting (VerrichtingTypeCodeNZa).
De scopiecodes van [local-verrichting-codes-nza](ValueSet-local-verrichting-codes-nza.html) zijn hier een deelverzameling van.
"""
* ^status = #active
* ^experimental = false

// Operaties
* $NZaZorgactiviteit#038853
* $NZaZorgactiviteit#035121
* $NZaZorgactiviteit#035122

// Verpleegdagen, dagbehandelingen en verkeerde beddagen
* $NZaZorgactiviteit#034732
* $NZaZorgactiviteit#034733
* $NZaZorgactiviteit#034734
* $NZaZorgactiviteit#034735
* $NZaZorgactiviteit#034752
* $NZaZorgactiviteit#034753
* $NZaZorgactiviteit#034638
* $NZaZorgactiviteit#034639
* $NZaZorgactiviteit#034738
* $NZaZorgactiviteit#034739
* $NZaZorgactiviteit#034792
* $NZaZorgactiviteit#034793
* $NZaZorgactiviteit#034796
* $NZaZorgactiviteit#034797
* $NZaZorgactiviteit#034798
* $NZaZorgactiviteit#034799
* $NZaZorgactiviteit#034808
* $NZaZorgactiviteit#034809
* $NZaZorgactiviteit#034810
* $NZaZorgactiviteit#034811
* $NZaZorgactiviteit#034880

// Consulten / zorgactiviteiten
* $NZaZorgactiviteit#190204
* $NZaZorgactiviteit#190205
* $NZaZorgactiviteit#190218
* $NZaZorgactiviteit#190031
* $NZaZorgactiviteit#190034
* $NZaZorgactiviteit#190035
* $NZaZorgactiviteit#190038
* $NZaZorgactiviteit#190055
* $NZaZorgactiviteit#190090
* $NZaZorgactiviteit#190091
* $NZaZorgactiviteit#190092
* $NZaZorgactiviteit#190093
* $NZaZorgactiviteit#190012
* $NZaZorgactiviteit#190013
* $NZaZorgactiviteit#190015
* $NZaZorgactiviteit#190016
* $NZaZorgactiviteit#190060
* $NZaZorgactiviteit#190063
* $NZaZorgactiviteit#190066
* $NZaZorgactiviteit#190019
* $NZaZorgactiviteit#190025
* $NZaZorgactiviteit#190153
* $NZaZorgactiviteit#190154
* $NZaZorgactiviteit#190155
* $NZaZorgactiviteit#190157
* $NZaZorgactiviteit#190158
* $NZaZorgactiviteit#190161
* $NZaZorgactiviteit#190162
* $NZaZorgactiviteit#190163
* $NZaZorgactiviteit#190164
* $NZaZorgactiviteit#190165
* $NZaZorgactiviteit#190166
* $NZaZorgactiviteit#190167

// Scopieën
* $NZaZorgactiviteit#034620
* $NZaZorgactiviteit#034686
* $NZaZorgactiviteit#034690
* $NZaZorgactiviteit#035582

// Careplan
* $NZaZorgactiviteit#039133

// MRI
* $NZaZorgactiviteit#087090
* $NZaZorgactiviteit#087091
* $NZaZorgactiviteit#087096
* $NZaZorgactiviteit#087097

// Echo
* $NZaZorgactiviteit#039492

// Biopten
* $NZaZorgactiviteit#035280

// CT-scans
* $NZaZorgactiviteit#080043
* $NZaZorgactiviteit#089142
* $NZaZorgactiviteit#089042
* $NZaZorgactiviteit#087043
* $NZaZorgactiviteit#087042
