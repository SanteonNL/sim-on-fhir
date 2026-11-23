Alias: $NZaZorgactiviteit = urn:oid:2.16.840.1.113883.2.4.3.27.15.5

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
* $NZaZorgactiviteit#038853 "(Poli-)klinische behandeling van grote diepe abcessen zoals mastitis of pectoraal phlegmone, hand- en voetphlegome, panaritium (uitsluitend met incisie en drainage), perianaal abces en peritonsillair"
* $NZaZorgactiviteit#035121 "Operatie van een fistula ani."
* $NZaZorgactiviteit#035122 "Operatie recidief fistula ani."

// Verpleegdagen, dagbehandelingen en verkeerde beddagen
* $NZaZorgactiviteit#034732 "Totale colectomie plus rectumamputatie, open procedure (zie 034734 voor endoscopisch)."
* $NZaZorgactiviteit#034733 "Totale colectomie met ileorectale anastomose, open procedure (zie 034735 voor endoscopisch)."
* $NZaZorgactiviteit#034734 "Endoscopische totale colectomie plus rectumamputatie (zie 034732 voor open procedure)."
* $NZaZorgactiviteit#034735 "Endoscopische totale colectomie met ileorectale anastomose (zie 034733 voor open procedure)."
* $NZaZorgactiviteit#034752 "Aanleggen van een anus preaternaturalis na laparotomie bijvoorbeeld bij ileus of peritonitis, open procedure (zie 034753 voor endoscopisch)."
* $NZaZorgactiviteit#034753 "Endoscopisch aanleggen van een anus preaternaturalis na laparotomie bijvoorbeeld bij ileus of peritonitis (zie 034752 voor open procedure)."
* $NZaZorgactiviteit#034638 "Dunne darmresectie, open procedure (zie 034639 voor endoscopisch)."
* $NZaZorgactiviteit#034639 "Endoscopische dunne darmresectie (zie 034638 voor open procedure)."
* $NZaZorgactiviteit#034738 "Colonresectie, al dan niet met coecostomie, open procedure (zie 034739 voor endoscopisch)"
* $NZaZorgactiviteit#034739 "Endoscopische colonresectie, al dan niet met coecostomie (zie 034738 voor open procedure)."
* $NZaZorgactiviteit#034792 "Enterostomie als onderdeel van een laparotomie of om andere redenen, open procedure (zie 034793 voor endoscopisch)."
* $NZaZorgactiviteit#034793 "Endoscopische enterostomie als onderdeel van een laparoscopie of om andere redenen (zie 034792 voor open procedure)."
* $NZaZorgactiviteit#034796 "Ileostomie, open procedure (zie 034797 voor endoscopisch)."
* $NZaZorgactiviteit#034797 "Endoscopische ileostomie (zie 034796 voor open procedure)."
* $NZaZorgactiviteit#034798 "Enterostomie, open procedure (zie 034799 voor endoscopisch)."
* $NZaZorgactiviteit#034799 "Endoscopische enterostomie (zie 034798 voor open procedure)."
* $NZaZorgactiviteit#034808 "Entero-anastomose operatie, open procedure (zie 034809 voor endoscopisch)."
* $NZaZorgactiviteit#034809 "Endoscopische entero-anastomose operatie (zie 034808 voor open procedure)."
* $NZaZorgactiviteit#034810 "Ileorectale anastomose, open procedure (zie 034811 voor endoscopisch)."
* $NZaZorgactiviteit#034811 "Endoscopische ileorectale anastomose (zie 034810 voor open procedure)."
* $NZaZorgactiviteit#034880 "ILEUSOK ZONDER RESECTIE OF ANASTOMOSE, PER 1-7-2004 ALLEEN OPEN PROCEDURE"

// Consulten / zorgactiviteiten
* $NZaZorgactiviteit#190204 "VERPLEEGDAG"
* $NZaZorgactiviteit#190205 "VERPLEEGDAG KLASSE 3B"
* $NZaZorgactiviteit#190218 "Verpleegdag."
* $NZaZorgactiviteit#190031 "Verkeerde bed"
* $NZaZorgactiviteit#190034 "Afwezigheidsdag"
* $NZaZorgactiviteit#190035 "DAGVERPLEGING I: NORMAAL"
* $NZaZorgactiviteit#190038 "Verblijf vervallen ziekenhuisindicatie, niet verpleeghuis"
* $NZaZorgactiviteit#190055 "DAGVERPLEGING II: ZWAAR"
* $NZaZorgactiviteit#190090 "Dagverpleging."
* $NZaZorgactiviteit#190091 "Langdurige observatie zonder overnachting."
* $NZaZorgactiviteit#190092 "Verkeerde bed, Wlz-indicatie."
* $NZaZorgactiviteit#190093 "Verblijf vervallen ziekenhuisindicatie, geen Wlz-indicatie."
* $NZaZorgactiviteit#190012 "DBC-REGISTRATIECODE VOOR EEN NIET-DECL. POLIKLINIEKBEZOEK DAT LEIDT TOT OPENING VAN EEN NIEUWE DBC"
* $NZaZorgactiviteit#190013 "DBC-REGISTRATIECODE VOOR HERHAAL-POLIKLINIEKBEZOEKEN BIJ EEN LOPENDE DBC"
* $NZaZorgactiviteit#190015 "Spoedeisende hulp contact op de SEH afdeling"
* $NZaZorgactiviteit#190016 "Spoedeisende hulp contact buiten de SEH afdeling, elders in het ziekenhuis"
* $NZaZorgactiviteit#190060 "Eerste polikliniekbezoek."
* $NZaZorgactiviteit#190063 "Intensief consult ten behoeve van zorgvuldige afweging behandelopties."
* $NZaZorgactiviteit#190066 "Uitgebreid consult ten behoeve van zorgvuldige afweging behandelopties, samen met patiënt en/of met zijn/haar vertegenwoordiger."
* $NZaZorgactiviteit#190019 "Screen to screen beeldcontact tussen poortspecialist en patiënt bij een al geopende DBC ter vervanging van een fysiek herhaalconsult (voor andere vormen van teleconsult zie 190025)."
* $NZaZorgactiviteit#190025 "Teleconsult tussen poortspecialist en patiënt."
* $NZaZorgactiviteit#190153 "IC-dag licht."
* $NZaZorgactiviteit#190154 "IC-dag middel."
* $NZaZorgactiviteit#190155 "IC-dag zwaar."
* $NZaZorgactiviteit#190157 "IC-dag, type 1."
* $NZaZorgactiviteit#190158 "IC-dag, type 2."
* $NZaZorgactiviteit#190161 "Screen to screen consult ter vervanging van een polikliniekbezoek."
* $NZaZorgactiviteit#190162 "Belconsult ter vervanging van een herhaal-polikliniekbezoek."
* $NZaZorgactiviteit#190163 "Schriftelijke consultatie ter vervanging van een herhaal-polikliniekbezoek."
* $NZaZorgactiviteit#190164 "Belconsult ter vervanging van een eerste polikliniekbezoek."
* $NZaZorgactiviteit#190165 "Screen-to-screen consult ter vervanging van een eerste polikliniekbezoek."
* $NZaZorgactiviteit#190166 "Screen-to-screen consult ter vervanging van een herhaal-polikliniekbezoek."
* $NZaZorgactiviteit#190167 "Schriftelijke consultatie ter vervanging van een eerste polikliniekbezoek."

// Scopieën
* $NZaZorgactiviteit#034620 "Diagnostische endoscopie van oesofagus, maag en/of duodenum met behulp van een flexibele endoscoop, inclusief eventuele biopten."
* $NZaZorgactiviteit#034686 "DIAGN. COLOSCOPIE MET FIBERSC."
* $NZaZorgactiviteit#034690 "Diagnostische sigmoidoscopie met behulp van fiberscoop, inclusief eventuele proefexcisie(s) en poliepectomie."
* $NZaZorgactiviteit#035582 "Diagnostische laparoscopie, inclusief eventuele proefexcisie(s) (excl. bij vruchtbaarheidsproblematiek, zie 035583)."

// Careplan
* $NZaZorgactiviteit#039133 "Telemonitoring (exclusief i.h.k.v. CardioMEMS studie, zie 032716)."

// MRI
* $NZaZorgactiviteit#087090 "MRI ABDOMEN"
* $NZaZorgactiviteit#087091 "MRI lever."
* $NZaZorgactiviteit#087096 "MRI rectum."
* $NZaZorgactiviteit#087097 "MRI abdomen (excl. rectum, zie 087096)."

// Echo
* $NZaZorgactiviteit#039492 "Echografie van de buikorganen (excl. echo van de darmen, zie 034679)."

// Biopten
* $NZaZorgactiviteit#035280 "LEVERBIOPSIE"

// CT-scans
* $NZaZorgactiviteit#080043 "CT total body scan, exclusief preventief onderzoek."
* $NZaZorgactiviteit#089142 "COMPUTERTOMOGRAFIE ONDERSTE EXTREMITEITEN"
* $NZaZorgactiviteit#089042 "CT BEKKEN"
* $NZaZorgactiviteit#087043 "CT virtuele colonoscopie."
* $NZaZorgactiviteit#087042 "CT ABDOMEN"
