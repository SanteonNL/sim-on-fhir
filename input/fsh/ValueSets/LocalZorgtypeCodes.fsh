Alias: $Zorgtype = https://declaratie.nza.nl/zorgtype

ValueSet: LocalZorgtypeCodes
Id: local-zorgtype-codes
Title: "IBD Zorgtype Codes (11 en 21)"
Description: "Bevat de toegestane landelijke zorgtypes 11 en 21 uit de tabel EPISODE_ZORGTYPE."
* ^status = #active
* ^compose.include[0].system = $Zorgtype
* ^compose.include[0].concept[0].code = #11
* ^compose.include[0].concept[0].display = "Regulier DBC-zorgtraject"
* ^compose.include[0].concept[1].code = #21
* ^compose.include[0].concept[1].display = "Regulier vervolg-DBC-zorgtraject"