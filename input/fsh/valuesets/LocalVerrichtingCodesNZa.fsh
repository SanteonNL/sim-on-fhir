Alias: $NZaProcedure = https://declaratie.nza.nl/zorgactiviteit

ValueSet: LocalVerrichtingCodesNZa
Id: local-verrichting-codes-nza
Title: "Verrichting Type Codes NZa (Scopie)"
Description: "De specifieke NZa-verrichtingencodes voor de scopie-indicator."
* ^status = #active
* ^compose.include[0].system = $NZaProcedure
* ^compose.include[0].concept[0].code = #034620
* ^compose.include[0].concept[0].display = "Verrichting 034620"
* ^compose.include[0].concept[1].code = #034686
* ^compose.include[0].concept[1].display = "Verrichting 034686"
* ^compose.include[0].concept[2].code = #034690
* ^compose.include[0].concept[2].display = "Verrichting 034690"
* ^compose.include[0].concept[3].code = #035582
* ^compose.include[0].concept[3].display = "Verrichting 035582"