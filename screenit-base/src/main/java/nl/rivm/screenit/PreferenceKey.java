package nl.rivm.screenit;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.time.LocalTime;
import java.util.Date;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum PreferenceKey
{
	START_MIDDAG("Start middag", LocalTime.class),

	START_AVOND("Start avond", LocalTime.class),

	VOORAANKONDIGINSPERIODE("Vooraankondingsperiode", Integer.class),

	COLON_VOORAANKONDIGING_NA_VERVOLGONDERZOEK("Vooraankondiging na vervolgonderzoek", Integer.class),

	UITNODIGINGSINTERVAL("Spreidingsperiode (interval)", Integer.class),

	IFOBTRETOURPERIODE("FIT retour-periode", Integer.class),

	IFOBTRAPELPERIODE("FIT herinnering-periode", Integer.class),

	IFOBTANALYSEPERIODE("FIT analyse-periode", Integer.class),

	INTAKEAFSPRAAKPERIODE("Intake afspraak-periode", Integer.class),

	PERCENTGAGEIFOBTONGUSTIG("FIT ongunstig percentage", Double.class),

	PERCENTAGEIFOBTRETOUR("FIT retour percentage", Double.class),

	WAARSCHUWINGAANTALIFOBTS("Waarschuwingslimiet aantal FIT's", Integer.class),

	MAXIMUMAANTALIFOBTS("Maximale aantal FIT's", Integer.class),

	WACHTWOORDEMAIL("Wachtwoordaanvragen e-mail", String.class),

	WACHTWOORDEMAILSUBJECT("Wachtwoordaanvragen e-mailonderwerp", String.class),

	WACHTWOORDVERLOOPTEMAIL("Wachtwoord verloopt e-mail", String.class),

	WACHTWOORDVERLOOPTEMAILSUBJECT("Wachtwoord verloopt e-mailonderwerp", String.class),

	UZIEMAIL("Uzi e-mail", String.class),

	UZIEMAILSUBJECT("Uzi e-mailonderwerp", String.class),

	GEBLOKKEERDEMAIL("Geblokkeerd account e-mail", String.class),

	GEBLOKKEERDEMAILSUBJECT("Geblokkeerd account e-mailonderwerp", String.class),

	INACTIVERENEMAIL("Medewerker inactiveerd e-mail", String.class),

	INACTIVERENSUBJECT("Medewerker inactiveerd e-mailonderwerp", String.class),

	HUISARTS_REG_EMAIL("Registreren huisarts e-mail", String.class),

	HUISARTS_REG_EMAILSUBJECT("Registreren huisarts e-mailonderwerp", String.class),

	HUISARTS_WACHTWOORD_EMAIL("Wachtwoord vergeten huisarts e-mail", String.class),

	HUISARTS_WACHTWOORD_EMAILSUBJECT("Wachtwoord vergeten huisarts e-mailonderwerp", String.class),

	DAGEN_WACHTWOORD_GELDIG("Aantal dagen wachtwoord geldig", Integer.class),

	MAXIMUM_FOUTIEVE_AANMELDPOGINGEN("Maximum foutieve aanmeldpogingen", Integer.class),

	FOUTIEVE_AANMELDPOGINGEN_TIMEOUT("Duur account blokkering", Integer.class),

	WACHTWOORDAANVRAGEN("Wachtwoorden aanvragen", Boolean.class),

	WACHTWOORD_VERLOOPT_HERINNERINGS_TERMIJN("Herinneringstermijn voor wachtwoord verloopt", Integer.class),

	IFOBT_NORM_WAARDE("FIT norm waarde", Double.class),

	IFOBT_DETECTIEGRENS("FIT detectiegrens", Double.class),

	ONGUNSTIG_TEKST_HUISARTS("Ongunstige tekst huisarts", String.class),

	MINIMALE_LEEFTIJD_COLON("Minimale leeftijd colonscreening", Integer.class),

	MAXIMALE_LEEFTIJD_COLON("Maximale leeftijd colonscreening", Integer.class),

	ONGUNSTIGE_UITSLAG_WACHT_PERIODE("Ongunstige uitslag wacht periode", Integer.class),

	COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE("Maximale extra dagen planning intake", Integer.class),

	COLON_LAATSTE_RONDE_BRIEF_TEKST("Tekst bij (potentieel) laatste uitnodiging", String.class),

	COLON_VOLGENDE_RONDE_BRIEF_TEKST("Tekst bij (potentieel) volgende uitnodiging", String.class),

	MAX_AFSTAND_CLIENT_COLOSCOPIECENTRUM("Maximale afstand client - intakelocatie", Integer.class),

	COLON_MAX_EXTRA_POGINGEN_PLANNING_INTAKE("Max extra pogingen planning intake", Integer.class),

	COLON_AANTAL_RONDES_UITNODIGINGSBRIEF_ZONDER_FIT("Aantal rondes uitnodigingsbrief zonder FIT", Integer.class),

	AFSTANDFACTOR("Afstandfactor", Integer.class),

	TIJDFACTOR("Tijdfactor", Integer.class),

	INTAKE_NIET_WIJZIGBAAR("Periode Afspraak intake niet wijzigbaar", Integer.class),

	DASHBOARDEMAIL("E-mail dashboard", String.class),

	OVEREEENKOMSTMAIL("Overeenkomst gewijzigd e-mail", String.class),

	OVEREENKOMSTSUBJECT("Overeenkomst gewijzigd e-mailonderwerp", String.class),

	OVEREEENKOMSTMAIL_ZVUA("Zakelijke voorwaarden BMHK huisarts gewijzigd e-mail", String.class),

	OVEREENKOMSTSUBJECT_ZVUA("Zakelijke voorwaarden BMHK huisarts gewijzigd e-mailonderwerp", String.class),

	HUISARTS_NO_SHOW_PERIODE("Huisarts bericht op no show", Integer.class),

	PERIODE_MINIMALE_HOUDBAARHEID_IFOBT_MONSTERS_VOOR_CONTROLE("Periode minimale houdbaarheid FIT monsters voor controle", Integer.class),

	PERIODE_MINIMALE_HOUDBAARHEID_ZAS_MONSTERS_VOOR_CONTROLE("Periode minimale houdbaarheid ZAS bij verzending inpakcentrum", Integer.class),

	EDIFACTADRES("Edifact adres", String.class),

	WACHTTIJD_VERZENDEN_PAKKET_TWEE_OP_EEN_ADRES("Wachttijd verzenden pakket bij 2 op 1 adres", Integer.class),

	MAIL_VERZENDEN("Mail verzenden", Enum.class),

	SMS_VERZENDEN("Sms verzenden", Enum.class),

	ALTERNATIEF_ADRES("Alternatief adres", String.class),

	ALTERNATIEF_MOBIELNUMMER("Alternatief mobielnummer", String.class),

	KANSBEREKENING_BK("Kansberekening BK", Enum.class),

	KANSBEREKENING_BK_TEST_DEFAULT_OPKOMSTKANS("Opkomstkans voor testen indien BK kansberekening uit staat", Integer.class),

	STARTDATUM_BMHK("Startdatum BMHK", Date.class),

	UITSTEL_BIJ_ZWANGERSCHAP_CERVIX("Uitstel bij zwangerschap", Integer.class),

	AFKAPWAARDE_LABFORMULIER("Afkapwaarde labformulier", Integer.class),

	PERIODE_UITSLAG_NAAR_HUISARTS("Periode uitslag nog naar huisarts", Integer.class),

	INTERNAL_ZORGMAIL_BESTAND_URL("URL zorgmail update bestand", String.class),

	CERVIX_HERINNERINGS_PERIODE_NON_RESPONDER("Herinneringsperiode non-responder", Integer.class),

	CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE("Interval Controle Uitstrijkje", Integer.class),

	CERVIX_HERINNERINGS_PERIODE_ZAS("Herinneringsperiode ZAS", Integer.class),

	CERVIX_HERINNERINGS_PERIODE_LAATSTE_HERINNERING("Periode laatste herinnering", Integer.class),

	CERVIX_MAX_ZAS_AANVRAGEN_INFOLIJN("Maximum aantal ZAS-en handmatig aanvragen via InfoLijn", Integer.class),

	CERVIX_MAX_ZAS_AANVRAGEN_CLIENT("Maximum aantal ZAS-en handmatig aanvragen via Clientportaal", Integer.class),

	CERVIX_HERINNERING_TEKST("Herinnering tekst", String.class),

	CERVIX_UITGESTELD_TEKST("Uitgesteld tekst", String.class),

	CERVIX_EENMALIG_HERAANMELDEN_TEKST("Eenmalig heraanmelden tekst", String.class),

	CERVIX_DEFINITIEF_HERAANMELDEN_TEKST("Definitief heraanmelden tekst", String.class),

	CERVIX_VERVOLGONDERZOEK_NEGATIEF_65PLUS_TEKST("Tekst wanneer vervolgonderzoek negatief is en doelgroep 65 plus", String.class),
	CERVIX_VERVOLGONDERZOEK_NEGATIEF_60PLUS_TEKST("Tekst wanneer vervolgonderzoek negatief is en doelgroep 60 plus", String.class),
	CERVIX_VERVOLGONDERZOEK_NEGATIEF_OVERIGE_TEKST("Tekst wanneer vervolgonderzoek negatief is en bedoelt voor alle overige doelgroepen", String.class),
	CERVIX_CYTOLOGIE_POSITIEF_60PLUS_TEKST("Tekst wanneer de cytologie uitslag positief is en doelgroep 60 plus", String.class),
	CERVIX_CYTOLOGIE_POSITIEF_OVERIGE_TEKST("Tekst wanneer de cytologie uitslag positief is en bedoelt voor alle overige doelgroepen", String.class),

	CERVIX_NIEUWE_ZAS_NA_OUDE_INGESTUURDE_ZAS_TEKST("Tekst wanneer een nieuwe ZAS aangevraagd is nav ingestuurde oude ZAS type (Z)", String.class),

	CERVIX_NIEUWE_ZAS_STANDAARD_TEKST("Tekst wanneer een nieuwe ZAS aangevraagd is", String.class),

	COLON_EENMALIG_HERAANMELDEN_TEKST("Eenmalig heraanmelden tekst", String.class),

	COLON_TIJDELIJK_HERAANMELDEN_TEKST("Tijdelijk heraanmelden tekst", String.class),

	COLON_DEFINITIEF_HERAANMELDEN_TEKST("Definitief heraanmelden tekst", String.class),

	COLON_ZONDER_CONCLUSIE_PERIODE("Zonder intakeconclusie periode", Integer.class),

	COLON_ZONDER_DEF_VERVOLGBELEID_PERIODE("Zonder definitief vervolgbeleid periode", Integer.class),

	COLON_NIEUWE_FIT_AANGEVRAAGD_TEKST("Nieuwe FIT aangevraagd tekst", String.class),

	COLON_NIEUWE_FIT_NA_HERAANMELDING_TEKST("Nieuwe FIT na heraanmelding tekst", String.class),

	COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("DK clientportaal: Toon vervangende tekst", Boolean.class),

	COLON_CLIENTPORTAAL_VERVANGENDE_TEKST("DK clientportaal: Vervangende tekst", String.class),

	COLON_CLIENTPORTAAL_TIJDELIJKE_MELDING("DK clientportaal: Tijdelijke melding", String.class),

	COLON_DEFINITIEVE_AFMELDING_BEVESTIGING_TEKST("Definitief afgemeld bevestiging tekst", String.class),

	COLON_TIJDELIJKE_AFMELDING_BEVESTIGING_TEKST("Tijdelijk afgemeld bevestiging tekst", String.class),

	COLON_SIGNALERINGSTERMIJN_GEEN_CAPACITEIT("Signaleringstermijn voor geen opgegeven capaciteit", Integer.class),

	CERVIX_WACHTTIJD_UITSTRIJKJE_ONTBREEKT_ANALOOG("Uitstrijkje ontbreekt (analoog labformulier)", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_UITSTRIJKJE_ONTVANGEN_ANALOOG("Wacht op uitstrijkje ontvangen (analoog labformulier)", Integer.class),

	CERVIX_WACHTTIJD_UITSTRIJKJE_ONTBREEKT_DIGITAAL("Uitstrijkje ontbreekt (digitaal labformulier)", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_UITSTRIJKJE_ONTVANGEN_DIGITAAL("Wacht op uitstrijkje ontvangen (digitaal labformulier)", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_HPV_UITSLAG("Wacht op HPV uitslag", Integer.class),

	CERVIX_WACHTTIJD_LABFORMULIER_ONTBREEKT("Labformulier ontbreekt", Integer.class),

	CERVIX_WACHTTIJD_HUISARTS_DOORGEVEN("Huisarts doorgeven", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_GECONTROLEERD("Wacht op gecontroleerd", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE("Wacht op gecontroleerd voor cytologie", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_CYTOLOGIE_UITSLAG("Wacht op cytologie uitslag", Integer.class),

	CERVIX_WACHTTIJD_WACHT_OP_WAARSCHUWING_CYTOLOGIE_UITSLAG("Signalering lab wacht op cytologie uitslag", Integer.class),

	CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL("Bezwaar mail tekst voor gebruik van lichaamsmateriaal voor wetenschappelijk onderzoek", String.class),

	CERVIX_BEZWAAR_WETENSCHAPPELIJK_GEBRUIK_LICHAAMSMATERIAAL_SUBJECT("Bezwaar mail subject voor gebruik van lichaamsmateriaal voor wetenschappelijk onderzoek", String.class),

	CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES("Bezwaar mail voor controle verwijsadvies", String.class),

	CERVIX_BEZWAAR_CONTROLE_VERVOLG_VERWIJSADVIES_SUBJECT("Bezwaar mail subject voor controle verwijsadvies", String.class),

	CERVIX_OMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL("Mail tekst voor huisarts onbekend met omissie verstreken", String.class),

	CERVIX_OMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL_SUBJECT("Mail subject voor huisarts onbekend met omissie verstreken", String.class),

	CERVIX_OMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL("Mail tekst voor omissie afgegaan te laat beoordeling, met alsnog beoordeling", String.class),

	CERVIX_OMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL_SUBJECT("Mail subject voor omissie afgegaan te laat beoordeling, met alsnog beoordeling", String.class),

	CERVIX_OMISSIE_ONTBREKEND_CYTOLOGIEVERSLAG_SUBJECT("Mail subject voor omissie ontbrekend cytologieverslag", String.class),

	CERVIX_OMISSIE_ONTBREKEND_CYTOLOGIEVERSLAG_MAIL("Mail tekst voor omissie ontbrekend cytologieverslag", String.class),

	CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL("Mail tekst voor huisarts gekoppeld aan monster", String.class),

	CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL_SUBJECT("Mail subject voor huisarts gekoppeld aan monster", String.class),

	CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("BMHK clientportaal: Toon vervangende tekst", Boolean.class),

	CERVIX_CLIENTPORTAAL_VERVANGENDE_TEKST("BMHK clientportaal: Vervangende tekst", String.class),

	CERVIX_CLIENTPORTAAL_TIJDELIJKE_MELDING("BMHK clientportaal: Tijdelijke melding", String.class),

	BMHK_LABEL_PRINTEN_ZONDER_PDF("BMHK label printen zonder PDF", Boolean.class),

	CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE("Startdatum aanlevering genotypering analyseresultaten en invoering triage BMHK", Date.class),

	CERVIX_START_BMHK2023("Startdatum BMHK2023", Date.class),

	MAMMA_MINIMALE_LEEFTIJD("Minimale leeftijd", Integer.class),

	MAMMA_MAXIMALE_LEEFTIJD("Maximale leeftijd", Integer.class),

	MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN("Minimale interval mammografie onderzoeken", Integer.class),

	MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN("Minimale interval uitnodigingen", Integer.class),

	MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN("Afspraak bij uitnodigen vanaf", Integer.class),

	MAMMA_AFSPRAAK_ZOEKEN_STANDAARD_FILTER_EINDDATUM_DAGEN_IN_TOEKOMST("Afspraak zoeken standaard einddatum", Integer.class),

	MAMMA_AFSPRAAK_ZOEKEN_AANTAL_MINUTEN_IN_TOEKOMST("Afspraak zoeken minimaal aantal minuten in de toekomst", Integer.class),

	MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR("Afspraak reservering geldig voor", Integer.class),

	MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN("Afspraak verzetten zonder client contact vanaf", Integer.class),

	MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN("Capaciteit volledig benut tot en met", Integer.class),

	MAMMA_BEVESTIGINGSBRIEF_NIET_VERZENDEN_BINNEN_AANTAL_WERKDAGEN("Bevestigingsbrief niet versturen als de afspraak valt binnen aantal dagen", Integer.class),

	MAMMA_BULK_VERZETTEN_IN_VERLEDEN_AANTAL_WEKEN("Bulk verzetten in verleden", Integer.class),

	MAMMA_BULK_VERZETTEN_ALLEEN_BRIEF("Bulk verzetten alleen brief versturen", Boolean.class),

	MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST_NA_DAGEN("Op follow-up niet gedownload werklijst na dagen", Integer.class),

	MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN("Op follow-up radiologie werklijst na downloaden beelden", Integer.class),

	MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST_NA_RADIOLOGIEVERSLAG("Op follow-up pathologie werklijst na radiologieverslag", Integer.class),

	MAMMA_AFSPRAAK_LOCATIE_WIJZIGING_TEKST("Tekst bij locatiewijziging afspraak", String.class),

	MAMMA_AFSPRAAK_BETREFT_BEVESTIGING_TEKST("Tekst bij bevestiging afspraak bevolkingsonderzoek borstkanker", String.class),

	MAMMA_AFSPRAAK_BETREFT_WIJZIGING_TEKST("Tekst bij gewijzigde afspraak bevolkingsonderzoek borstkanker", String.class),

	MAMMA_UITNODIGING_NA_UITSTEL_TEKST("Tekst bij uitnodiging ten gevolge van uitstel", String.class),

	MAMMA_HERINNERINGS_PERIODE_GEEN_AFSPRAAK("Herinneringsperiode geen afspraak", Integer.class),

	MAMMA_HERINNERINGS_PERIODE_NO_SHOW("Herinneringsperiode no show", Integer.class),

	MAMMA_IMS_HOST_NAME("IMS host name voor BK", String.class),

	MAMMA_IMS_ORM_PORT("IMS ORM port voor BK", Integer.class),

	MAMMA_IMS_ORM_ILM_PORT("IMS ORM ILM port voor BK", Integer.class),

	MAMMA_IMS_ADT_PORT("IMS ADT port voor BK", Integer.class),

	MAMMA_HL7_ENABLED("Versturen van Hl7 berichten aan of uit.", Boolean.class),

	MAMMA_IMS_QUEUE_SIZE_WARNING_THRESHOLD("Queue-grootte waarschuwingsdrempel", Integer.class),

	MAMMA_ONDERBROKEN_ONDERZOEK_ZONDER_BEELDEN_TEKST("Tekst bij onderbroken onderzoeken zondere beelden", String.class),

	MAMMA_ONDERBROKEN_ONDERZOEK_MET_BEELDEN_TEKST("Tekst bij onderbroken onderzoeken met beelden", String.class),

	MAMMA_BULK_VERZETTEN_VERLEDEN_AFSPRAAK_TEKST("Tekst bij bulk verzetten van afspraak in het verleden", String.class),

	MAMMA_BULK_VERZETTEN_TOEKOMST_AFSPRAAK_TEKST("Tekst bij bulk verzetten van afspraak in de toekomst", String.class),

	MAMMA_MEEKIJKVERZOEK_MAIL_ADRES("E-mail adres(sen) voor meekijkverzoek", String.class),

	MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("BK clientportaal: Toon vervangende tekst", Boolean.class),

	MAMMA_CLIENTPORTAAL_VERVANGENDE_TEKST("BK clientportaal: Vervangende tekst", String.class),

	MAMMA_CLIENTPORTAAL_TIJDELIJKE_MELDING("BK clientportaal: Tijdelijke melding", String.class),

	CERVIX_UITSTEL_UITSLAGBRIEF_PAP3A2_OF_HOGER("Uitstel uitslagbrief na PAP3a2 of hoger", Integer.class),

	CERVIX_VOORAANKONDIGINGS_PERIODE("Het aantal dagen voor de 30e verjaardag dat de vooraankondiging gegenereerd wordt", Integer.class),

	INTERNAL_COLON_COMPLICATIE_VERWERKING_STOP("Stopdatum verwerking complicaties DK", Date.class),

	INTERNAL_WSB_SCHEMATRON_VERSIONPATHMAPPING("Schematron version path mapping", String.class),

	INTERNAL_CDA_VERSIONMAPPING("Schematron cda version path mapping", String.class),

	INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES("Herinneringsperiode onvolledig adres melding", Integer.class),

	INTERNAL_MAMMA_IMS_DICOM_CMOVE_CONFIG("PACS IMS CMove connection config", String.class),

	INTERNAL_MAMMA_IMS_DICOM_CSTORE_CONFIG("PACS IMS CStore connection config", String.class),

	INTERNAL_MAX_GROOTTE_ZIP("Max grootte zip bestand in bytes", Integer.class),

	INTERNAL_MAMMA_UPLOADLIMIET_UPLOADPORTAAL("BK Uploadlimiet uitwisselportaal in megabytes", Integer.class),

	INTERNAL_MAMMA_POC_CLIENTEN_SET("Set met accession numbers van de POC clienten set op keten", String.class),

	MAMMA_XDS_HOME_COMMUNITY_ID("XDS home community id", String.class),

	MAMMA_XDS_ASSIGNING_AUTHORITY("XDS assigning authority", String.class),

	MAMMA_XDS_REPOSITORY_ID("XDS repository id", String.class),

	MAMMA_XDS_REPOSITORY_URL("XDS repository url", String.class),

	MAMMA_XDS_REGISTRY_URL("XDS registry url", String.class),

	MAMMA_XDS_SOURCE_ID("XDS source id", String.class),

	INTERNAL_ZORGID_CALLBACKURL("ZorgID callbackurl", String.class),

	INTERNAL_ZORGID_SERVERURL("ZorgID serverurl", String.class),

	INTERNAL_ZORGID_CLIENTREADTIMEOUT("ZorgID client read timeout", Integer.class),

	INTERNAL_ZORGID_TRANSACTIONCLIENTREADTIMEOUT("ZorgID transaction client read timeout", Integer.class),

	INTERNAL_ZORGID_CLIENTCONNECTTIMEOUT("ZorgID client connection timeout", Integer.class),

	INTERNAL_ZORGID_CLIENTMAXCONNECTIONS("ZorgID client max connections", Integer.class),

	INTERNAL_ZORGID_WEBSERVICEKEYSTORE("ZorgID webservice keystore", String.class),

	INTERNAL_ZORGID_WEBSERVICEKEYSTOREPASSWORD("ZorgID webservice keystore password", String.class),

	INTERNAL_CERVIX_LAB_FORMULIER_VALID_FQDNS("Valided FQDNs (BMHK)", String.class),

	INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON("SE informatie ophalen CRON", String.class),

	INTERNAL_OPENID_CONNECT_IDP_KEYSTOREPASSWORD("OpenID Connect IPD keystore password", String.class),

	INTERNAL_OPENID_CONNECT_IDP_KEYSTORE("OpenID Connect IPD keystore location", String.class),

	INTERNAL_OPENID_CONNECT_IDP_ISSUER("OpenID Connect IPD issuer", String.class),

	INTERNAL_OPENID_CONNECT_IDP_KEYCLOAK_ORIGIN("OpenID Connect IPD keykcloak origin", String.class),

	INTERNAL_OPENID_CONNECT_IDP_CLIENT_ID("OpenID Connect IPD client ID", String.class),

	INTERNAL_OPENID_CONNECT_IDP_FLOW_ALIAS("OpenID Connect IPD flow alias", String.class),

	INTERNAL_OPENID_CONNECT_IDP_EXPIRATION("OpenID Connect IPD expiration time", Integer.class),

	POSTCODE_NL_API_KEY("Postcode.nl API key", String.class),

	POSTCODE_NL_API_SECRET("Postcode.nl API secret", String.class),

	POSTCODE_NL_API_HOST("Postcode.nl API host", String.class),

	POSTCODE_NL_API_SCHEME("Postcode.nl API scheme", String.class),

	POSTCODE_NL_API_DELIVERYPATH("Postcode.nl API delivery path", String.class),

	POSTCODE_NL_API_TARGET_WPL("Target laatste update WPL", String.class),

	POSTCODE_NL_API_TARGET_NUM("Target laatste update NUM", String.class),

	MAMMA_DICOM_SOP_CONFIG("DICOM SOP configuratie", String.class),

	MAMMA_ONDERZOEKSCAPACITEIT_NIET_BESCHIKBAAR_BINNEN_WERKDAGEN("Onderzoekscapaciteit niet beschikbaar binnen werkdagen", Integer.class),

	ILM_BEWAARTERMIJN("Bewaartermijn", Integer.class),

	ILM_BEWAARTERMIJN_NIET_MEDISCH("Bewaartermijn niet medisch", Integer.class),

	ILM_SIGNALEERTERMIJN_BEELDEN_STATUS("Signaleertermijn beelden status", Integer.class),

	ILM_BEZWAARTERMIJN_BEELDEN_VERWIJDERD("Bezwaartermijn beelden verwijderd", Integer.class),

	ILM_BEWAARTERMIJN_PALGA("Bewaartermijn palga verslagen", Integer.class),

	MAMMA_SE_MAX_OFFLINE_INLOGPERIODE("SE offline inloggen", Integer.class),

	MAMMA_SE_DAGLIJST_OPHALEN_DAGEN("SE daglijst ophalen dagen", Integer.class),

	MAMMA_ANNOTEER_EERSTE_RONDE("Eerste ronde uitnodiging apart afdrukken", Boolean.class),

	CLIENT_NIEUW_GENDERDIVERS_TEKST("Nieuwe client genderdivers tekst", String.class),

	CLIENT_GENDERIDENTITEITSWIJZIGING_TEKST("Genderidentiteitswijziging tekst", String.class),

	INTERNAL_MAMMA_SE_PING_INTERVAL("Se REST ping interval", Integer.class),

	INTERNAL_MAMMA_SE_PONG_TIMEOUT("Se REST pong timeout", Integer.class),

	MAMMA_AFSPRAAK_SMS_HERINNERING_TERMIJN("Tijd in uren waarvoor sms herinnering voor afspraak gestuurd wordt", Integer.class),

	RETRIES_VERZENDEN_INPAKCENTRUM("Retry voor verzenden naar inpakcentrum", Integer.class),

	TIME_BETWEEN_RETRIES_VERZENDEN_INPAKCENTRUM("Tijd tussen de retries voor verzenden naar inpakcentrum in ms", Integer.class),

	COLON_ROOSTER_NACHT_BEPERKING_TYPE("Harde of zachte nacht beperking voor het DK rooster", String.class),
	COLON_ROOSTER_NACHT_BEPERKING_BEGIN("Start tijd voor de nacht beperking voor het DK rooster", String.class),
	COLON_ROOSTER_NACHT_BEPERKING_EIND("Eind tijd voor de nacht beperking voor het DK rooster", String.class),
	COLON_ROOSTER_ZATERDAG_BEPERKING_TYPE("Harde of zachte beperking op zaterdag voor het DK rooster", String.class),
	COLON_ROOSTER_ZONDAG_BEPERKING_TYPE("Harde of zachte beperking op zondag voor het DK rooster", String.class);

	private final String layoutName;

	private final Class<?> type;

}
