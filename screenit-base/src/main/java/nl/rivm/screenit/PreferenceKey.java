package nl.rivm.screenit;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

public enum PreferenceKey
{

	VOORAANKONDIGINSPERIODE("Vooraankondingsperiode", Integer.class),

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

	IFOBT_NORM_WAARDE("FIT norm waarde", Double.class),

	IFOBT_DETECTIEGRENS("FIT detectiegrens", Double.class),

	ONGUNSTIG_TEKST_HUISARTS("Ongunstige tekst huisarts", String.class),

	MINIMALE_LEEFTIJD_COLON("Minimale leeftijd colonscreening", Integer.class),

	MAXIMALE_LEEFTIJD_COLON("Maximale leeftijd colonscreening", Integer.class),

	ONGUNSTIGE_UITSLAG_WACHT_PERIODE("Ongunstige uitslag wacht periode", Integer.class),

	COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE("Maximale extra dagen planning intake", Integer.class),

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

	ALTERNATIEF_ADRES("Alternatief adres", String.class),

	KANSBEREKENING_BK("Kansberekening BK", Enum.class),

	STARTDATUM_BMHK("Startdatum BMHK", Date.class),

	UITSTEL_BIJ_ZWANGERSCHAP_CERVIX("Uitstel bij zwangerschap", Integer.class),

	AFKAPWAARDE_LABFORMULIER("Afkapwaarde labformulier", Integer.class),

	PERIODE_UITSLAG_NAAR_HUISARTS("Periode uitslag nog naar huisarts", Integer.class),

	INTERNAL_ZORGMAIL_BESTAND_URL("URL zorgmail update bestand", String.class),

	CERVIX_HERINNERINGS_PERIODE("Herinneringsperiode", Integer.class),

	CERVIX_HERINNERINGS_PERIODE_ZAS("Herinneringsperiode ZAS", Integer.class),

	CERVIX_MAX_ZAS_AANVRAGEN_INFOLIJN("Maximum aantal ZAS-en via InfoLijn", Integer.class),

	CERVIX_MAX_ZAS_AANVRAGEN_CLIENT("Maximum aantal ZAS-en via Clientportaal", Integer.class),

	CERVIX_HERINNERING_TEKST("Herinnering tekst", String.class),

	CERVIX_UITGESTELD_TEKST("Uitgesteld tekst", String.class),

	CERVIX_EENMALIG_HERAANMELDEN_TEKST("Eenmalig heraanmelden tekst", String.class),

	CERVIX_DEFINITIEF_HERAANMELDEN_TEKST("Definitief heraanmelden tekst", String.class),

	COLON_EENMALIG_HERAANMELDEN_TEKST("Eenmalig heraanmelden tekst", String.class),

	COLON_DEFINITIEF_HERAANMELDEN_TEKST("Definitief heraanmelden tekst", String.class),

	COLON_ZONDER_CONCLUSIE_PERIODE("Zonder intakeconclusie periode", Integer.class),

	COLON_ZONDER_DEF_VERVOLGBELEID_PERIODE("Zonder definitief vervolgbeleid periode", Integer.class),

	COLON_NIEUWE_FIT_AANGEVRAAGD_TEKST("Nieuwe FIT aangevraagd tekst", String.class),

	COLON_NIEUWE_FIT_NA_HERAANMELDING_TEKST("Nieuwe FIT na heraanmelding tekst", String.class),

	COLON_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("DK clientportaal: Toon vervangende tekst", Boolean.class),

	COLON_CLIENTPORTAAL_VERVANGENDE_TEKST("DK clientportaal: Vervangende tekst", String.class),

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

	CERVIX_OMMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL("Mail tekst voor huisarts onbekend met ommissie verstreken", String.class),

	CERVIX_OMMISSIE_VERSTREKEN_HA_ONBEKEND_MAIL_SUBJECT("Mail subject voor huisarts onbekend met ommissie verstreken", String.class),

	CERVIX_OMMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL("Mail tekst voor omissie afgegaan te laat beoordeling, met alsnog beoordeling", String.class),

	CERVIX_OMMISSIE_VERSTREKEN_ALSNOG_BEOORDELING_ONTVANGEN_MAIL_SUBJECT("Mail subject voor omissie afgegaan te laat beoordeling, met alsnog beoordeling", String.class),

	CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL("Mail tekst voor huisarts gekoppeld aan monster", String.class),

	CERVIX_HUISARTS_AAN_UITSTRIJKJE_GEKOPPELD_MAIL_SUBJECT("Mail subject voor huisarts gekoppeld aan monster", String.class),

	CERVIX_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("BMHK clientportaal: Toon vervangende tekst", Boolean.class),

	CERVIX_CLIENTPORTAAL_VERVANGENDE_TEKST("BMHK clientportaal: Vervangende tekst", String.class),

	MAMMA_MINIMALE_LEEFTIJD("Minimale leeftijd", Integer.class),

	MAMMA_MAXIMALE_LEEFTIJD("Maximale leeftijd", Integer.class),

	MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN("Minimale interval mammografie onderzoeken", Integer.class),

	MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN("Minimale interval uitnodigingen", Integer.class),

	MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN("Afspraak bij uitnodigen vanaf", Integer.class),

	MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN("Afspraak verzetten zonder client contact vanaf", Integer.class),

	MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN("Capaciteit volledig benut tot en met", Integer.class),

	MAMMA_BULK_VERZETTEN_IN_VERLEDEN_AANTAL_WEKEN("Bulk verzetten in verleden", Integer.class),

	MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST_NA_DAGEN("Op follow-up niet gedownload werklijst na dagen", Integer.class),

	MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN("Op follow-up radiologie werklijst na downloaden beelden", Integer.class),

	MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST_NA_RADIOLOGIEVERSLAG("Op follow-up pathologie werklijst na radiologieverslag", Integer.class),

	MAMMA_AFSPRAAK_LOCATIE_WIJZIGING_TEKST("Tekst bij locatiewijziging afspraak", String.class),

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

	MAMMA_PALGA_EXPORT_ALLEEN_VERWEZEN("Alleen clienten met een BVO BK uitslag \"ongunstig\" verwezen naar ziekenhuis exporteren voor Palga", Boolean.class),

	MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST("BK clientportaal: Toon vervangende tekst", Boolean.class),

	MAMMA_CLIENTPORTAAL_VERVANGENDE_TEKST("BK clientportaal: Vervangende tekst", String.class),

	CERVIX_UITSTEL_UITSLAGBRIEF_PAP3A2_OF_HOGER("Uitstel uitslagbrief na PAP3a2 of hoger", Integer.class),

	INTERNAL_COLON_COMPLICATIE_VERWERKING_STOP("Stopdatum verwerking complicaties DK", Date.class),

	INTERNAL_WSB_SCHEMATRON_VERSIONPATHMAPPING("Schematron version path mapping", String.class),

	INTERNAL_CDA_VERSIONMAPPING("Schematron cda version path mapping", String.class),

	INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES("Herinneringsperiode onvolledig adres melding", Integer.class),

	INTERNAL_MAMMA_IMS_DICOM_CMOVE_CONFIG("PACS IMS CMove connection config", String.class),

	INTERNAL_MAMMA_IMS_DICOM_CSTORE_CONFIG("PACS IMS CStore connection config", String.class),

	INTERNAL_MAX_GROOTTE_ZIP("Max grootte zip bestand in bytes", Integer.class),

	INTERNAL_MAMMA_UPLOADLIMIET_UPLOADPORTAAL("BK Uploadlimiet uitwisselportaal in megabytes", Integer.class),

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

	POSTCODE_NL_API_KEY("Postcode.nl API key", String.class),

	POSTCODE_NL_API_SECRET("Postcode.nl API secret", String.class),

	POSTCODE_NL_API_HOST("Postcode.nl API host", String.class),

	POSTCODE_NL_API_SCHEME("Postcode.nl API scheme", String.class),

	POSTCODE_NL_API_DELIVERYPATH("Postcode.nl API delivery path", String.class),

	POSTCODE_NL_API_TARGET_WPL("Target laatste update WPL", String.class),

	POSTCODE_NL_API_TARGET_NUM("Target laatste update NUM", String.class),

	MAMMA_DICOM_SOP_CONFIG("DICOM SOP configuratie", String.class),

	MAMMA_PALGA_CSV_EXPORT_AANTAL("Aantal unieke cliënten per palga export csv file", Integer.class),

	INTERNAL_UZI_LOGIN_URL_PREFIX("Prefix voor mTLS inlog UZI pas (zonder Zorg-ID)", String.class),

	MAMMA_ONDERZOEKSCAPACITEIT_NIET_BESCHIKBAAR_BINNEN_WERKDAGEN("Onderzoekscapaciteit niet beschikbaar binnen werkdagen", Integer.class),

	ILM_BEWAARTERMIJN("Bewaartermijn", Integer.class),

	ILM_BEWAARTERMIJN_BEELDEN_UITSLAG_GUNSTIG("Bewaartermijn beelden uitslag gunstig", Integer.class),

	ILM_BEWAARTERMIJN_NIET_MEDISCH("Bewaartermijn niet medisch", Integer.class),

	ILM_SIGNALEERTERMIJN_BEELDEN_STATUS("Signaleertermijn beelden status", Integer.class),

	ILM_BEZWAARTERMIJN_BEELDEN_VERWIJDERD("Bezwaartermijn beelden verwijderd", Integer.class),

	MAMMA_SE_MAX_OFFLINE_INLOGPERIODE("SE offline inloggen", Integer.class),

	MAMMA_SE_DAGLIJST_OPHALEN_DAGEN("SE daglijst ophalen dagen", Integer.class),

	INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON("SE informatie ophalen CRON", String.class);

	private final String layoutName;

	private final Class<?> clazz;

	PreferenceKey(String layoutName, Class<?> clazz)
	{
		this.clazz = clazz;
		this.layoutName = layoutName;
	}

	public Class<?> getType()
	{
		return clazz;
	}

	public String getLayoutName()
	{
		return layoutName;
	}
}
