package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.OrganisatieType;

public enum Recht implements INaam
{

	CLIENT_DASHBOARD("Cli\u00EBnt dashboard", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	CLIENT_GEGEVENS("Cli\u00EBnt gegevens", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	CLIENT_DOSSIER_VERWIJDEREN(
		"Cli\u00EBntdossier verwijderen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	CLIENT_AANVRAAG_VERWIJDEREN_UITSLAG_BRIEF(
		"Aanvraag verwijderen uitslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_INZIEN_LOGGING("Loginformatie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_BATCH_STATUS(
		"Batch status",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_SELECTIE_VERWERKING_VERSLAG("Selectie verwerking", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_GBA_VERWERKING_VERSLAG("Gba verwerking", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_IFOBT_VERWERKING_VERSLAG("FIT verwerking", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_BRIEVEN_VERWERKING_VERSLAG(
		"Brieven genereren verwerking",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CDA_VERWERKING_FOUT_VERSLAG(
		"Cda verwerking fout",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_INTAKE_VERWERKING_VERSLAG(
		"Verslag intake uitgeven",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_FORMULIER_BEHEER(
		"Beheer colon formulierdefinities",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_DEFINITIE_VRAGENLIJSTEN(
		"Beheer vragenlijsten",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_VERSLAGEN(
		"Beheer verslagen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.AANPASSEN),

	GEBRUIKER_GEVOLGEN_LABPROCES_VERWERKEN_VERSLAG(
		"Gevolgen labproces verwerken verslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_HERINNEREN_VERWERKEN_VERSLAG(
		"Herinneren verwerken verslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_HUISARTSBERICHTEN_VERSLAG(
		"Huisartsberichten verslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BEHEER_PARAMETERISATIE(
		"Beheer parameterisatie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.AANPASSEN),

	GEBRUIKER_ROLLEN_BEHEREN(
		"Beheer rollen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_BEHEER_BRIEVEN(
		"Brieven beheer",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_BEHEER_GEBIEDEN(
		"Beheer gebieden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN)
	{

		@Override
		public List<OrganisatieType> getOrganisatieTypes()
		{
			return Arrays.asList(OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE);
		}

	},

	GEBRUIKER_BEHEER_GEBIEDEN_PERC_IFOBT_RETOUR(
		"Beheer gebieden FIT retourpercentage",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_BEHEER_GEBIEDEN_PERC_ONGUNSTIGE_IFOBT(
		"Beheer gebieden FIT ongunstigepercentage",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_BEHEER_GEBIEDEN_ADHERENTIE_AANPASSEN(
		"Beheer adherentie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	GEBRUIKER_LOCATIE_ROOSTER("Beheer rooster", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON })
	{

		@Override
		public List<OrganisatieType> getOrganisatieTypes()
		{
			return Arrays.asList(OrganisatieType.COLOSCOPIECENTRUM);
		}
	},

	GEBRUIKER_UITNODIGING_VERSTUREN("Uitnodiging versturen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_SCREENING_SCANNING("Screening scanning", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_MEDEWERKER_BEHEER("Medewerker beheer", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_GBA_AANVRAGEN("Gba gegevens opvragen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_GBA_TIJDELIJK_ADRES("Gba tijdelijk adres", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_MEDEWERKER_ORGANISATIES_BEHEER(
		"Medewerker organisaties",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_COLOSCOPIECENTRUM_ORG_BEHEER(
		"Beheer intakelocatie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_BEHEER_DASHBOARD(
		"Beheer dashboard",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.AANPASSEN),

	GEBRUIKER_BEHEER_DASHBOARD_GEZIEN_KNOP(
		"Beheer dashboard gezien knop",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES(
		"Documenttemplates testen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	VERVANGEN_DOCUMENTEN(
		"Documenten kunnen vervangen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_BEHEER_CC_LOCATIES("Beheer intakelocatie kamers", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_BEHEER_CC_GEBIEDEN("Beheer intakelocatie gebieden", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_INPAKCENTRUM_ORG_BEHEER(
		"Beheer inpakcentrum",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENINGS_ORG_BEHEER(
		"Beheer screeningsorganisatie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_LABORATORIA_BEHEER(
		"Beheer FIT laboratoria",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_PROJECT_OVERZICHT("Project overzicht", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_PROJECT_BRIEVEN("Project brieven", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_PROJECT_SELECTIE("Project selectie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_BRIEFPROJECT_OVERZICHT("Briefproject overzicht", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_BRIEFPROJECT_BRIEVEN("Briefproject brieven", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_BRIEFPROJECT_SELECTIE("Briefproject selectie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_PROJECT_VRAGENLIJST_ANTWOORDEN_INZIEN(
		"Vragenlijst antwoorden inzien",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		null,
		Actie.INZIEN),

	GEBRUIKER_UITSLAG_PROJECT_UPLOADEN(
		"Uitslagen studietest projecten uploaden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_PA_LABORATORIA_BEHEER(
		"Beheer PA laboratoria",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_ZORGVERZEKERAARS_BEHEER("Beheer zorgverzekeraars", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER(
		"Beheer huisartsenpraktijken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_BMHK_LABORATORIA_BEHEER(
		"Beheer BMHK Laboratoria",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_BMHK_LABORATORIA_OVERZICHT_VERRICHTINGEN(
		"Overzicht verrichtingen BMHK Laboratoria",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BMHK_HUISARTS_OVERZICHT_VERRICHTINGEN(
		"Overzicht verrichtingen huisarts",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BMHK_LABORATORIUM_ORDER_VERWERKEN(
		"Order verwerking voor BMHK laboratorium",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.AANPASSEN),

	GEBRUIKER_ZORGINSTELLING_ORG_BEHEER(
		"Beheer zorginstelling",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_COLOSCOPIELOCATIE_ORG_BEHEER(
		"Beheer coloscopielocatie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_RIVM_BEHEER(
		"Beheer landelijk",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_KWALITEITSPLATFORM_BEHEER(
		"Beheer kwaliteitsplatform",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_CLIENT_GEGEVENS("Cli\u00EBnt gegevens", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_ZOEKEN(
		"Cli\u00EBnt zoeken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		null,
		Actie.INZIEN),

	GEBRUIKER_CLIENT_INZIEN_FORMULIER_NA_HPVMIN(
		"Inzien BMHK-formulier na HPV(-)",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CLIENT_TELEFOONNUMMER_REGISTREREN(
		"Cli\u00EBnt telefoonnummer registreren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_SCREENINGSRONDE("Screeningsronde", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_CONTACT("Registreren cli\u00EBnt contact", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN("Registreren complicaties", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_BEZWAAR("Bezwaar cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS(
		"Aanvraag overdracht persoonsgegevens",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS(
		"Download overdracht persoonsgegevens",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		null,
		new Actie[] { Actie.INZIEN }),

	GEBRUIKER_CLIENT_CIS_HISTORIE(
		"CIS Historie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.INZIEN }),

	GEBRUIKER_CLIENT_UITSTEL("Uitstel vragen cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_CLIENT_SR_AANVRAAGFORMULIER_ONTVANGEN("Antwoordformulier cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_UITSLAGIFOBTONTVANGEN("Uitslag FIT cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT("Afspraak intake cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_NIEUWE_INTAKEAFSPRAAKGEMAAKT("Intake afspraak cli\u00EBnt maken", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_INTAKE_WIJZIGEN_ANDER_BRIEF(
		"Intake afspraak cli\u00EBnt wijzigen met andere brief",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		null,
		Actie.AANPASSEN),

	GEBRUIKER_CLIENT_SR_INTAKE_VERPLAATS_BINNEN_INTAKE_NIET_WIJZIGBAAR_PERIODE(
		"Intake afspraak cli\u00EBnt verplaatsen binnen de intake niet wijzigbaar periode",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		null,
		Actie.AANPASSEN),

	GEBRUIKER_CLIENT_COLON_AFMELDEN("Afmelden cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_COLON_HERAANMELDEN("Heraanmelden cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_UITSLAGINTAKEONTVANGEN("Uitslag Intake cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_UITSLAGCTCOLOGRAFIEONTVANGEN("Uitslag CT Colografie cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN("Uitslag Coloscopie cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN("Uitslag Pathologie cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_NIEUWE_IFOBT_AANVRAGEN("Status cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_CONCLUSIE("Conclusie intake", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN(
		"Brieven opnieuw klaarzetten",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN(
		"Brieven tegenhouden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		null,
		Actie.AANPASSEN),

	GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_OPNIEUW_VERZENDEN(
		"Huisarts bericht opnieuw verzenden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		null,
		Actie.AANPASSEN),

	GEBRUIKER_CLIENT_SR_HUISARTS_KOPPELEN(
		"Huisarts koppelen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	GEBRUIKER_SCREENING_PRINTER("Screening brieven printen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_SCREENING_PRINTER_PROJECTBRIEVEN(
		"Screening projectbrieven printen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_SCREENING_NIETTEBEOORDELEN("Niet te beoordelen monsters scannen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN(
		"Verwerken ongeldige berichten",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_SCREENING_IFOBT_BATCH("FIT batch registratie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }, new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_INTAKE_WERKLIJST("Werklijst intake", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_SCREENING_DEFINITIEF_VERVOLGDBELEID("Beheren definitief vervolgbeleid", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_SCREENING_RETOURZENDINGEN(
		"Retourzendingen verwerking",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_RETOURREDENKOPPELEN(
		"Retourredenen koppelen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_BEHEER_OVEREENKOMSTEN_MODELLEN("Beheer overeenkomstenmodellen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_OVEREENKOMSTEN_BEHEER("Beheer overeenkomsten", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_OVEREENKOMSTEN_ZOEKEN("Zoeken overeenkomsten", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }, new ToegangLevel[] { ToegangLevel.LANDELIJK }, Actie.INZIEN),

	GEBRUIKER_ORGANISATIE_DOCUMENTEN("Documenten bij organisatie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_CLIENT_DOCUMENTEN("Documenten bij cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_PROJECTEN("Projecten bij cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_SCREENING_ZOEKENOPBARCODE("Zoeken op barcode FIT monster", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	HANDLEIDINGEN_DOWNLOADEN(
		"Handleidingen downloaden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_BEHEER_HUISARTSIMPORT(
		"Beheer importeren huisartsen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	HUISARTSBERICHT_TEMPLATE(
		"Beheer huisartsbericht template",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_WIJZIGEN_HUISARTS(
		"Huisarts wijzigen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_KLAARZETTEN_CERVIX_HUISARTS(
		"Klaarzetten BMHK huisarts",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN),

	GEBRUIKER_AFMELDEN_PROEF_BEVOLKINGSONDERZOEK("Registreren afmelding ikv proefBVO", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_HERAANMELDEN_PROEF_BEVOLKINGSONDERZOEK("Registreren heraanmelding ikv proefBVO", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	GEBRUIKER_IMPORT_CAP_VERDELING(
		"Import capaciteitsverdeling",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	CLIENT_TOOLTIP_BEHEER(
		"Beheer Client Tooltip",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_RAPPORTAGE(
		"Rapportage",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_RAPPORTAGE_WEBFOCUS(
		"Rapportage - WebFocus",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BEHEER_SENTINELCONTROLES(
		"Beheer Sentinelcontroles",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN),

	GEBRUIKER_BEHEER_SKML_INTERNE_CONTROLE(
		"Beheer SKML interne controle",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN),

	GEBRUIKER_BEHEER_SCHEMA_EXTERNE_CONTROLE(
		"Beheer schema externe kwaliteitscontroles",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_BEHEER_REGISTRATIE_KWALITEITSCONTROLE(
		"Registratie voor kwaliteitscontrole",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.AANPASSEN,
		Actie.TOEVOEGEN),

	GEBRUIKER_SCREENING_BEOORDELING_IFOBT(
		"Beoordeling FIT",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_SCREENING_AUTORISATIE_IFOBT(
		"Autorisatie FIT",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.AANPASSEN),

	GEBRUIKER_SCREENING_VERWIJDEREN_BATCHES_IFOBT(
		"Verwijderen batches FIT",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.VERWIJDEREN),

	GERBRUIKER_CERVIX_LABFORMULIEREN_AANVRAGEN(
		"Aanvragen van BMHK labformulieren voor huisarts",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.TOEVOEGEN),

	TESTEN("Testen", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }, new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	TECHNISCH_BEHEER(
		"Technisch beheer",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_BEZWAAR_BRP(
		"Bezwaar BRP",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	GEBRUIKER_KOPPELRESULTATEN_KANKERREGISTRATIE(
		"Upload koppelbestand Kankerregistratie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	NIEUWS_WIJZIGEN(
		"Beheer Nieuws",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_CLIENT_CERVIX_AFMELDEN("Afmelden cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_CLIENT_CERVIX_AANMELDEN_DEELNAME_BUITEN_BVO_BMHK(
		"Aanmelden cli\u00EBnt voor deelname buiten bvo BMHK",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_CLIENT_CERVIX_HERAANMELDEN("Heraanmelden cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	GEBRUIKER_CERVIX_LABFORMULIEREN_CONTROLEREN(
		"Formulieren controleren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.INZIEN,
		Actie.AANPASSEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_CERVIX_LABFORMULIEREN_CONTROLEREN_VOOR_CYTOLOGIE(
		"Formulieren controleren voor cytologie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.INZIEN,
		Actie.AANPASSEN),

	GEBRUIKER_CERVIX_LABFORMULIEREN_ZOEKEN_VOOR_CYTOLOGIE(
		"Formulieren zoeken voor cytologie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.INZIEN),

	GEBRUIKER_CERVIX_LABFORMULIEREN_HUISARTS_ONBEKEND(
		"Formulieren huisarts onbekend",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.INZIEN,
		Actie.AANPASSEN),

	GEBRUIKER_CERVIX_HUISARTS_TARIEF(
		"Huisartstarief",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_CERVIX_LABORATORIUM_TARIEF(
		"Laboratoriumtarief",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_CERVIX_ONTVANGST_MONSTER(
		"Ontvangst monster",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.AANPASSEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_CERVIX_STATUS_MONSTER("Status monster", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }, new ToegangLevel[] { ToegangLevel.INSTELLING }, Actie.INZIEN),

	GEBRUIKER_CLIENT_SR_CERVIX_HUISARTSBERICHT_DETAILS(
		"Huisartsbericht details",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CLIENT_SR_CERVIX_HPV_UITSLAG_INZIEN(
		"HPV uitslag client",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CLIENT_VERWIJDEREN_RESULTATEN_MONSTER(
		"Verwijderen resultaten monster",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_CLIENT_SR_CERVIX_INZIEN_BMHK_FORMULIER(
		"Inzien BMHK-formulier",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CERVIX_CYTOLOGIE_VERSLAG("Cytologie verslag cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }, null, Actie.INZIEN),

	UITSTRIJKEND_ARTS("Uitstrijkend arts", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }, new ToegangLevel[] { ToegangLevel.INSTELLING }, Actie.AANPASSEN),

	GEBRUIKER_CERVIX_BARCODES_AFDRUKKEN(
		"Afdrukken barcodes controlemonsters",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.INZIEN),

	GEBRUIKER_CERVIX_CLIENT_ZAS_AANVRAGEN(
		"ZAS aanvragen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_CERVIX_CLIENT_HERDRUK(
		"Aanvragen nieuwe uitnodiging",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_SCREENING_ZAS_BATCH("ZAS reeks registratie", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }, new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_BETALINGEN_BMHK(
		"Betalingen BMHK exporteren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.TOEVOEGEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_SCREENING_MAMMA_PLANNING("Planning", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }, new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_MAMMA_SE_BEHEER("SE beheer", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }, new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN(
		"Connectiestatus inzien op SE",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_SCREENING_MAMMA_TEHUIS("Tehuis beheer", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }, new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER(
		"Beheer mammapoli",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_MAMMA_RADIOLOGIEAFDELING_ORG_BEHEER(
		"Beheer radiologie afdeling",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK }),

	GEBRUIKER_SCREENING_MAMMA_AFSPRAKEN_BEHEER(
		"Afspraken beheer",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CLIENT_MAMMA_AFSPRAKEN(
		"Afspraken cli\u00EBnt",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.INZIEN }),

	GEBRUIKER_CLIENT_MAMMA_RONDE_FORCEREN(
		"Ronde forceren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.TOEVOEGEN }),

	GEBRUIKER_CLIENT_MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS(
		"Mindervalide onderzoek ziekenhuis",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS_TERUGDRAAIEN(
		"Mindervalide onderzoek ziekenhuis terugdraaien",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_INFOBRIEF_PROTHESEN(
		"Infobrief prothesen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.TOEVOEGEN }),

	GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_MAKEN(
		"Afspraak maken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.TOEVOEGEN }),

	GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN(
		"Afspraak wijzigen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_MAMMA_AFSPRAAK_BULK_VERZETTEN(
		"Bulk verzetten",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_BUITEN_REGIO(
		"Afspraak buiten regio",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_MAKEN_FORCEREN(
		"Afspraak forceren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.TOEVOEGEN }),

	GEBRUIKER_CLIENT_MAMMA_AFMELDEN(
		"Afmelden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_HERAANMELDEN(
		"Heraanmelden",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.AANPASSEN }),

	GEBRUIKER_CLIENT_MAMMA_HERBEOORDELEN(
		"Herbeoordeling aanvragen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN,
		Actie.VERWIJDEREN),

	GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING(
		"IMS desktop sync",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		new Actie[] { Actie.INZIEN }),

	GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST(
		"Beoordeling werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING }),

	GEBRUIKER_SCREENING_MAMMA_ARBITRAGE_WERKLIJST(
		"Arbitrage werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING }),

	GEBRUIKER_SCREENING_MAMMA_DOSSIERGEGEVENS(
		"Dossiergegevens",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_CENTRALE_EENHEID_ORG_BEHEER(
		"Beheer centrale eenheid",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_CENTRALE_EENHEID_VERWIJSVERSLAGEN_CONTROLLEREN(
		"CE verwijsverslagen controleren",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_CENTRALE_EENHEID_OPSCHORTEN_BEOORDELINGEN(
		"CE opgeschorte beoordelingen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_CENTRALE_EENHEID_ONDERBROKEN_ONDERZOEKEN(
		"CE onderbroken onderzoeken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_CENTRALE_EENHEID_PROCESMONITORING(
		"CE procesmonitoring",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_CENTRALE_EENHEID_GEEN_BEOORDELING_MOGELIJK(
		"CE geen beoordeling mogelijk",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_CENTRALE_EENHEID_UPLOADVERZOEKEN(
		"CE uploadverzoeken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_NIET_GEDOWNLOAD_WERKLIJST(
		"Follow-up niet gedownload werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST(
		"Follow-up radiologie werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST(
		"Follow-up pathologie werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_CONCLUSIE_WERKLIJST(
		"Follow-up conclusie werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO },
		Actie.AANPASSEN),

	GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER(
		"Beheer beoordelingseenheid",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO }),

	GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN(
		"Inschrijven op SE",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK(
		"Onderzoek starten op SE",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN(
		"Signaleren op SE",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME(
		"Kwaliteitsopname op SE",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_CLIENT_MAMMA_DOELGROEP_WIJZIGEN(
		"Doelgroep wijzigen",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	GEBRUIKER_TESTPORTAAL(
		"Testportaal gebruiken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.AANPASSEN),

	GEBRUIKER_MAMMA_UITNODIGEN_VERWERKING_VERSLAG(
		"Uitnodigen verwerking",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_ILM_VERWERKING_VERSLAG(
		"ILM verwerkingsverslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_EXCHANGE(
		"Uitwisselingsportaal rechten (downloaden)",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_EXCHANGE_UPLOAD(
		"Uitwisselingsportaal rechten (uploaden)",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_RADIOLOGIE(
		"Follow-up radiologie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.INSTELLING },
		Actie.TOEVOEGEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_INZAGE_RADIOLOGIE_VERSLAG(
		"Inzage follow-up radiologieverslag",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_INZAGE_UPLOADVERZOEK(
		"Inzage uploadverzoek",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_BEELDEN_VERWIJDEREN_UPLOADVERZOEK(
		"Beelden verwijderen uploadverzoek",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),

	GEBRUIKER_FOTOBESPREKING(
		"Fotobespreking",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_BEELDEN_BEKIJKEN(
		"Beelden bekijken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_BEELDEN_PORTFOLIO(
		"Beelden portfolio bekijken",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_VISITATIE(
		"Visitatie",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_AD_HOC_MEEMKIJKVERZOEK_WERKLIJST(
		"Ad hoc meekijkverzoek werklijst",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK }),

	GEBRUIKER_MAMMA_INZIEN_ONDERZOEK(
		"Inzien onderzoek",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_INZIEN_LEZING(
		"Inzien lezing",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.INZIEN),

	GEBRUIKER_MAMMA_FOLLOW_UP_VERSLAG("Follow Up verslag cli\u00EBnt", new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	GEBRUIKER_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG(
		"Client wil geen onderzoek vervolg",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_CLIENT_OPROEP_NA_ONDERBROKEN_ONDERZOEK(
		"Oproepbrief na onderbroken onderzoek",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.REGIO, ToegangLevel.LANDELIJK },
		Actie.TOEVOEGEN),

	GEBRUIKER_MAMMA_PALGA_CSV_UITWISSELING(
		"Palga CSV uitwisseling",
		new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA },
		new ToegangLevel[] { ToegangLevel.LANDELIJK },
		Actie.VERWIJDEREN),
		;

	private final String omschrijving;

	private final Bevolkingsonderzoek[] bevolkingsonderzoeken;

	private final Actie[] actie;

	private final ToegangLevel[] level;

	Recht(String omschrijving, Bevolkingsonderzoek[] bevolkingsonderzoeken, ToegangLevel[] level, Actie... actie)
	{
		this.omschrijving = omschrijving;
		if (bevolkingsonderzoeken != null)
		{
			this.bevolkingsonderzoeken = bevolkingsonderzoeken.clone();
		}
		else
		{
			this.bevolkingsonderzoeken = null;
		}
		if (level != null)
		{
			this.level = level.clone();
		}
		else
		{
			this.level = null;
		}
		this.actie = actie;
	}

	Recht(String omschrijving, Bevolkingsonderzoek[] bevolkingsonderzoeken)
	{
		this(omschrijving, bevolkingsonderzoeken, null);
	}

	public String getOmschrijving()
	{
		return omschrijving;
	}

	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	public Actie[] getActie()
	{
		return actie;
	}

	public ToegangLevel[] getLevel()
	{
		return level;
	}

	@Override
	public String getNaam()
	{
		return getOmschrijving();
	}

	public List<OrganisatieType> getOrganisatieTypes()
	{
		return null;
	}
}
