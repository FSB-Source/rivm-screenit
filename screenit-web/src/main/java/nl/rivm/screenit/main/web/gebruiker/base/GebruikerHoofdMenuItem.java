
package nl.rivm.screenit.main.web.gebruiker.base;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.algemeen.batch.BatchStartPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.bezwaar.BezwaarBRPIntrekkenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.brieventemplates.BriefBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.formulierdefinities.FormulierenBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.handleidingen.HandleidingenDownloadPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.intervalcarcinoom.UploadKoppelresultatenKankerregistratiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.LoggingInzienPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerZoeken;
import nl.rivm.screenit.main.web.gebruiker.algemeen.nieuws.BeheerNieuwsPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.nieuws.NieuwsPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten.OvereenkomstenBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.CervixParameterisatiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ColonParameterisatiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.MammaParameterisatiePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project.ProjectOverzicht;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projectvragenlijsten.ProjectVragenlijstenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.retourzending.RetourzendingenVerwerkenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.rollenrechten.RollenBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer.CorrectiesPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.tooltip.ClientTooltipBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.dashboard.DashboardPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.BezwaarAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.CervixAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.CervixRegioAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.ColonAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.MammaAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken.ProjectAfdrukkenDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.rapportage.RapportagePage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie.CervixBetalingPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.houdbaarheid.zasreeks.ZASReeksOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.huisarts.CervixKlaarzettenHuisartsPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.kwaliteitsborging.CervixBarcodesAfdrukkenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.aanvragen.CervixLabformulierAanvraagPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixLabformulierenControlerenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixLabformulierenControlerenVoorCytologiePage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixLabformulierenHuisartsOnbekendPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixLabformulierenZoekenVoorCytologiePage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixOrdersVerwerkenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster.CervixOntvangstMonsterPage;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster.CervixStatusMonsterPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden.GemeenteZoeken;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.houdbaarheid.ifobtbatch.IFOBTBatchOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.huisartsimport.HuisartsImportBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.importcapverdeling.ImportCapVerdelingPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.intake.ColonGeplandeIntakesWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.KwaliteitscontroleBatchOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks.SentinelControlesPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.niettebeoordelen.NietTeBeoordelenMonstersPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekenBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster.RoosterPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.proefbvo.ProefBVOAfmeldenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen.VerwerkteBerichtenOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.zoekenopbarcode.ZoekenOpBarcodePage;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.CervixVerwerkOngeldigeBerichtenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.ColonVerwerkOngeldigeBerichtenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.MammaVerwerkOngeldigeBerichtenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken.MammaAfsprakenSEZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MammaBeoordelingenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.geenbeoordelingmogelijk.MammaCeGeenBeoordelingMogelijkWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.onderbroken.MammaCeOnderbrokenOnderzoekenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.opschorting.MammaCeOpgeschorteBeoordelingenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.procesmonitoring.MammaCeProcesmonitoringWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken.MammaCeUploadBeeldenVerzoekWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.verwijsverslag.MammaCeVerwijsVerslagenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.MammaExchangeDownloadPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.MammaExchangeUploadPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup.MammaFollowUpRadiologieVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followup.MammaFollowUpNietGedownloadWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie.MammaFollowUpConclusieWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie.MammaFollowUpPathologieRegioWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie.MammaFollowUpRadiologieRegioWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.adhoc.MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden.MammaBeeldenZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden.MammaPortfolioZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking.MammaFotobesprekingOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOverzichtPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.palga.MammaPalgaUitwisselingPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardPage;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.CervixTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.ColonTestProcesPage;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.ColonTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.preferences.TestPreferencesPage;

public enum GebruikerHoofdMenuItem
{
	DASHBOARD
	{
		@Override
		public GebruikerMenuItem getMenuItem()
		{
			return new GebruikerMenuItem("menu.dashboard", DashboardPage.class);
		}

	},

	CLIENTEN
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			return new GebruikerMenuItem("menu.clienten", ClientZoekenPage.class);
		}

	},

	CERVIX
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			List<IMenuItem> contextMenuItems = new ArrayList<>();
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.uitstrijkend.arts.klaarzetten", CervixKlaarzettenHuisartsPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren.aanvragen", CervixLabformulierAanvraagPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.zasbatch", ZASReeksOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.barcodes-afdrukken", CervixBarcodesAfdrukkenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.ontvangstmonster", CervixOntvangstMonsterPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.statusmonster", CervixStatusMonsterPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren-controleren", CervixLabformulierenControlerenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren-controleren-voor-cytologie", CervixLabformulierenControlerenVoorCytologiePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren-zoeken-voor-cytologie", CervixLabformulierenZoekenVoorCytologiePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren-huisarts-onbekend", CervixLabformulierenHuisartsOnbekendPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.orders-verwerken", CervixOrdersVerwerkenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.screening.verwerkenongeldigeberichten", CervixVerwerkOngeldigeBerichtenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.betaling", CervixBetalingPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.all.afdrukkendocumenten", CervixAfdrukkenDocumentenPage.class, CervixRegioAfdrukkenDocumentenPage.class));
			return new GebruikerMenuItem("menu.cervix", contextMenuItems);
		}

	},

	MAMMA
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			List<IMenuItem> contextMenuItems = new ArrayList<>();
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.planning", MammaPlanningDashboardPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.afspraken", MammaAfsprakenSEZoekenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.beoordeling", MammaBeoordelingenWerklijstPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.centrale-eenheid", MammaCeVerwijsVerslagenWerklijstPage.class,
				MammaCeUploadBeeldenVerzoekWerklijstPage.class, MammaCeGeenBeoordelingMogelijkWerklijstPage.class, MammaCeOpgeschorteBeoordelingenWerklijstPage.class,
				MammaCeProcesmonitoringWerklijstPage.class, MammaCeOnderbrokenOnderzoekenWerklijstPage.class));
			contextMenuItems.add(
				new GebruikerMenuItem("menu.mammascreening.exchange", MammaExchangeDownloadPage.class, MammaExchangeUploadPage.class, MammaFollowUpRadiologieVerslagPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.fotobespreking", MammaFotobesprekingOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.follow-up", MammaFollowUpNietGedownloadWerklijst.class, MammaFollowUpRadiologieRegioWerklijstPage.class,
				MammaFollowUpPathologieRegioWerklijstPage.class, MammaFollowUpConclusieWerklijst.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.beelden", MammaBeeldenZoekenPage.class, MammaPortfolioZoekenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.screening.verwerkenongeldigeberichten", MammaVerwerkOngeldigeBerichtenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.visitatie", MammaVisitatieOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.adhockwaliteitscontrole", MammaAdhocMeekijkverzoekOnderzoekenWerklijstPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.all.afdrukkendocumenten", MammaAfdrukkenDocumentenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.palga", MammaPalgaUitwisselingPage.class));
			return new GebruikerMenuItem("menu.mamma", contextMenuItems);
		}

	},

	COLON
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			List<IMenuItem> contextMenuItems = new ArrayList<>();
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.overeenkomstenzoeken", OvereenkomstZoekenBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.gebieden", GemeenteZoeken.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.importcapverdeling", ImportCapVerdelingPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.huisartsimport", HuisartsImportBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.kwaliteitscontrole.reeks", SentinelControlesPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.kwaliteitscontrole", KwaliteitscontroleBatchOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.ifobtbatch", IFOBTBatchOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.zoekenopbarcode", ZoekenOpBarcodePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.niettebeoordelen", NietTeBeoordelenMonstersPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.verslagen", VerwerkteBerichtenOverzichtPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.screening.verwerkenongeldigeberichten", ColonVerwerkOngeldigeBerichtenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.proefbvo", ProefBVOAfmeldenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.rooster", RoosterPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.colonscreening.werklijstintake", ColonGeplandeIntakesWerklijstPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.all.afdrukkendocumenten", ColonAfdrukkenDocumentenPage.class));
			return new GebruikerMenuItem("menu.colon", contextMenuItems);
		}

	},

	ALGEMEEN
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			List<IMenuItem> contextMenuItems = new ArrayList<>();
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerker", MedewerkerZoeken.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.organisatie", OrganisatieZoeken.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.overeenkomsten", OvereenkomstenBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.rollen", RollenBeheer.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.batchstatus", BatchStartPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.logginginzien", LoggingInzienPage.class));
			contextMenuItems
				.add(new GebruikerMenuItem("menu.algemeen.parameterisatie", ColonParameterisatiePage.class, CervixParameterisatiePage.class, MammaParameterisatiePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.technischbeheer", CorrectiesPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.formulierendefinitie", FormulierenBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.clienttooltipbeheer", ClientTooltipBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.nieuwsbeheer", BeheerNieuwsPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.nieuws", NieuwsPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.projecten", ProjectOverzicht.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.vragenlijsten", ProjectVragenlijstenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.handleidingen", HandleidingenDownloadPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.retourzendingen", RetourzendingenVerwerkenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.bezwaar.brpintrekken", BezwaarBRPIntrekkenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.upload.koppelresultaat.kankerregistratie", UploadKoppelresultatenKankerregistratiePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.brieventemplates", BriefBeheerPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.brieventemplates.testen", DocumentTemplateTestenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.all.afdrukkendocumenten", BezwaarAfdrukkenDocumentenPage.class, ProjectAfdrukkenDocumentenPage.class));
			return new GebruikerMenuItem("menu.algemeen", contextMenuItems);
		}

	},

	RAPPORTAGE
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			return new GebruikerMenuItem("menu.rapportage", RapportagePage.class);
		}

	},

	TESTEN
	{

		@Override
		public GebruikerMenuItem getMenuItem()
		{
			List<IMenuItem> contextMenuItems = new ArrayList<>();
			contextMenuItems.add(new GebruikerMenuItem("menu.testen", TestPreferencesPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.testen.colon", ColonTestProcesPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline.bmhk", CervixTestTimelinePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline.bk", MammaTestTimelinePage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline.dk", ColonTestTimelinePage.class));
			return new GebruikerMenuItem("menu.testen", contextMenuItems);
		}
	},

	;

	public abstract GebruikerMenuItem getMenuItem();
}
