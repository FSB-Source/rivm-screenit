package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixCISHistorieAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixDeelnamewensAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixHerdrukAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixNieuweRondeAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixUistelZwangerschapAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.actions.TestCervixVerstuurUitnodigingAction;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixAanvraagPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixBeoordeeldDoorCytologiePopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixGeanalyseerdOpHpvPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixHuisartsKoppelenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixLabformulierGecontroleerdPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixLabformulierGecontroleerdVoorCytologiePopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixLabformulierGescandPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixNietAnalyseerbaarPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixNieuweCISRonde0Popup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixOntvangenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixOrderVerstuurdPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixVervolgonderzoekBriefPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixVerzetDatumPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups.TestCervixZasAanvragenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestBewerkHuidigeUitnodigingPopUp;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestColonVerzetDatumPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestEindeScreeningRondePopUp;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestHerinneringPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestIfobtTestPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestInakeAfspraakPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestIntakeAfspraakConclusiePopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestMDLVerslagPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestRetourzendingPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups.TestScreeningRondePopUp;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaBeoordelingStatusNaarUitslagOngunstigAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaDeelnamewensAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaNieuweRondeMetOpenUitnodigingAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaOnderzoekAdhocMeekijkverzoekAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaOnderzoekDoorvoerenVanafSeAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaOnderzoekOntvangenAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.actions.TestMammaOnderzoekStartenAction;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaEersteTweeLezingenMakenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaFollowUpVastleggenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaKiesSeVoorAfspraakPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaKiesSeVoorNieuweRondeMetDatumtijdAfspraakPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaLezingMakenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaOnderzoekAfrondenPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaVerzetDatumPopup;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups.TestMammaZettenNaarArbitragePopup;
import nl.rivm.screenit.model.INaam;

public enum TestVervolgKeuzeOptie implements INaam
{
	NIEUWESCREENINGRONDEDARMKANKER("Nieuwe screeningronde darmkanker", TestScreeningRondePopUp.class),

	EINDE_SCREENINGSRONDE("Einde ronde", TestEindeScreeningRondePopUp.class),

	UITNODIGING_POPUP("Bewerk uitnodiging", TestBewerkHuidigeUitnodigingPopUp.class),

	IFOBT("Uitslag verwerken", TestIfobtTestPopup.class),

	HERINNERING("Herinnering versturen", TestHerinneringPopup.class),

	VERZET_TIJD("Zet dossier achteruit", TestColonVerzetDatumPopup.class),

	INTAKE_AFSPRAAK("Geef client een intake afspraak", TestInakeAfspraakPopup.class),

	INTAKE_AFSPRAAK_CONCLUSIE("Geef intake afspraak een conclusie", TestIntakeAfspraakConclusiePopup.class),

	MDL_VERSLAG("Geef MDL verslag", TestMDLVerslagPopup.class),

	RETOURZENDING("Retourzending voor uitnodiging", TestRetourzendingPopup.class),

	CERVIX_VERZET_TIJD("Zet dossier achteruit", TestCervixVerzetDatumPopup.class),

	CERVIX_ONTVANGEN("Ontvangen", TestCervixOntvangenPopup.class),

	CERVIX_NIET_ANALYSEERBAAR("Niet analyseerbaar", TestCervixNietAnalyseerbaarPopup.class),

	CERVIX_GEANALYSEERD_OP_HPV("Geanalyseerd op HPV", TestCervixGeanalyseerdOpHpvPopup.class),

	CERVIX_BEOORDEELD_DOOR_CYTOLOGIE("Beoordeeld door cytologie", TestCervixBeoordeeldDoorCytologiePopup.class),

	CERVIX_LABFORMULIER_GESCAND("Labformulier gescand", TestCervixLabformulierGescandPopup.class),

	CERVIX_LABFORMULIER_GECONTROLEERD("Labformulier gecontroleerd", TestCervixLabformulierGecontroleerdPopup.class),

	CERVIX_LABFORMULIER_GECONTROLEERD_VOOR_CYTOLOGIE("Labformulier gecontroleerd voor cytologie", TestCervixLabformulierGecontroleerdVoorCytologiePopup.class),

	CERVIX_AANVRAAG("Aanvraag doen", TestCervixAanvraagPopup.class),

	CERVIX_NIEUWE_RONDE("Nieuwe Ronde", TestCervixNieuweRondeAction.class),

	CERVIX_VERSTUUR_UITNODIGING("Verstuur uitnodiging", TestCervixVerstuurUitnodigingAction.class),

	CERVIX_UITSTEL_ZWANGER("Maak uitstel aan i.v.m. zwangerschap", TestCervixUistelZwangerschapAction.class),

	CERVIX_NIEUWE_CISHISTORIE("CIS historisch dossier toevoegen", TestCervixCISHistorieAction.class),

	CERVIX_NIEUWE_CIS_RONDE0("Nieuwe CIS Ronde 0", TestCervixNieuweCISRonde0Popup.class),

	CERVIX_ORDER_VERSTUURD("Order verstuurd", TestCervixOrderVerstuurdPopup.class),

	CERVIX_VERVOLGONDERZOEK_BRIEF("Controle-uitstrijkje brief", TestCervixVervolgonderzoekBriefPopup.class),

	CERVIX_ZAS("ZAS", TestCervixZasAanvragenPopup.class),

	CERVIX_HUISARTS_KOPPELEN("Huisarts koppelen", TestCervixHuisartsKoppelenPopup.class),

	CERVIX_HERDRUK("Herdruk", TestCervixHerdrukAction.class),

	CERVIX_DEELNAME_WENS("Deelnamewens aangeven voor Baarmoederhalskanker", TestCervixDeelnamewensAction.class),

	MAMMA_NIEUWE_RONDE_MET_AFSPRAAK_UITNODIGING("Nieuwe ronde met afspraak uitnodiging", TestMammaKiesSeVoorNieuweRondeMetDatumtijdAfspraakPopup.class),

	MAMMA_NIEUWE_RONDE_MET_OPEN_UITNODIGING("Nieuwe ronde met open uitnodiging", TestMammaNieuweRondeMetOpenUitnodigingAction.class),

	MAMMA_VERZET_TIJD("Zet dossier achteruit", TestMammaVerzetDatumPopup.class),

	MAMMA_AFSPRAAK("Afspraak", TestMammaKiesSeVoorAfspraakPopup.class),

	MAMMA_ONDERZOEK_STARTEN("Onderzoek starten op SE", TestMammaOnderzoekStartenAction.class),

	MAMMA_AD_HOC_MEEKIJKVERZOEK("Ad hoc meekijkverzoek op SE", TestMammaOnderzoekAdhocMeekijkverzoekAction.class),

	MAMMA_ONDERZOEK_AFRONDEN("Onderzoek afgerond op SE", TestMammaOnderzoekAfrondenPopup.class),

	MAMMA_SE_DOORVOEREN("Onderzoek doorgevoerd vanaf SE", TestMammaOnderzoekDoorvoerenVanafSeAction.class),

	MAMMA_BEELDEN_ONTVANGEN("Beelden ontvangen op BE", TestMammaOnderzoekOntvangenAction.class),

	MAMMA_EERSTE_LEZING_MAKEN("Eerste lezing maken", TestMammaLezingMakenPopup.class),

	MAMMA_TWEEDE_LEZING_MAKEN("Tweede lezing maken", TestMammaLezingMakenPopup.class),

	MAMMA_EERSTE_TWEE_LEZINGEN_MAKEN("Eerste twee lezingen maken", TestMammaEersteTweeLezingenMakenPopup.class),

	MAMMA_DISCREPANTIE_NAAR_ARBITRAGE("Naar arbitrage zetten", TestMammaZettenNaarArbitragePopup.class),

	MAMMA_DISCREPANTIE_NAAR_CONCLUSIE("Naar conclusie zetten", TestMammaLezingMakenPopup.class),

	MAMMA_ARBITRAGE_NAAR_CONCLUSIE("Naar conclusie zetten", TestMammaLezingMakenPopup.class),

	MAMMA_VERSLAG_LEZING_MAKEN("Verslag lezing maken", TestMammaLezingMakenPopup.class),

	MAMMA_STATUS_NAAR_UITSLAG_ONGUNSTIG("Naar uitslag ongunstig zetten", TestMammaBeoordelingStatusNaarUitslagOngunstigAction.class),

	MAMMA_FOLLOW_UP_VASTLEGGEN("Follow Up conclusie vastleggen", TestMammaFollowUpVastleggenPopup.class),

	MAMMA_DEELNAME_WENS("Deelnamewens aangeven voor Borstkanker", TestMammaDeelnamewensAction.class);

	private String naam;

	private Class<? extends TestVervolgKeuze> detailClass;

	TestVervolgKeuzeOptie(String knopNaam, Class<? extends TestVervolgKeuze> detailpanel)
	{
		this.naam = knopNaam;
		this.detailClass = detailpanel;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public Class<? extends TestVervolgKeuze> getDetailClass()
	{
		return detailClass;
	}
}
