
package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AntwoordformulierOntvangenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.BriefKlaargezetPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.DigitaalClientBerichtVerzondenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.IFobtVerslagPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix.CervixCytologieVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix.CervixExtraHuisartsberichtDetailsPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix.CervixHpvInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix.CervixHuisartsberichtDetailsPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix.CervixLabformulierInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon.ColonHuisartsInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon.ColonHuisartsberichtDetailsPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon.ConclusieInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaFollowUpRadiologieGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaHerbeoordelingGebeurtenisPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaHuisartsInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaHuisartsberichtDetailsPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaLezingInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaOnderzoekInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaUploadBeeldenVerzoekGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaVerslagInzienPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.project.VragenlijstInzienPanel;

public enum TypeGebeurtenis
{

	VOORAANKONDIGING(BriefKlaargezetPanel.class),

	UITNODIGING(),

	UITNODIGING_AANGEMAAKT(),

	BRIEF_AANGEMAAKT(BriefKlaargezetPanel.class),

	BRIEF_KLAARGEZET(BriefKlaargezetPanel.class),

	BRIEF_AFGEDRUKT(BriefKlaargezetPanel.class),

	BRIEF_TEGENHOUDEN(BriefKlaargezetPanel.class),

	BRIEF_VERVANGEN(BriefKlaargezetPanel.class),

	BRIEF_HERDRUK(BriefKlaargezetPanel.class),

	MAIL_VERZONDEN(DigitaalClientBerichtVerzondenPanel.class),

	MAIL_OPNIEUW_VERZONDEN(DigitaalClientBerichtVerzondenPanel.class),

	SMS_VERZONDEN(DigitaalClientBerichtVerzondenPanel.class),

	SMS_VERSTUREN_GEFAALD,

	ANTWOORDFORMULIERONTVANGEN(AntwoordformulierOntvangenPanel.class),

	AFNAMEDATUM_INGEVULD_OP_PORTAAL(),

	IFOBTVERLOREN(),

	RETOURPERIODEIFOBTVERSTREKEN(),

	IFOBTNIETTEBEOORDELEN(),

	VERVALDATUMIFOBTVERLOPEN(),

	UITSLAGIFOBTONTVANGEN(IFobtVerslagPanel.class),

	UITSLAGIFOBTONBETROUWBAAR(),

	INTAKEAFSPRAAKGEMAAKT(),

	INTAKEAFSPRAAKAFGEZEGD(),

	CONCLUSIEINTAKEONTVANGEN(ConclusieInzienPanel.class),

	CONCLUSIEINTAKEVERWIJDERD(),

	UITSLAGINTAKEONTVANGEN(),

	UITSLAGCTCOLOGRAFIEONTVANGEN(),

	UITSLAGCOLOSCOPIEONTVANGEN(),

	UITSLAGPATHOLOGIEONTVANGEN(),

	AFMELDING_EENMALIG(),

	AFMELDING_TIJDELIJK(),

	HERAANMELDING(),

	AFGEROND(),

	RETOURZENDING_BESTAND(),

	RETOURZENDING_HANDMATIG(),

	AFMELDFORMULIER_TOEGEVOEGD(),

	INACTIEF_REDEN(),

	UITNODIGINGSPAKKET_SAMENGESTELD(),

	HUISARTSBERICHT_VERZONDEN(),

	HUISARTSBERICHT_KLAARGEZET(),

	PROJECT_GEACTIVEERD(),

	PROJECT_GEDEACTIVEERD(),

	PROJECT_TOEGEVOEGD(),

	PROJECT_BRIEF_HERDRUK(BriefKlaargezetPanel.class),

	PROJECT_BRIEF_AANGEMAAKT(BriefKlaargezetPanel.class),

	PROJECT_BRIEF_KLAARGEZET(BriefKlaargezetPanel.class),

	PROJECT_BRIEF_AFGEDRUKT(BriefKlaargezetPanel.class),

	PROJECT_BRIEF_TEGENHOUDEN(BriefKlaargezetPanel.class),

	PROJECT_BRIEF_VERVANGEN(BriefKlaargezetPanel.class),

	PROJECT_VRAGENLIJST_ONTVANGEN_PAPIER(VragenlijstInzienPanel.class),

	PROJECT_VRAGENLIJST_ONTVANGEN_PAPIER_VERWIJDERD(VragenlijstInzienPanel.class),

	PROJECT_VRAGENLIJST_ONTVANGEN_DIGITAAL(VragenlijstInzienPanel.class),

	COLON_HUISARTS_TOEGEVOEGD(ColonHuisartsInzienPanel.class),

	COLON_HUISARTSBERICHT_AANGEMAAKT(ColonHuisartsberichtDetailsPanel.class),

	COLON_HUISARTSBERICHT_VERSTUURD(ColonHuisartsberichtDetailsPanel.class),

	COLON_HUISARTSBERICHT_VERSTUREN_MISLUKT(ColonHuisartsberichtDetailsPanel.class),

	COLON_HUISARTSBERICHT_OPNIEUW_VERSTUURD(ColonHuisartsberichtDetailsPanel.class),

	COLON_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT(ColonHuisartsberichtDetailsPanel.class),

	UITSTEL(),

	MAMMA_ACHTERVANG_UITSTEL(),

	MAMMA_MINDER_VALIDE_UITWIJK(),

	CIS_PAP0(),

	BMHK_MONSTER_HPV_GEANALYSEERD(CervixHpvInzienPanel.class),

	BMHK_MONSTER_ONTVANGEN(),

	BMHK_CYTOLOGISCHE_BEOORDELING(CervixCytologieVerslagInzienPanel.class),

	BMHK_MONSTER_NIET_ANALYSEERBAAR(),

	BMHK_MONSTER_NIET_ONTVANGEN(),

	BMHK_RESULTATEN_MONSTER_VERWIJDERD(),

	BMHK_LABFORMULIER_GECONTROLEERD(CervixLabformulierInzienPanel.class),

	BMHK_HUISARTSBERICHT_AANGEMAAKT(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_VERSTUURD(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_VERSTUREN_MISLUKT(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_OPNIEUW_VERSTUURD(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_HA_ONBEKEND(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_KLANTNUMMER_NIET_GEVERIFIEERD(CervixHuisartsberichtDetailsPanel.class),

	BMHK_HUISARTSBERICHT_VERSTUURD_EXTRA_HUISARTS(CervixExtraHuisartsberichtDetailsPanel.class),

	BMHK_ORDER_VERSTUURD,

	BMHK_ZAS_AANGEMAAKT(BriefKlaargezetPanel.class),

	BMHK_ZAS_AANGEVRAAGD(BriefKlaargezetPanel.class),

	BMHK_ZAS_KLAARGEZET(BriefKlaargezetPanel.class),

	BMHK_ZAS_SAMENGESTELD,

	BMHK_ZAS_GEANNULEERD,

	BMHK_HERZIENING_CYTOLOGISCHE_BEOORDELING,

	BMHK_OMISSIE_WACHT_OP_UITSTRIJKJE_ONTVANGEN,

	BMHK_OMISSIE_WACHT_OP_HPV_UITSLAG,

	BMHK_OMISSIE_WACHT_OP_GECONTROLEERD,

	BMHK_OMISSIE_WACHT_OP_GECONTROLEERD_VOOR_CYTOLOGIE,

	BMHK_OMISSIE_WACHT_OP_CYTOLOGIE_UITSLAG,

	MAMMA_DATUM_TIJD_AFSPRAAK,

	MAMMA_AFSPRAAK,

	MAMMA_AFSPRAAK_FORCEREN,

	MAMMA_ONDERZOEK_AFGEROND(MammaOnderzoekInzienPanel.class),

	MAMMA_ONDERZOEK_ONDERBROKEN(MammaOnderzoekInzienPanel.class),

	MAMMA_ONDERZOEK_ONDERBROKEN_GEEN_VERVOLG(MammaOnderzoekInzienPanel.class),

	MAMMA_ONDERZOEK_VAN_ONDERBROKEN_NAAR_ONVOLLEDIG(MammaOnderzoekInzienPanel.class),

	MAMMA_ONDERZOEK_ONVOLLEDIG(MammaOnderzoekInzienPanel.class),

	MAMMA_EERSTE_BEOORDELING_AFGEROND(MammaLezingInzienPanel.class),

	MAMMA_TWEEDE_BEOORDELING_AFGEROND(MammaLezingInzienPanel.class),

	MAMMA_DISCREPANTIE_AFGEROND(MammaLezingInzienPanel.class),

	MAMMA_DISCREPANTIE_AUTOMATISCH_AFGEROND,

	MAMMA_ARBITRAGE_AFGEROND(MammaLezingInzienPanel.class),

	MAMMA_HUISARTSBERICHT_AANGEMAAKT(MammaHuisartsberichtDetailsPanel.class),
	MAMMA_HUISARTSBERICHT_VERSTUURD(MammaHuisartsberichtDetailsPanel.class),

	MAMMA_HUISARTSBERICHT_VERSTUREN_MISLUKT(MammaHuisartsberichtDetailsPanel.class),

	MAMMA_HUISARTSBERICHT_OPNIEUW_VERSTUURD(MammaHuisartsberichtDetailsPanel.class),

	MAMMA_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT(MammaHuisartsberichtDetailsPanel.class),

	MAMMA_VERWIJSVERSLAG_AFGEKEURD,

	MAMMA_UITSLAG_ONGUNSTIG(MammaVerslagInzienPanel.class),

	MAMMA_VERWIJSVERSLAG_GEREED,

	MAMMA_HUISARTS_TOEGEVOEGD(MammaHuisartsInzienPanel.class),

	MAMMA_FOLLOW_UP_RADIOLOGIE_VERSLAG(MammaFollowUpRadiologieGebeurtenisDetailPanel.class),

	MAMMA_FOLLOW_UP_PATHOLOGIE_VERSLAG(),

	MAMMA_FOLLOW_UP_CONCLUSIE,

	MAMMA_BEOORDELING_GEANNULEERD(MammaHerbeoordelingGebeurtenisPanel.class),

	MAMMA_BEOORDELING_OPGESCHORT,

	MAMMA_UPLOAD_BEELDEN_VERZOEK(MammaUploadBeeldenVerzoekGebeurtenisDetailPanel.class);

	private final Class<? extends AbstractGebeurtenisDetailPanel> detailPanelClass;

	TypeGebeurtenis()
	{
		this(null);
	}

	TypeGebeurtenis(Class<? extends AbstractGebeurtenisDetailPanel> detailPanelClass)
	{
		this.detailPanelClass = detailPanelClass;
	}

	public Class<? extends AbstractGebeurtenisDetailPanel> getDetailPanelClass()
	{
		return detailPanelClass;
	}
}
