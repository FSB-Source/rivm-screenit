package nl.rivm.screenit.main.web.gebruiker.clienten;

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

import java.util.ArrayList;
import java.util.Arrays;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactAanvragenNieuweZASPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactAfmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactFrisseStartPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactHeraanmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactHerdrukPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix.CervixClientContactUitstelPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientContactAanvragenNieuweIFOBTPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientContactAfmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientContactAfspraakWijzigenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientContactHeraanmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonClientContactNieuweAfspraakAanmakenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.ColonHuisartsWijzigenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen.ClientContactBezwaarPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen.ClientContactClientGegevensAanvragenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen.ClientContactInzagePersoonsgegevensPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen.ClientContactTijdelijkAdresPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactAfmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactAfspraakMakenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactAfspraakWijzigenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactDoelgroepVastleggenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactHeraanmeldenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactHerbeoordelenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactRondeForcerenPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactVerzoekOmContactPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaClientContactWilGeenVervolgOnderzoekPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.MammaHuisartsWijzigenPanel;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Recht;

public enum ClientContactActieTypeWrapper
{

	GEEN(ClientContactActieType.GEEN, null, null, null)
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return getFullExclusieArrayWithoutSelf(ClientContactActieTypeWrapper.GEEN);
		}
	},

	OPNIEUW_AANVRAGEN_CLIENTGEGEVENS(
		ClientContactActieType.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS,
		ClientContactClientGegevensAanvragenPanel.class,
		Recht.GEBRUIKER_GBA_AANVRAGEN,
		"icon-refresh")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_AFMELDEN,
				ClientContactActieTypeWrapper.COLON_HERAANMELDEN };

		}
	},

	TIJDELIJK_ADRES(ClientContactActieType.TIJDELIJK_ADRES, ClientContactTijdelijkAdresPanel.class, Recht.GEBRUIKER_CLIENT_GEGEVENS, "icon-home")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	COLON_AFMELDEN(ClientContactActieType.COLON_AFMELDEN, ColonClientContactAfmeldenPanel.class, Recht.GEBRUIKER_CLIENT_COLON_AFMELDEN, "icon-ban-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_HERAANMELDEN,
				ClientContactActieTypeWrapper.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieTypeWrapper.COLON_AANVRAGEN_NIEUWE_IFOBT,
				ClientContactActieTypeWrapper.COLON_NIEUWE_AFSPRAAK_AANMAKEN, ClientContactActieTypeWrapper.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS };
		}
	},

	COLON_HERAANMELDEN(ClientContactActieType.COLON_HERAANMELDEN, ColonClientContactHeraanmeldenPanel.class, Recht.GEBRUIKER_CLIENT_COLON_HERAANMELDEN, "icon-ok-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_AFMELDEN,
				ClientContactActieTypeWrapper.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieTypeWrapper.COLON_AANVRAGEN_NIEUWE_IFOBT,
				ClientContactActieTypeWrapper.COLON_NIEUWE_AFSPRAAK_AANMAKEN, ClientContactActieTypeWrapper.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS };
		}
	},

	COLON_AANVRAGEN_NIEUWE_IFOBT(
		ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT,
		ColonClientContactAanvragenNieuweIFOBTPanel.class,
		Recht.GEBRUIKER_CLIENT_SR_NIEUWE_IFOBT_AANVRAGEN,
		"icon-repeat")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_AFMELDEN,
				ClientContactActieTypeWrapper.COLON_HERAANMELDEN };
		}
	},

	COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN(
		ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN,
		ColonClientContactAfspraakWijzigenPanel.class,
		Recht.GEBRUIKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_AFMELDEN,
				ClientContactActieTypeWrapper.COLON_HERAANMELDEN };
		}
	},

	COLON_NIEUWE_AFSPRAAK_AANMAKEN(
		ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN,
		ColonClientContactNieuweAfspraakAanmakenPanel.class,
		Recht.GEBRUIKER_CLIENT_SR_NIEUWE_INTAKEAFSPRAAKGEMAAKT,
		"icon-nieuwe-afspraak")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.COLON_AFMELDEN,
				ClientContactActieTypeWrapper.COLON_HERAANMELDEN };
		}
	},

	COLON_HUISARTS_WIJZIGEN(ClientContactActieType.COLON_HUISARTS_WIJZIGEN, ColonHuisartsWijzigenPanel.class, Recht.GEBRUIKER_WIJZIGEN_HUISARTS, "icon-edit")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN(
		ClientContactActieType.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN,
		null,
		Recht.CLIENT_AANVRAAG_VERWIJDEREN_UITSLAG_BRIEF,
		"icon-repeat")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	BEZWAAR(ClientContactActieType.BEZWAAR, ClientContactBezwaarPanel.class, Recht.GEBRUIKER_CLIENT_BEZWAAR, "icon-ok-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	INZAGE_PERSOONSGEGEVENS(
		ClientContactActieType.INZAGE_PERSOONSGEGEVENS,
		ClientContactInzagePersoonsgegevensPanel.class,
		Recht.GEBRUIKER_AANVRAAG_OVERDRACHT_PERSOONSGEGEVENS,
		"icon-refresh")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	CERVIX_DEELNAME_BUITEN_BVO_BMHK(ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK, null, Recht.GEBRUIKER_CLIENT_CERVIX_AANMELDEN_DEELNAME_BUITEN_BVO_BMHK, null)
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return getFullExclusieArrayWithoutSelf(ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK);
		}
	},

	CERVIX_AFMELDEN(ClientContactActieType.CERVIX_AFMELDEN, CervixClientContactAfmeldenPanel.class, Recht.GEBRUIKER_CLIENT_CERVIX_AFMELDEN, "icon-ban-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.CERVIX_HERAANMELDEN,
				ClientContactActieTypeWrapper.CERVIX_UITSTEL, ClientContactActieTypeWrapper.CERVIX_ZAS_AANVRAGEN, ClientContactActieTypeWrapper.CERVIX_FRISSE_START,
				ClientContactActieTypeWrapper.CERVIX_HERDRUK };
		}
	},

	CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN(
		ClientContactActieType.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN,
		null,
		Recht.CLIENT_AANVRAAG_VERWIJDEREN_UITSLAG_BRIEF,
		"icon-repeat")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	CERVIX_HERAANMELDEN(ClientContactActieType.CERVIX_HERAANMELDEN, CervixClientContactHeraanmeldenPanel.class, Recht.GEBRUIKER_CLIENT_CERVIX_HERAANMELDEN, "icon-ok-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.CERVIX_AFMELDEN };
		}
	},

	CERVIX_UITSTEL(ClientContactActieType.CERVIX_UITSTEL, CervixClientContactUitstelPanel.class, Recht.GEBRUIKER_CLIENT_UITSTEL, "icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK, CERVIX_AFMELDEN,
				CERVIX_ZAS_AANVRAGEN };
		}
	},

	CERVIX_ZAS_AANVRAGEN(ClientContactActieType.CERVIX_ZAS_AANVRAGEN, CervixClientContactAanvragenNieuweZASPanel.class, Recht.GEBRUIKER_CERVIX_CLIENT_ZAS_AANVRAGEN, "icon-refresh")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.CERVIX_AFMELDEN,
				ClientContactActieTypeWrapper.CERVIX_UITSTEL };
		}
	},

	CERVIX_HERDRUK(ClientContactActieType.CERVIX_HERDRUK, CervixClientContactHerdrukPanel.class, Recht.GEBRUIKER_CERVIX_CLIENT_HERDRUK, "icon-repeat")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.CERVIX_AFMELDEN,
				ClientContactActieTypeWrapper.CERVIX_FRISSE_START };
		}
	},

	CERVIX_FRISSE_START(ClientContactActieType.CERVIX_FRISSE_START, CervixClientContactFrisseStartPanel.class, Recht.GEBRUIKER_CERVIX_CLIENT_HERDRUK, "icon-repeat")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.CERVIX_AFMELDEN,
				ClientContactActieTypeWrapper.CERVIX_HERDRUK };
		}
	},

	MAMMA_AFMELDEN(ClientContactActieType.MAMMA_AFMELDEN, MammaClientContactAfmeldenPanel.class, Recht.GEBRUIKER_CLIENT_MAMMA_AFMELDEN, "icon-ban-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_HERAANMELDEN(ClientContactActieType.MAMMA_HERAANMELDEN, MammaClientContactHeraanmeldenPanel.class, Recht.GEBRUIKER_CLIENT_MAMMA_AFMELDEN, "icon-ok-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_RONDE_FORCEREN(ClientContactActieType.MAMMA_RONDE_FORCEREN, MammaClientContactRondeForcerenPanel.class, Recht.GEBRUIKER_CLIENT_MAMMA_RONDE_FORCEREN, "icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS, ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN,
				ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS(
		ClientContactActieType.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
		null,
		Recht.GEBRUIKER_CLIENT_MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN, ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_INFOBRIEF_PROTHESEN(
		ClientContactActieType.MAMMA_INFOBRIEF_PROTHESEN,
		null,
		Recht.GEBRUIKER_CLIENT_MAMMA_INFOBRIEF_PROTHESEN,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	MAMMA_AFSPRAAK_MAKEN(ClientContactActieType.MAMMA_AFSPRAAK_MAKEN, MammaClientContactAfspraakMakenPanel.class, Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_MAKEN, "icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS, ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN,
				MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG, MAMMA_VERZOEK_CLIENT_CONTACT };
		}
	},

	MAMMA_AFSPRAAK_WIJZIGEN(
		ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN,
		MammaClientContactAfspraakWijzigenPanel.class,
		Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN, ClientContactActieTypeWrapper.MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG,
				ClientContactActieTypeWrapper.MAMMA_VERZOEK_CLIENT_CONTACT, ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
				ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_DOELGROEP_WIJZIGEN(
		ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN,
		MammaClientContactDoelgroepVastleggenPanel.class,
		Recht.GEBRUIKER_CLIENT_MAMMA_DOELGROEP_WIJZIGEN,
		"icon-ok-circle")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN, ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_HUISARTS_WIJZIGEN(ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN, MammaHuisartsWijzigenPanel.class, Recht.GEBRUIKER_WIJZIGEN_HUISARTS, "icon-edit")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK };
		}
	},

	MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG(
		ClientContactActieType.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK,
		MammaClientContactWilGeenVervolgOnderzoekPanel.class,
		Recht.GEBRUIKER_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG,
		"icon-edit")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN, ClientContactActieTypeWrapper.MAMMA_VERZOEK_CLIENT_CONTACT, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_VERZOEK_CLIENT_CONTACT(
		ClientContactActieType.MAMMA_VERZOEK_CLIENT_CONTACT,
		MammaClientContactVerzoekOmContactPanel.class,
		Recht.GEBRUIKER_CLIENT_OPROEP_NA_ONDERBROKEN_ONDERZOEK,
		"icon-edit")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN, ClientContactActieTypeWrapper.MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG,
				ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN };
		}
	},

	MAMMA_AFSPRAAK_MAKEN_FORCEREN(
		ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
		MammaClientContactAfspraakMakenPanel.class,
		Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_MAKEN_FORCEREN,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN, ClientContactActieTypeWrapper.MAMMA_HERBEOORDELEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN, ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
				ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN, MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG, MAMMA_VERZOEK_CLIENT_CONTACT };
		}
	},

	MAMMA_HERBEOORDELEN(
		ClientContactActieType.MAMMA_HERBEOORDELEN,
		MammaClientContactHerbeoordelenPanel.class,
		Recht.GEBRUIKER_CLIENT_MAMMA_HERBEOORDELEN,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN,
				ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN, ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS,
				ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN, MAMMA_CLIENT_WIL_GEEN_ONDERZOEK_VERVOLG, MAMMA_VERZOEK_CLIENT_CONTACT };
		}
	},

	MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS(
		ClientContactActieType.MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS,
		null,
		Recht.GEBRUIKER_CLIENT_MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS_TERUGDRAAIEN,
		"icon-calendar")
	{
		@Override
		public ClientContactActieTypeWrapper[] getExclusie()
		{
			return new ClientContactActieTypeWrapper[] { ClientContactActieTypeWrapper.GEEN, ClientContactActieTypeWrapper.CERVIX_DEELNAME_BUITEN_BVO_BMHK,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFMELDEN, ClientContactActieTypeWrapper.MAMMA_HERAANMELDEN, ClientContactActieTypeWrapper.MAMMA_RONDE_FORCEREN,
				ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN, ClientContactActieTypeWrapper.MAMMA_DOELGROEP_WIJZIGEN };
		}
	},
	;

	private static ClientContactActieTypeWrapper[] getFullExclusieArrayWithoutSelf(ClientContactActieTypeWrapper self)
	{
		final ArrayList<ClientContactActieTypeWrapper> clientContactActieTypeWrappers = new ArrayList<>(Arrays.asList(ClientContactActieTypeWrapper.values()));
		clientContactActieTypeWrappers.remove(self);
		return clientContactActieTypeWrappers.toArray(new ClientContactActieTypeWrapper[] {});
	}

	private final ClientContactActieType type;

	private final Class<? extends AbstractClientContactActiePanel<?>> panelClass;

	private final Recht recht;

	private final String snelkeuzeCssClass;

	ClientContactActieTypeWrapper(ClientContactActieType type, Class<? extends AbstractClientContactActiePanel<?>> panelClass, Recht recht, String snelkeuzeCssClass)
	{
		this.type = type;
		this.panelClass = panelClass;
		this.recht = recht;
		this.snelkeuzeCssClass = snelkeuzeCssClass;
	}

	public ClientContactActieType getType()
	{
		return type;
	}

	public Class<? extends AbstractClientContactActiePanel<?>> getPanelClass()
	{
		return panelClass;
	}

	public Recht getRecht()
	{
		return recht;
	}

	public String getSnelkeuzeCssClass()
	{
		return snelkeuzeCssClass;
	}

	public abstract ClientContactActieTypeWrapper[] getExclusie();
}
