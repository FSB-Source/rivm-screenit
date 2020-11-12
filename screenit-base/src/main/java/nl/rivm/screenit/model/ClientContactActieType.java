package nl.rivm.screenit.model;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public enum ClientContactActieType
{
	GEEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	OPNIEUW_AANVRAGEN_CLIENTGEGEVENS(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	TIJDELIJK_ADRES(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	CERVIX_DEELNAME_BUITEN_BVO_BMHK(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	COLON_AANVRAGEN_NIEUWE_IFOBT(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_NIEUWE_AFSPRAAK_AANMAKEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_AFMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_HERAANMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_HUISARTS_WIJZIGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	COLON_OPEN_UITNODIGING(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON }),

	BEZWAAR(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	INZAGE_PERSOONSGEGEVENS(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA }),

	CERVIX_AFMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_HERAANMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_UITSTEL(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_ZAS_AANVRAGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_HERDRUK(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_FRISSE_START(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.CERVIX }),

	MAMMA_RONDE_FORCEREN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_AFSPRAAK_MAKEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_AFSPRAAK_MAKEN_FORCEREN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_AFSPRAAK_WIJZIGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_AFMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_HERAANMELDEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_DOELGROEP_WIJZIGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_HUISARTS_WIJZIGEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_MINDER_VALIDE_NIET_MEER_ZIEKENHUIS(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_VERZOEK_CLIENT_CONTACT(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),

	MAMMA_INFOBRIEF_PROTHESEN(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.MAMMA }),
	;

	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	ClientContactActieType(Bevolkingsonderzoek[] bevolkingsonderzoeken)
	{
		this.bevolkingsonderzoeken = Arrays.asList(bevolkingsonderzoeken);
	}

	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}
}
