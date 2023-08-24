package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

@AllArgsConstructor
@Getter
public enum OrganisatieType
{

	RADIOLOGIEAFDELING("Afdeling radiologie", Recht.GEBRUIKER_MAMMA_RADIOLOGIEAFDELING_ORG_BEHEER),

	BMHK_LABORATORIUM("BMHK laboratorium", Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER),

	BEOORDELINGSEENHEID("Beoordelingseenheid", Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER),

	CENTRALE_EENHEID("Centrale eenheid", Recht.GEBRUIKER_CENTRALE_EENHEID_ORG_BEHEER),

	COLOSCOPIELOCATIE("Coloscopielocatie", Recht.GEBRUIKER_COLOSCOPIELOCATIE_ORG_BEHEER),

	LABORATORIUM("FIT laboratorium", Recht.GEBRUIKER_LABORATORIA_BEHEER),

	HUISARTS("Huisarts", Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER),

	INPAKCENTRUM("Inpakcentrum", Recht.GEBRUIKER_INPAKCENTRUM_ORG_BEHEER),

	COLOSCOPIECENTRUM("Intakelocatie", Recht.GEBRUIKER_COLOSCOPIECENTRUM_ORG_BEHEER),

	RIVM("Landelijk beheer", Recht.GEBRUIKER_RIVM_BEHEER),

	MAMMAPOLI("Mammapoli", Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER),

	PA_LABORATORIUM("PA laboratorium", Recht.GEBRUIKER_PA_LABORATORIA_BEHEER),

	SCREENINGSORGANISATIE("Screeningsorganisatie", Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER),

	ZORGINSTELLING("Zorginstelling", Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER),

	ZORGVERZEKERAAR("Zorgverzekeraar", Recht.GEBRUIKER_ZORGVERZEKERAARS_BEHEER),

	KWALITEITSPLATFORM("Kwaliteitsplatform", Recht.GEBRUIKER_KWALITEITSPLATFORM_BEHEER),
	;

	private final String naam;

	private final Recht recht;

	@Override
	public String toString()
	{
		return this.naam;
	}

	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return this.recht.getBevolkingsonderzoeken();
	}
}
