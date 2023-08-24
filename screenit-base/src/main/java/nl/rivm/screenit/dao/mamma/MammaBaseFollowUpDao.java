package nl.rivm.screenit.dao.mamma;

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

import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingRadiologieDto;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;

public interface MammaBaseFollowUpDao
{
	List<MammaFollowUpRadiologieVerslag> zoekRadiologieVerslagen(Instelling instelling, MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie, int first, int count,
		SortState<String> sortState);

	long countRadiologieVerslagen(Instelling instelling, MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie);

	List<MammaFollowUpInstellingRadiologieDto> zoekOpenstaandeRadiologieVerslagenPerOrganisatie(ScreeningOrganisatie regio,
		MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie, Integer jaar);

	List<MammaFollowUpInstellingDto> zoekInstellingenMetOpenstaandePaVerslagen(ScreeningOrganisatie regio);

	List<MammaFollowUpRadiologieVerslag> zoekDossiersMetOpenstaandePaVerslagen(Instelling instelling, int first, int count, SortState<String> sortState);

	long countDossiersMetOpenstaandePaVerslagen(Instelling instelling);

	List<MammaBeoordeling> zoekOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio, int first, int count, SortState<String> sortState);

	long countOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio);

	boolean heeftOpenstaandeFollowUpConclusie(MammaDossier dossier);
}
