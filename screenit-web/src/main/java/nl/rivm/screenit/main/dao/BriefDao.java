package nl.rivm.screenit.main.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BriefType;

public interface BriefDao
{
	<MB extends MergedBrieven<?>> List<MB> getMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter, long first, long count, String sortProperty,
		boolean ascending);

	<MB extends MergedBrieven<?>> Long countMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter);

	List<? extends ClientBrief> getBrievenVanAfmelding(Afmelding afmelding, BriefType... types);

	List<BezwaarBrief> getBrievenVanBezwaar(BezwaarMoment moment);
}
