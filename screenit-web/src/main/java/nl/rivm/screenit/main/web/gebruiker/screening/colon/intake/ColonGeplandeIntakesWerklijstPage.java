package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;

public class ColonGeplandeIntakesWerklijstPage extends WerklijstIntakePage
{

	public ColonGeplandeIntakesWerklijstPage()
	{
		super(AfspraakStatus.GEPLAND);
	}

	@Override
	protected List<ConclusieTypeFilter> getFilterOpties()
	{
		return new ArrayList<>();
	}

	@Override
	protected List<IColumn<ColonIntakeAfspraak, String>> getColumns()
	{
		var columns = super.getColumns();
		addDatumBriefAfspraakColumn(columns);
		addHuisartsColumn(columns);
		addVerwezenColumn(columns);
		return columns;
	}
}
