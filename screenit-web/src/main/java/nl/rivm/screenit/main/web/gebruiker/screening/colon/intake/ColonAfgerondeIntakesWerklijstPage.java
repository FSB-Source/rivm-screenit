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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;

public class ColonAfgerondeIntakesWerklijstPage extends WerklijstIntakePage
{

	public ColonAfgerondeIntakesWerklijstPage()
	{
		super(ColonAfspraakStatus.UITGEVOERD);
		exportToCsvLink.setVisible(false);
	}

	@Override
	protected List<ConclusieTypeFilter> getFilterOpties()
	{
		return Arrays.asList(ConclusieTypeFilter.getAfgerondeAfspraken());
	}

	@Override
	protected SortableDataProvider<ColonIntakeAfspraak, String> getWerklijstIntakeDataProvider(ColonIntakelocatie intakelocatie, int aantalPerPagina)
	{
		var werklijstIntakeDataProvider = super.getWerklijstIntakeDataProvider(intakelocatie, aantalPerPagina);
		werklijstIntakeDataProvider.setSort("vanaf", SortOrder.DESCENDING);
		return werklijstIntakeDataProvider;
	}

	@Override
	protected List<IColumn<ColonIntakeAfspraak, String>> getColumns()
	{
		var columns = super.getColumns();
		addStatusColumn(columns);
		addVerwezenColumn(columns);
		return columns;
	}
}
