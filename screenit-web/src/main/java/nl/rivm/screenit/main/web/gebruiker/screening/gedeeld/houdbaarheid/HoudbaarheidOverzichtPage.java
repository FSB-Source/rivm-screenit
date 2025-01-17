
package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.lang.reflect.ParameterizedType;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.rivm.screenit.model.AbstractHoudbaarheid_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class HoudbaarheidOverzichtPage<H extends AbstractHoudbaarheid> extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	public HoudbaarheidOverzichtPage()
	{
		List<IColumn<H, String>> columns = createColumns();

		add(new ScreenitDataTable<H, String>("data", columns,
			new HoudbaarheidDataProvider<H>((Class<H>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0]), 10, new Model<>("actuele batches"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<H> model)
			{
				setResponsePage(createEditPage(model));
			}

			@Override
			protected boolean isRowClickable(IModel<H> model)
			{
				return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_IFOBT_BATCH, Actie.AANPASSEN) ||
					ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_ZAS_BATCH, Actie.AANPASSEN);
			}
		});

		add(new Link<Void>("nieuweBatch")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(createEditPage(null));
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_IFOBT_BATCH, Actie.TOEVOEGEN) ||
					ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_ZAS_BATCH, Actie.TOEVOEGEN));
			}

		});
	}

	protected List<IColumn<H, String>> createColumns()
	{
		List<IColumn<H, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<H, String>(new Model<String>("Begin id"), AbstractHoudbaarheid_.BARCODE_START, AbstractHoudbaarheid_.BARCODE_START));
		columns.add(new PropertyColumn<H, String>(new Model<String>("Eind id"), AbstractHoudbaarheid_.BARCODE_END, AbstractHoudbaarheid_.BARCODE_END));
		columns.add(new DateTimePropertyColumn<H, String>(new Model<String>("Einddatum houdbaarheid"), AbstractHoudbaarheid_.VERVAL_DATUM, AbstractHoudbaarheid_.VERVAL_DATUM,
			new SimpleDateFormat("dd-MM-yyyy")));
		return columns;
	}

	protected abstract HoudbaarheidEditPage<H> createEditPage(IModel<H> model);

}
