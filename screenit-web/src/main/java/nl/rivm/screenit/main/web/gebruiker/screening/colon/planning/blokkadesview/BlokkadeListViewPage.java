
package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.blokkadesview;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.PlanningBasePage;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BlokkadeListViewPage extends PlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private ScreenitDataTable<ColonBlokkade, String> table;

	@SpringBean
	private RoosterService roosterService;

	@SpringBean
	private HibernateService hibernateService;

	public BlokkadeListViewPage()
	{
		super();

		ColoscopieCentrum intakelocatie = ScreenitSession.get().getColoscopieCentrum();
		add(new Label("coloscopiecentrum", intakelocatie.getNaam()));

		RoosterListViewFilter filter = new RoosterListViewFilter();
		filter.setStartDatum(new Date());
		filter.setEndDatum(new Date());
		filter.setStatus(null);

		final IModel<RoosterListViewFilter> zoekModel = new Model<RoosterListViewFilter>(filter);

		setDefaultModel(zoekModel);
		maakTabel(intakelocatie, zoekModel);

		Form<RoosterListViewFilter> form = new Form<>("form", new CompoundPropertyModel<>(zoekModel));
		add(form);

		FormComponent<Date> startDatum = ComponentHelper.addTextField(form, "startDatum", false, 10, Date.class, false);
		startDatum.setType(Date.class);

		FormComponent<Date> eindDatum = ComponentHelper.addTextField(form, "endDatum", false, 10, Date.class, false);
		eindDatum.setType(Date.class);

		startDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				String melding = updateTabel(target, startDatum.getModelObject(), eindDatum.getModelObject());

				if (StringUtils.isNotBlank(melding))
				{
					startDatum.error(getString(melding));
				}
			}

		});

		eindDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				String melding = updateTabel(target, startDatum.getModelObject(), eindDatum.getModelObject());

				if (StringUtils.isNotBlank(melding))
				{
					eindDatum.error(getString(melding));
				}
			}

		});

		add(new ExportToXslLink<>("csv", "Blokkade(s)", table));

	}

	private void maakTabel(ColoscopieCentrum intakelocatie, final IModel<RoosterListViewFilter> zoekModel)
	{
		List<IColumn<ColonBlokkade, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<ColonBlokkade, String>(Model.of("Datum/tijd"), "startTime", "startTime", new SimpleDateFormat("dd-MM-yyyy HH:mm"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<ColonBlokkade> embeddedModel)
			{
				IModel<?> labelModel = super.getDataModel(embeddedModel);

				String label = labelModel.getObject().toString();
				label += " - " + DateUtil.formatTime(embeddedModel.getObject().getEndTime());
				return new Model(label);
			}

		});
		columns.add(new PropertyColumn<ColonBlokkade, String>(Model.of("Kamer"), "kamer.name", "location.name"));
		columns.add(new PropertyColumn<ColonBlokkade, String>(Model.of("Omschrijving"), "description", "description"));

		table = new ScreenitDataTable<ColonBlokkade, String>("tabel", columns, new BlokkadeListViewDataProvider(zoekModel, intakelocatie), 10,
			Model.of("blokkade(s)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ColonBlokkade> model)
			{

			}

			@Override
			protected boolean isRowClickable(IModel<ColonBlokkade> model)
			{
				return false;
			}
		};
		add(table);
	}

	private String updateTabel(AjaxRequestTarget target, Date startDatum, Date eindDatum)
	{
		if (startDatum == null || eindDatum == null)
		{
			return "start.eind.leeg";
		}
		if (startDatum.after(eindDatum))
		{
			return "start.voor.eind";
		}
		target.add(table);
		return "";
	}
}
