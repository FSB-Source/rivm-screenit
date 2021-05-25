
package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.listview;

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
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.PlanningBasePage;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.input.simplechoice.SimpleChoiceRenderer;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Classes;
import org.joda.time.DateTime;
import org.joda.time.Interval;

public class RoosterListViewPage extends PlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private ScreenitDataTable<RoosterItemListViewWrapper, String> table;

	@SpringBean
	private RoosterService roosterService;

	@SpringBean
	private HibernateService hibernateService;

	public RoosterListViewPage()
	{
		super();

		ColoscopieCentrum coloscopieCentrum = ScreenitSession.get().getColoscopieCentrum();
		add(new Label("coloscopiecentrum", coloscopieCentrum.getNaam()));

		RoosterListViewFilter filter = new RoosterListViewFilter();
		filter.setStartDatum(new Date());
		filter.setEndDatum(new Date());
		filter.setStatus(null);

		final IModel<RoosterListViewFilter> zoekModel = new Model<RoosterListViewFilter>(filter);

		final Label totaalBlokken = new Label("totaalBlokken", 0)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				RoosterListViewFilter zoekObject = zoekModel.getObject();
				Date startDatum = zoekObject.getStartDatum();
				Date endDatum = zoekObject.getEndDatum();
				if (startDatum.after(endDatum))
				{

					startDatum = new DateTime(endDatum).withTimeAtStartOfDay().plusDays(1).toDate();
				}
				Interval periode = new Interval(new DateTime(startDatum), new DateTime(endDatum).withTimeAtStartOfDay().plusDays(1));
				setDefaultModelObject(roosterService.getCurrentAantalRoosterBlokken(ScreenitSession.get().getColoscopieCentrum(), periode));
			}

		};
		totaalBlokken.setOutputMarkupId(true);
		add(totaalBlokken);

		setDefaultModel(zoekModel);
		List<IColumn<RoosterItemListViewWrapper, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<RoosterItemListViewWrapper, String>(Model.of("Datum/tijd"), "startTime", "startTime", new SimpleDateFormat("dd-MM-yyyy HH:mm"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<RoosterItemListViewWrapper> embeddedModel)
			{
				IModel<?> labelModel = super.getDataModel(embeddedModel);

				String label = labelModel.getObject().toString();
				label += " - " + new SimpleDateFormat("HH:mm").format(embeddedModel.getObject().getEndTime());
				return new Model(label);
			}

		});
		columns.add(new PropertyColumn<RoosterItemListViewWrapper, String>(Model.of("Kamer"), "kamer", "kamer"));
		columns.add(new AbstractColumn<RoosterItemListViewWrapper, String>(Model.of("Status"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<RoosterItemListViewWrapper>> cellItem, String componentId, IModel<RoosterItemListViewWrapper> rowModel)
			{
				RoosterItemListViewWrapper wrapper = rowModel.getObject();

				RoosterItem roosterItem = hibernateService.load(RoosterItem.class, wrapper.getRoosterItemId());
				cellItem.add(new EnumLabel<RoosterItemStatus>(componentId, roosterService.getRoosterItemStatus(roosterItem)));
			}

		});

		table = new ScreenitDataTable<RoosterItemListViewWrapper, String>("tabel", columns, new RoosterListViewDataProvider(zoekModel, coloscopieCentrum), 10,
			Model.of("roosterblokken"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<RoosterItemListViewWrapper> model)
			{

			}

			@Override
			protected boolean isRowClickable(IModel<RoosterItemListViewWrapper> model)
			{
				return false;
			}
		};
		add(table);

		Form<RoosterListViewFilter> form = new Form<>("form", new CompoundPropertyModel<>(zoekModel));
		add(form);

		form.add(new AjaxButtonGroup<RoosterItemStatus>("status", new ListModel<RoosterItemStatus>(Arrays.asList(new RoosterItemStatus[] { null, RoosterItemStatus.BLOKKADE,
			RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT, RoosterItemStatus.INTAKE_GEPLAND, RoosterItemStatus.VRIJ_TE_VERPLAATSEN })), new SimpleChoiceRenderer<RoosterItemStatus>()
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Object getDisplayValue(RoosterItemStatus object)
				{
					if (object == null)
					{
						return getString(Classes.simpleName(RoosterItemStatus.class) + ".null");
					}
					else
					{
						return getString(EnumStringUtil.getPropertyString(object));
					}
				}

			})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSelectionChanged(RoosterItemStatus selection, AjaxRequestTarget target, String markupId)
			{
				target.add(table);
				target.add(totaalBlokken);
			}
		});

		FormComponent<Date> startDatum = ComponentHelper.addTextField(form, "startDatum", true, 10, Date.class, false);
		startDatum.setType(Date.class);
		startDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(table);
				target.add(totaalBlokken);
			}

		});

		FormComponent<Date> endDatum = ComponentHelper.addTextField(form, "endDatum", true, 10, Date.class, false);
		endDatum.setType(Date.class);
		endDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(totaalBlokken);
				target.add(table);
			}

		});

		add(new ExportToXslLink<>("csv", "Roosterblokken", table));

	}
}
