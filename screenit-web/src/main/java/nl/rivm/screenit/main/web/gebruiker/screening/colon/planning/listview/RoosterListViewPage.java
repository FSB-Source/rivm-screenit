package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.listview;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.table.ExportToXslLink;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDateTimePropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.PlanningBasePage;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.input.simplechoice.SimpleChoiceRenderer;

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

import com.google.common.collect.Range;

public class RoosterListViewPage extends PlanningBasePage
{
	@SpringBean
	private ColonAfspraakslotService afspraakslotService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final ScreenitDataTable<ColonAfspraakslotListViewWrapper, String> table;

	public RoosterListViewPage()
	{
		super();

		var intakelocatie = ScreenitSession.get().getIntakelocatie();
		add(new Label("intakelocatie", intakelocatie.getNaam()));

		var filter = new RoosterListViewFilter();
		filter.setStartDatum(currentDateSupplier.getDate());
		filter.setEindDatum(currentDateSupplier.getDate());
		filter.setStatus(null);

		final IModel<RoosterListViewFilter> zoekModel = new Model<>(filter);

		final Label totaalBlokken = new Label("totaalBlokken", 0)
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				RoosterListViewFilter zoekObject = zoekModel.getObject();
				var startDatum = DateUtil.toLocalDate(zoekObject.getStartDatum());
				var eindDatum = DateUtil.toLocalDate(zoekObject.getEindDatum());
				if (startDatum.isAfter(eindDatum))
				{

					startDatum = eindDatum.plusDays(1);
				}
				var periode = Range.closed(startDatum.atStartOfDay(), eindDatum.plusDays(1).atStartOfDay());
				setDefaultModelObject(afspraakslotService.getCurrentAantalAfspraakslots(ScreenitSession.get().getIntakelocatie(), periode));
			}

		};
		totaalBlokken.setOutputMarkupId(true);
		add(totaalBlokken);

		setDefaultModel(zoekModel);
		List<IColumn<ColonAfspraakslotListViewWrapper, String>> columns = new ArrayList<>();
		columns.add(new ScreenitDateTimePropertyColumn<>(Model.of("Datum/tijd"), "startDatum", "vanaf")
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonAfspraakslotListViewWrapper> embeddedModel)
			{
				IModel<?> labelModel = super.getDataModel(embeddedModel);

				String label = labelModel.getObject().toString();
				label += " - " + new SimpleDateFormat("HH:mm").format(embeddedModel.getObject().getEindDatum());
				return new Model(label);
			}

		});
		columns.add(new PropertyColumn<>(Model.of("Kamer"), "kamer", "kamer"));
		columns.add(new AbstractColumn<>(Model.of("Status"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<ColonAfspraakslotListViewWrapper>> cellItem, String componentId, IModel<ColonAfspraakslotListViewWrapper> rowModel)
			{
				ColonAfspraakslotListViewWrapper wrapper = rowModel.getObject();

				var afspraakslot = hibernateService.load(ColonAfspraakslot.class, wrapper.getAfspraakslotId());
				cellItem.add(new EnumLabel<>(componentId, afspraakslotService.getAfspraakslotStatus(afspraakslot)));
			}

		});

		table = new ScreenitDataTable<>("tabel", columns, new RoosterListViewDataProvider(zoekModel, intakelocatie), 10,
			Model.of("roosterblokken"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<ColonAfspraakslotListViewWrapper> model)
			{

			}

			@Override
			protected boolean isRowClickable(IModel<ColonAfspraakslotListViewWrapper> model)
			{
				return false;
			}
		};
		add(table);

		Form<RoosterListViewFilter> form = new Form<>("form", new CompoundPropertyModel<>(zoekModel));
		add(form);

		form.add(new AjaxButtonGroup<>("status", new ListModel<>(Arrays.asList(null, ColonAfspraakslotStatus.BLOKKADE,
			ColonAfspraakslotStatus.GEBRUIKT_VOOR_CAPACITEIT, ColonAfspraakslotStatus.INTAKE_GEPLAND, ColonAfspraakslotStatus.VRIJ_TE_VERPLAATSEN)), new SimpleChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(ColonAfspraakslotStatus object)
			{
				if (object == null)
				{
					return getString(Classes.simpleName(ColonAfspraakslotStatus.class) + ".null");
				}
				else
				{
					return getString(EnumStringUtil.getPropertyString(object));
				}
			}

		})
		{
			@Override
			protected void onSelectionChanged(ColonAfspraakslotStatus selection, AjaxRequestTarget target, String markupId)
			{
				target.add(table);
				target.add(totaalBlokken);
			}
		});

		FormComponent<Date> startDatum = ComponentHelper.addTextField(form, "startDatum", true, 10, Date.class, false);
		startDatum.setType(Date.class);
		startDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(table);
				target.add(totaalBlokken);
			}

		});

		FormComponent<Date> eindDatum = ComponentHelper.addTextField(form, "eindDatum", true, 10, Date.class, false);
		eindDatum.setType(Date.class);
		eindDatum.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(totaalBlokken);
				target.add(table);
			}

		});

		add(new ExportToXslLink<>("csv", "Afspraakslots", table));

	}
}
