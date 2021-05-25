
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.HibernateIdChoiceRenderer;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Organisatie;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.input.validator.BSNValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator.Operator;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;
import nl.topicuszorg.wicket.planning.web.component.TimeField;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_INZIEN_LOGGING,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class LoggingInzienPage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private WebMarkupContainer refreshContainer;

	public LoggingInzienPage()
	{

		IModel<LoggingZoekCriteria> logZoekCriteria;
		if (ScreenitSession.get().isZoekObjectGezetForComponent(LoggingInzienPage.class))
		{
			logZoekCriteria = (IModel<LoggingZoekCriteria>) ScreenitSession.get().getZoekObject(LoggingInzienPage.class);
		}
		else
		{
			LoggingZoekCriteria criteria = new LoggingZoekCriteria();
			criteria.setVanaf(currentDateSupplier.getDateTimeMidnight().minusWeeks(4).toDate());
			criteria.setTot(currentDateSupplier.getDateTimeMidnight().plusDays(1).toDate());
			logZoekCriteria = new Model<LoggingZoekCriteria>(criteria);
		}

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		add(new FilterForm("form", logZoekCriteria));

		List<IColumn<LogRegel, String>> columns = new ArrayList<>();
		columns.add(new AbstractColumn<LogRegel, String>(Model.of("BVO"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> cellItem, String componentId, IModel<LogRegel> rowModel)
			{
				cellItem.add(new Label(componentId, rowModel.getObject().getAfkortingen()));
			}
		});
		columns.add(new AbstractColumn<LogRegel, String>(Model.of("Regio"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> cellItem, String componentId, IModel<LogRegel> rowModel)
			{
				ScreeningOrganisatie regio = null;

				LogRegel logRegel = rowModel.getObject();

				if (logRegel.getClient() != null && logRegel.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie() != null)
				{
					regio = logRegel.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
				}

				if (regio == null)
				{
					Instelling gebruikerInstelling = null;
					if (logRegel.getIngelogdeGebruiker() != null)
					{
						gebruikerInstelling = logRegel.getIngelogdeGebruiker().getOrganisatie();
					}

					gebruikerInstelling = (Instelling) HibernateHelper.deproxy(gebruikerInstelling);
					if (gebruikerInstelling instanceof ScreeningOrganisatie)
					{
						regio = (ScreeningOrganisatie) gebruikerInstelling;
					}
				}
				if (regio != null)
				{
					cellItem.add(new Label(componentId, regio.getNaam()));
				}
				else
				{
					cellItem.add(new Label(componentId, ""));
				}
			}
		});
		if (ScreenitSession.get().getOnderzoeken().contains(Bevolkingsonderzoek.MAMMA))
		{
			columns.add(new PropertyColumn<LogRegel, String>(Model.of("SE"), "screeningsEenheid.naam"));
		}
		columns.add(new EnumPropertyColumn<LogRegel, String, LogGebeurtenis>(new SimpleStringResourceModel("label.gebeurtenis"), "logGebeurtenis", "logGebeurtenis")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> item, String componentId, IModel<LogRegel> rowModel)
			{
				super.populateItem(item, componentId, rowModel);
				item.add(new AttributeAppender("class", new Model<String>("column-gebeurtenis")));
			}
		});
		columns.add(new DateTimePropertyColumn<LogRegel, String>(new SimpleStringResourceModel("label.datumtijd"), "gebeurtenisDatum", "gebeurtenisDatum",
			Constants.getDateTimeSecondsFormat())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> item, String componentId, IModel<LogRegel> rowModel)
			{
				super.populateItem(item, componentId, rowModel);
				item.add(new AttributeAppender("class", new Model<String>("column-datum-tijd"), " "));
			}
		});
		columns.add(new PropertyColumn<LogRegel, String>(new SimpleStringResourceModel("label.gebruiker"), "gebruiker.gebruikersnaam", "gebruiker.gebruikersnaam")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<LogRegel>> item, String componentId, IModel<LogRegel> rowModel)
			{
				super.populateItem(item, componentId, rowModel);
				item.add(new AttributeAppender("class", new Model<String>("column-gebruiker")));
			}
		});
		columns.add(new PropertyColumn<LogRegel, String>(new SimpleStringResourceModel("label.client"), "persoon.achternaam", "client")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<?> getDataModel(IModel<LogRegel> rowModel)
			{
				return new Model(NaamUtil.getNaamClientMetBsnMetGeboortedatum(rowModel.getObject().getClient()));
			}

			@Override
			public void populateItem(final Item<ICellPopulator<LogRegel>> item, final String componentId, final IModel<LogRegel> rowModel)
			{
				super.populateItem(item, componentId, rowModel);
				item.add(new AttributeAppender("class", new Model<String>("column-bsn")));
			}

		});
		columns.add(new PropertyColumn<LogRegel, String>(new SimpleStringResourceModel("label.event.melding"), "logEvent", "logEvent.melding")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(final Item<ICellPopulator<LogRegel>> item, final String componentId, final IModel<LogRegel> rowModel)
			{
				item.add(new Label(componentId, getDataModel(rowModel)).setEscapeModelStrings(false));
				item.add(new AttributeAppender("class", new Model<String>("column-melding")));
			}
		});

		refreshContainer.add(new LoggingTable("logging", columns, new LoggingDataProvider(logZoekCriteria), 10));
	}

	private class FilterForm extends Form<LoggingZoekCriteria>
	{

		private static final long serialVersionUID = 1L;

		public FilterForm(String id, IModel<LoggingZoekCriteria> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			add(new TextField<>("gebruikersnaam"));
			add(new TextField<>("melding"));
			add(new TextField<>("bsnClient").add(new BSNValidator()));

			DateTimeField vanaf = new DateTimeField("vanaf")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
				{
					DatePicker<Date> datePicker = super.newDatePicker(wicketId, model);
					datePicker.setLabel(new Model<String>("Datum/tijd vanaf"));
					return datePicker;
				}

				@Override
				protected TimeField newTimeField(String wicketId, IModel<Date> model)
				{
					TimeField timeField = new TimeField(wicketId, model)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected TextField<Integer> getHoursField()
						{
							TextField<Integer> hours = super.getHoursField();
							hours.setLabel(new Model<String>("Datum/tijd vanaf uren"));
							return hours;
						}

						@Override
						protected TextField<Integer> getMinutesField()
						{
							TextField<Integer> minutes = super.getMinutesField();
							minutes.setLabel(new Model<String>("Datum/tijd vanaf minuten"));
							return minutes;
						}

						@Override
						public void convertInput()
						{
							Integer m = getMinutesField().getConvertedInput();
							Integer h = getHoursField().getConvertedInput();

							if (h == null)
							{
								getHoursField().setConvertedInput(00);
							}
							if (m == null)
							{
								getMinutesField().setConvertedInput(00);
							}
							super.convertInput();
						}
					};
					return timeField;
				}

			};
			DateTimeField tot = new DateTimeField("tot")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
				{
					DatePicker<Date> datePicker = super.newDatePicker(wicketId, model);
					datePicker.setLabel(new Model<String>("Datum/tijd tot"));
					return datePicker;
				}

				@Override
				protected TimeField newTimeField(String wicketId, IModel<Date> model)
				{
					TimeField timeField = new TimeField(wicketId, model)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected TextField<Integer> getHoursField()
						{
							TextField<Integer> hours = super.getHoursField();
							hours.setLabel(new Model<String>("Datum/tijd tot uren"));
							return hours;
						}

						@Override
						protected TextField<Integer> getMinutesField()
						{
							TextField<Integer> minutes = super.getMinutesField();
							minutes.setLabel(new Model<String>("Datum/tijd tot minuten"));
							return minutes;
						}

						@Override
						public void convertInput()
						{
							Integer m = getMinutesField().getConvertedInput();
							Integer h = getHoursField().getConvertedInput();

							if (h == null)
							{
								getHoursField().setConvertedInput(23);
							}
							if (m == null)
							{
								getMinutesField().setConvertedInput(59);
							}
							super.convertInput();
						}

					};
					return timeField;
				}
			};
			add(tot, vanaf);

			AjaxSubmitLink filterButton = new IndicatingAjaxSubmitLink("filteren", this)
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					super.onSubmit(target);
					ScreenitSession.get().setZoekObject(LoggingInzienPage.class, FilterForm.this.getModel());
					target.add(refreshContainer);
				}
			};

			setDefaultButton(filterButton);

			add(filterButton);

			add(new ScreenitListMultipleChoice<Level>("level", Arrays.asList(Level.values()), new EnumChoiceRenderer<Level>()));

			List<MammaScreeningsEenheid> screeningsEenheden = screeningsEenheidService.getActieveScreeningsEenheden();
			List<Long> seIds = screeningsEenheden.stream().map(AbstractHibernateObject::getId).collect(Collectors.toList());
			final ScreenitDropdown<Long> screeningsEenheidDropdown = new ScreenitDropdown<>("screeningsEenheidId", seIds,
				new HibernateIdChoiceRenderer(screeningsEenheden, "naam"));
			screeningsEenheidDropdown.setNullValid(true);
			screeningsEenheidDropdown.setVisible(ScreenitSession.get().getOnderzoeken().contains(Bevolkingsonderzoek.MAMMA));
			add(screeningsEenheidDropdown);

			add(new ScreenitListMultipleChoice<LogGebeurtenis>("gebeurtenis", Arrays.asList(LogGebeurtenis.values()), new EnumChoiceRenderer<LogGebeurtenis>()));

			add(new ScreenitListMultipleChoice<Bevolkingsonderzoek>("bevolkingsonderzoeken", ScreenitSession.get().getOnderzoeken(),
				new EnumChoiceRenderer<Bevolkingsonderzoek>()));

			add(new DependantDateValidator(vanaf, tot, Operator.AFTER));

			List<Instelling> soLijst = organisatieZoekService.getAllActieveOrganisatiesWithType(ScreeningOrganisatie.class);

			add(new ScreenitDropdown<Long>("regio", soLijst.stream().map(Organisatie::getId).collect(Collectors.toList()),
				new HibernateIdChoiceRenderer(soLijst, "naam")).setNullValid(true));
		}
	}
}
