package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ExportToCsvLink;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.util.postcode.PostcodeFormatter;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.BSNValidator;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.export.AbstractExportableColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_INTAKE_WERKLIJST,
	organisatieTypeScopes = OrganisatieType.COLOSCOPIECENTRUM,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public abstract class WerklijstIntakePage extends ColonScreeningBasePage
{

	private ScreenitDataTable<ColonIntakeAfspraak, String> table;

	private final IModel<WerklijstIntakeFilter> zoekModel;

	private Form<WerklijstIntakeFilter> bsnForm;

	private Form<WerklijstIntakeFilter> form;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private ColonDossierBaseService dossierBaseService;

	@SpringBean
	private AfspraakService afspraakService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private Label aantalLabel;

	public WerklijstIntakePage(AfspraakStatus filterStatus)
	{
		ColoscopieCentrum intakelocatie = ScreenitSession.get().getColoscopieCentrum();
		add(new Label("intakelocatie", intakelocatie.getNaam()));
		add(new Label("title", getString("title." + EnumStringUtil.getPropertyString(filterStatus, AfspraakStatus.class))));
		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		zoekModel = getNewWerkLijstIntakeFilter(filterStatus);
		setDefaultModel(zoekModel);

		WerklijstIntakeDataProvider werklijstIntakeDataProvider = new WerklijstIntakeDataProvider(zoekModel, intakelocatie);
		if (this instanceof ColonOpenstaanteIntakesWerklijstPage)
		{
			werklijstIntakeDataProvider.setSort("volgendeUitnodiging.peildatum", SortOrder.ASCENDING);
		}
		if (this instanceof ColonAfgerondeIntakesWerklijstPage)
		{
			werklijstIntakeDataProvider.setSort("startTime", SortOrder.DESCENDING);
		}
		table = new ScreenitDataTable<ColonIntakeAfspraak, String>("tabel", getColumns(), werklijstIntakeDataProvider, 10, Model.of("afspraken"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ColonIntakeAfspraak> model)
			{
				dialog.openWith(target, new ColonConclusieVastleggenPanel(IDialog.CONTENT_ID, ModelUtil.cModel(ModelUtil.nullSafeGet(model)))
				{

					@Override
					protected void close(AjaxRequestTarget target)
					{
						target.add(table);
						target.add(aantalLabel);
						dialog.close(target);
					}

				});
			}

			@Override
			protected boolean isRowClickable(IModel<ColonIntakeAfspraak> model)
			{
				return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_INTAKE_WERKLIJST, Actie.AANPASSEN);
			}
		};
		add(table);

		form = new Form<>("form", zoekModel);
		form.setOutputMarkupId(true);
		add(form);

		boolean afgerondeAfspraken = AfspraakStatus.UITGEVOERD.equals(filterStatus);
		form.setVisible(!afgerondeAfspraken);

		final FormComponent<Date> vanaf = ComponentHelper.addTextField(form, "vanaf", false, 10, Date.class, false);
		vanaf.setType(Date.class);
		vanaf.setOutputMarkupId(true);
		vanaf.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onFilter(target);
			}
		});

		final FormComponent<Date> totEnMet = ComponentHelper.addTextField(form, "totEnMet", false, 10, Date.class, false);
		totEnMet.setType(Date.class);
		totEnMet.setOutputMarkupId(true);
		totEnMet.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onFilter(target);
			}
		});

		List<ConclusieTypeFilter> values = getFilterOpties();
		ScreenitDropdown<ConclusieTypeFilter> conclusieType = ComponentHelper.newDropDownChoice("conclusieTypeFilter", new ListModel<>(values), new EnumChoiceRenderer<>(this),
			false);
		conclusieType.setOutputMarkupId(true);
		conclusieType.setNullValid(true);
		conclusieType.setVisible(!AfspraakStatus.GEPLAND.equals(filterStatus));
		form.add(conclusieType);
		conclusieType.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onFilter(target);
			}
		});

		bsnForm = new Form<>("bsnForm", zoekModel);
		bsnForm.setOutputMarkupId(true);
		add(bsnForm);
		FormComponent<String> bsn = ComponentHelper.addTextField(bsnForm, "bsn", false, 10, String.class, false);
		bsn.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onFilterBsn(target);
			}
		});
		bsn.add(new BSNValidator());
		AjaxSubmitLink submitOnEnter = new AjaxSubmitLink("submitOnEnter")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				onFilterBsn(target);
			}
		};
		bsnForm.add(submitOnEnter);
		bsnForm.setDefaultButton(submitOnEnter);
		bsnForm.setVisible(!afgerondeAfspraken);

		addBsnGeboortedatumForm(afgerondeAfspraken);

		add(new ExportToCsvLink<>("csv", "Intake Afspraken", table.getDataProvider(), getColumns()).setVisible(!afgerondeAfspraken));
	}

	private void addBsnGeboortedatumForm(boolean afgerondeAfspraken)
	{
		Form<WerklijstIntakeFilter> form = new Form<>("bsnGebroortedatumForm", zoekModel);
		form.setOutputMarkupId(true);
		add(form);
		FormComponent<String> bsn = ComponentHelper.addTextField(form, "bsn", true, 10, String.class, false);
		bsn.add(new BSNValidator());
		form.add(
			new ScreenitDateTextField("geboortedatum").setRequired(true).setVisible(afgerondeAfspraken).setOutputMarkupId(true)
				.add(new AjaxFormComponentUpdatingBehavior("change")
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{
						target.add(getComponent());
					}
				}));
		AjaxSubmitLink submitOnEnter = new AjaxSubmitLink("zoek")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				onFilterBsn(target);
			}
		};
		form.add(submitOnEnter);
		form.setDefaultButton(submitOnEnter);
		form.setVisible(afgerondeAfspraken);
	}

	private List<IColumn<ColonIntakeAfspraak, String>> getColumns()
	{
		List<IColumn<ColonIntakeAfspraak, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<ColonIntakeAfspraak, String>(Model.of("Intakeafspraak"), "startTime", "startTime", new SimpleDateFormat("dd-MM-yyyy HH:mm"))
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonIntakeAfspraak> embeddedModel)
			{
				IModel<Object> dataModel = super.getDataModel(embeddedModel);
				if (embeddedModel.getObject().getRoosterItem() == null)
				{
					dataModel.setObject(dataModel.getObject() + " *");
				}
				return dataModel;
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Kamer"), "location.name", "location.name"));
		columns.add(new ClientColumn<>("persoon.achternaam", "client"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("Geslacht"), "persoon.geslacht", "client.persoon.geslacht"));
		columns.add(new PropertyColumn<ColonIntakeAfspraak, String>(Model.of("Adres"), "client.persoon.adres.straat")
		{

			@Override
			public IModel<Object> getDataModel(IModel<ColonIntakeAfspraak> rowModel)
			{
				GbaPersoon persoon = rowModel.getObject().getClient().getPersoon();

				Adres adres = AdresUtil.getAdres(persoon, new DateTime());
				return new Model(AdresUtil.getAdres(adres));
			}
		});

		columns.add(new PropertyColumn<ColonIntakeAfspraak, String>(Model.of("PC/Plaats"), "client.persoon.adres.plaats")
		{

			@Override
			public IModel<Object> getDataModel(IModel<ColonIntakeAfspraak> rowModel)
			{
				GbaPersoon persoon = rowModel.getObject().getClient().getPersoon();

				Adres adres = AdresUtil.getAdres(persoon, new DateTime());
				return new Model(PostcodeFormatter.formatPostcode(adres.getPostcode(), true) + " " + adres.getPlaats());
			}
		});

		columns.add(new AbstractExportableColumn<ColonIntakeAfspraak, String>(Model.of("Datum brief afspraak"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<ColonIntakeAfspraak>> cellItem, String componentId, IModel<ColonIntakeAfspraak> rowModel)
			{
				cellItem.add(new Label(componentId, getBriefAfgedrukt(rowModel)));
			}

			private String getBriefAfgedrukt(IModel<ColonIntakeAfspraak> rowModel)
			{
				String briefAfgedrukt = "";
				ColonScreeningRonde ronde = rowModel.getObject().getColonScreeningRonde();
				if (ronde != null)
				{
					ClientBrief<?, ?, ?> laatsteBrief = null;
					for (ColonBrief brief : ronde.getBrieven())
					{
						if (brief.getIntakeAfspraak() != null
							&& (BriefType.COLON_UITNODIGING_INTAKE.equals(brief.getBriefType())
								|| BriefType.COLON_INTAKE_GEWIJZIGD.equals(brief.getBriefType())))
						{
							laatsteBrief = bepaalLaatsteBrief(laatsteBrief, brief);
						}
					}
					if (laatsteBrief != null)
					{
						briefAfgedrukt = new SimpleDateFormat("dd-MM-yyyy").format(laatsteBrief.getMergedBrieven().getPrintDatum());
					}
				}
				return briefAfgedrukt;
			}

			private ClientBrief bepaalLaatsteBrief(ClientBrief laatsteBrief, ClientBrief brief)
			{
				laatsteBrief = bepaalLaatsteBriefInner(laatsteBrief, brief);
				ProjectBrief projectBrief = brief.getProjectBrief();
				if (projectBrief != null)
				{
					laatsteBrief = bepaalLaatsteBriefInner(laatsteBrief, projectBrief);
				}
				return laatsteBrief;
			}

			private ClientBrief bepaalLaatsteBriefInner(ClientBrief laatsteBrief, ClientBrief brief)
			{
				MergedBrieven<?> mergedBrieven = brief.getMergedBrieven();
				if (mergedBrieven != null && mergedBrieven.getPrintDatum() != null
					&& (laatsteBrief == null || laatsteBrief.getMergedBrieven().getPrintDatum().before(mergedBrieven.getPrintDatum())))
				{
					laatsteBrief = brief;
				}
				return laatsteBrief;
			}

			@Override
			public IModel<?> getDataModel(IModel<ColonIntakeAfspraak> rowModel)
			{
				return new Model(getBriefAfgedrukt(rowModel));
			}
		});

		columns.add(new PropertyColumn<ColonIntakeAfspraak, String>(Model.of("Huisarts"), "client.huisarts")
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonIntakeAfspraak> rowModel)
			{
				String huisarts = "";

				ColonScreeningRonde ronde = rowModel.getObject().getColonScreeningRonde();
				if (ronde != null && ronde.getColonHuisarts() != null)
				{
					huisarts = NaamUtil.getNaamHuisarts(ronde.getColonHuisarts());
				}
				else if (ronde != null && ronde.getOnbekendeHuisarts() != null)
				{
					huisarts = NaamUtil.getNaamOnbekendeHuisarts(ronde.getOnbekendeHuisarts());
				}

				return new Model(huisarts);
			}
		});
		if (this instanceof ColonOpenstaanteIntakesWerklijstPage)
		{
			columns.add(new PropertyColumn<ColonIntakeAfspraak, String>(Model.of("#dagen tot nieuwe BVO uitnodiging"), "volgendeUitnodiging.peildatum",
				"client.colonDossier.volgendeUitnodiging")
			{

				@Override
				public IModel<Object> getDataModel(IModel<ColonIntakeAfspraak> rowModel)
				{
					LocalDate datumVolgendeUitnodiging = dossierBaseService.getDatumVolgendeUitnodiging(rowModel.getObject().getClient().getColonDossier());
					String aantalDagenToGo = "";
					if (datumVolgendeUitnodiging != null)
					{
						aantalDagenToGo = "" + ChronoUnit.DAYS.between(dateSupplier.getLocalDate(), datumVolgendeUitnodiging);
					}
					return new Model(aantalDagenToGo);
				}
			});
		}
		if (!(this instanceof ColonGeplandeIntakesWerklijstPage))
		{
			columns.add(new EnumPropertyColumn<>(Model.of("Conclusie"), "conclusie.type", "conclusie.type"));
		}
		return columns;
	}

	protected abstract List<ConclusieTypeFilter> getFilterOpties();

	private IModel<WerklijstIntakeFilter> getNewWerkLijstIntakeFilter(AfspraakStatus afspraakStatus)
	{
		ScreenitSession session = ScreenitSession.get();
		IModel<WerklijstIntakeFilter> intakeFilter;
		if ((intakeFilter = (IModel<WerklijstIntakeFilter>) session.getZoekObject(this.getClass())) != null)
		{
			return intakeFilter;
		}
		intakeFilter = new CompoundPropertyModel<>(new WerklijstIntakeFilter());
		WerklijstIntakeFilter filter = intakeFilter.getObject();
		filter.setStatus(afspraakStatus);
		Integer uitnodigingsInterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}
		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}
		filter.setInterval(uitnodigingsInterval);
		filter.setMaxLeeftijd(maximaleLeeftijd);
		return intakeFilter;
	}

	private void onFilter(AjaxRequestTarget target)
	{
		WerklijstIntakeFilter filter = zoekModel.getObject();
		filter.setBsn(null);
		filter.setEersteKeerZoeken(false);
		target.add(table);
		target.add(bsnForm);
		ScreenitSession.get().setZoekObject(this.getClass(), zoekModel);
	}

	private void onFilterBsn(AjaxRequestTarget target)
	{
		WerklijstIntakeFilter filter = zoekModel.getObject();

		if (StringUtils.isNotBlank(filter.getBsn()))
		{
			filter.setVanaf(null);
			filter.setTotEnMet(null);
			filter.setConclusieTypeFilter(null);
		}
		else
		{
			filter.setEersteKeerZoeken(true);
		}
		target.add(table);
		target.add(form);
		ScreenitSession.get().setZoekObject(this.getClass(), zoekModel);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekModel);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.werklijst.geplande", ColonGeplandeIntakesWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.werklijst.openstaande", ColonOpenstaanteIntakesWerklijstPage.class)
		{

			@Override
			public Component getPostfix(String id)
			{
				WerklijstIntakeFilter filter = new WerklijstIntakeFilter();
				aantalLabel = new Label(id, new LoadableDetachableModel<String>()
				{

					@Override
					protected String load()
					{
						long aantal = afspraakService.countAfsprakenVoorColoscopiecentrum(filter, ScreenitSession.get().getColoscopieCentrum());
						return "(" + aantal + ")";
					}

				});
				aantalLabel.setOutputMarkupId(true);
				return aantalLabel;
			}

		});
		contextMenuItems
			.add(new GebruikerMenuItem("label.werklijst.afgeronde", ColonAfgerondeIntakesWerklijstPage.class));
		return contextMenuItems;
	}
}
