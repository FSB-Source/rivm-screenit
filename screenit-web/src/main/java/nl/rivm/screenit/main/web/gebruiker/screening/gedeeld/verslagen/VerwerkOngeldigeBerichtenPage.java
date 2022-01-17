
package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.BerichtenZoekFilter;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class VerwerkOngeldigeBerichtenPage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	private final BootstrapDialog dialog;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private OngeldigeBerichtenService ongeldigeBerichtenService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ClientService clientService;

	private Component berichtenTabel;

	private final IModel<BerichtenZoekFilter> berichtenZoekFilter;

	@SpringBean
	private LogService logService;

	public VerwerkOngeldigeBerichtenPage()
	{
		this.berichtenZoekFilter = getBerichtenZoekFilterModel();

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new FilterForm("form", this.berichtenZoekFilter));

		addOrReplaceTable(null);
	}

	private IModel<BerichtenZoekFilter> getBerichtenZoekFilterModel()
	{
		IModel<BerichtenZoekFilter> filterIModel;
		if ((filterIModel = (IModel<BerichtenZoekFilter>) ScreenitSession.get().getZoekObject(VerwerkOngeldigeBerichtenPage.class)) != null)
		{
			return filterIModel;
		}
		filterIModel = new Model<>(new BerichtenZoekFilter());
		BerichtenZoekFilter berichtenZoekFilter = filterIModel.getObject();
		List<Bevolkingsonderzoek> onderzoeken = ScreenitSession.get().getOnderzoeken();
		if (onderzoeken.contains(Bevolkingsonderzoek.COLON))
		{
			berichtenZoekFilter.setMldBerichten(true);
			berichtenZoekFilter.setPaLabBerichten(true);
		}
		else
		{
			berichtenZoekFilter.setMldBerichten(false);
			berichtenZoekFilter.setPaLabBerichten(false);
		}
		if (onderzoeken.contains(Bevolkingsonderzoek.CERVIX))
		{
			berichtenZoekFilter.setCytologieBerichten(true);
		}
		else
		{
			berichtenZoekFilter.setCytologieBerichten(false);
		}
		if (onderzoeken.contains(Bevolkingsonderzoek.MAMMA))
		{
			berichtenZoekFilter.setFollowUpBerichten(true);
		}
		else
		{
			berichtenZoekFilter.setFollowUpBerichten(false);
		}
		return filterIModel;
	}

	private void addOrReplaceTable(AjaxRequestTarget target)
	{

		List<IColumn<MeldingOngeldigCdaBericht, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<MeldingOngeldigCdaBericht, String>(Model.of("Datum/tijd melding"), "datum", "datum"));
		columns
			.add(new DateTimePropertyColumn<MeldingOngeldigCdaBericht, String>(Model.of("Datum/tijd ontvangst"), "ontvangenCdaBericht.ontvangen", "ontvangenCdaBericht.ontvangen"));
		columns.add(new PropertyColumn<MeldingOngeldigCdaBericht, String>(Model.of("Type"), "ontvangenCdaBericht.berichtType", "ontvangenCdaBericht.berichtType"));
		columns.add(new PropertyColumn<MeldingOngeldigCdaBericht, String>(Model.of("Reden ongeldigheid"), "melding", "melding"));
		ToegangLevel toegangsLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN);
		if (toegangsLevel == ToegangLevel.LANDELIJK)
		{
			columns.add(new PropertyColumn<MeldingOngeldigCdaBericht, String>(Model.of("Screeningsorganisatie"), "screeningOrganisatie.naam", "screeningOrganisatie.naam"));
		}

		final boolean magClientDossierInzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_GEGEVENS, Actie.INZIEN);

		if (magClientDossierInzien)
		{
			columns.add(new AbstractColumn<MeldingOngeldigCdaBericht, String>(Model.of(getString("bekijkClientdossier")))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MeldingOngeldigCdaBericht>> cellItem, String componentId, IModel<MeldingOngeldigCdaBericht> rowModel)
				{
					Client client = clientService.getClientByBsn(rowModel.getObject().getBsn());
					if (client != null && !GbaStatus.BEZWAAR.equals(client.getGbaStatus()) && !GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
					{
						cellItem.add(new AjaxImageCellPanel<MeldingOngeldigCdaBericht>(componentId, rowModel, "icon-user")
						{

							private static final long serialVersionUID = 1L;

							@Override
							protected void onClick(AjaxRequestTarget target)
							{
								setResponsePage(new ClientInzienPage(new SimpleHibernateModel<>(clientService.getClientByBsn(rowModel.getObject().getBsn()))));
							}
						});
					}
					else
					{
						cellItem.add(new EmptyPanel(componentId));
					}
				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}

		columns.add(new AbstractColumn<MeldingOngeldigCdaBericht, String>(Model.of("Bekijk bericht"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<MeldingOngeldigCdaBericht>> cellItem, String componentId, IModel<MeldingOngeldigCdaBericht> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<MeldingOngeldigCdaBericht>(componentId, rowModel, "icon-info-sign")
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						dialog.openWith(target, new BerichtInzienPanel(IDialog.CONTENT_ID, getModel())
						{

							private static final long serialVersionUID = 1L;

							@Override
							protected void opnieuwAanbieden(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target)
							{
								dialog.close(target);
								berichtOpnieuwAanbieden(model, target);
							}

							@Override
							protected void verwijderen(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target)
							{
								dialog.close(target);
								verwijderMelding(model, target);
							}

						});
					}
				});

			}

			@Override
			public String getCssClass()
			{
				return "status";
			}

		});
		final boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.AANPASSEN);
		if (magAanpassen)
		{
			columns.add(new AbstractColumn<MeldingOngeldigCdaBericht, String>(Model.of("Opnieuw aanbieden"))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MeldingOngeldigCdaBericht>> cellItem, String componentId, final IModel<MeldingOngeldigCdaBericht> rowModel)
				{
					if (Boolean.TRUE.equals(rowModel.getObject().getHerstelbaar()))
					{
						cellItem.add(new AjaxImageCellPanel<MeldingOngeldigCdaBericht>(componentId, rowModel, "icon-refresh")
						{

							private static final long serialVersionUID = 1L;

							@Override
							protected void onClick(AjaxRequestTarget target)
							{
								berichtOpnieuwAanbieden(getModel(), target);
							}
						});
					}
					else
					{
						cellItem.add(new EmptyPanel(componentId));
					}
				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}
		final boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.VERWIJDEREN);
		if (magVerwijderen)
		{
			columns.add(new AbstractColumn<MeldingOngeldigCdaBericht, String>(Model.of("Verwijderen"))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void populateItem(Item<ICellPopulator<MeldingOngeldigCdaBericht>> cellItem, String componentId, final IModel<MeldingOngeldigCdaBericht> rowModel)
				{
					final AjaxImageCellPanel<MeldingOngeldigCdaBericht> imageCellPanel = new AjaxImageCellPanel<MeldingOngeldigCdaBericht>(componentId, rowModel, "icon-trash")
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							dialog.openWith(target,
								new ConfirmPanel(IDialog.CONTENT_ID, new SimpleStringResourceModel("ongeldigBerichtVerwijderen"), null, new DefaultConfirmCallback()
								{

									private static final long serialVersionUID = 1L;

									@Override
									public void onYesClick(AjaxRequestTarget target)
									{
										verwijderMelding(getModel(), target);
									}

								}, dialog));

						}
					};
					cellItem.add(imageCellPanel);

				}

				@Override
				public String getCssClass()
				{
					return "status";
				}

			});
		}

		Component newScreenitDataTable = new ScreenitDataTable<MeldingOngeldigCdaBericht, String>("tabel", columns, new OngeldigeBerichtenDataProvider(berichtenZoekFilter), 10,
			Model.of("ongeldig(e) bericht(en)"));
		newScreenitDataTable.setOutputMarkupId(true);
		if (berichtenTabel != null)
		{
			berichtenTabel.replaceWith(newScreenitDataTable);
		}
		else
		{
			add(newScreenitDataTable);
		}
		berichtenTabel = newScreenitDataTable;

		if (target != null)
		{
			target.add(berichtenTabel);
		}
	}

	private class FilterForm extends Form<BerichtenZoekFilter>
	{

		private static final long serialVersionUID = 1L;

		public FilterForm(String id, IModel<BerichtenZoekFilter> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			List<ScreeningOrganisatie> list = instellingService.getAllActiefScreeningOrganisaties();
			List<ScreeningOrganisatie> toegestaneSos = new ArrayList<>();
			for (ScreeningOrganisatie so : list)
			{
				if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN, Actie.INZIEN, so))
				{
					toegestaneSos.add(so);
				}
			}
			ScreenitDropdown<ScreeningOrganisatie> screeningOrganisatie = new ScreenitDropdown<>("screeningOrganisatie", ModelUtil.listRModel(toegestaneSos));
			add(screeningOrganisatie);
			ToegangLevel toegangsLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_SCREENING_VERWERKEN_ONGELIDGE_BERICHTEN);
			if (toegestaneSos.size() == 1 && toegangsLevel != ToegangLevel.LANDELIJK)
			{
				screeningOrganisatie.setNullValid(false);
				screeningOrganisatie.setEnabled(false);
				getModelObject().setScreeningOrganisatie(toegestaneSos.get(0));
			}

			add(new TextField<>("text"));

			List<Bevolkingsonderzoek> onderzoeken = ScreenitSession.get().getOnderzoeken();
			boolean colon = onderzoeken.contains(Bevolkingsonderzoek.COLON);
			boolean cervix = onderzoeken.contains(Bevolkingsonderzoek.CERVIX);
			boolean mamma = onderzoeken.contains(Bevolkingsonderzoek.MAMMA);

			CheckBox mldBerichten = new CheckBox("mldBerichten");
			CheckBox paLabBerichten = new CheckBox("paLabBerichten");
			CheckBox cytologieBerichten = new CheckBox("cytologieBerichten");
			CheckBox followUpBerichten = new CheckBox("followUpBerichten");
			mldBerichten.setVisible(colon);
			paLabBerichten.setVisible(colon);
			cytologieBerichten.setVisible(cervix);
			followUpBerichten.setVisible(mamma);
			add(mldBerichten);
			add(paLabBerichten);
			add(cytologieBerichten);
			add(followUpBerichten);
			add(new IndicatingAjaxButton("filter", this)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					addOrReplaceTable(target);
				}
			});
			add(new ConfirmingIndicatingAjaxSubmitLink<Void>("herzenden", this, dialog, "alleGefilterdeOngeldigBerichtOpnieuwAanbieden")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					ScreenitSession.get().setZoekObject(VerwerkOngeldigeBerichtenPage.class, getForm().getModel());
					ongeldigeBerichtenService.herverwerkAlleBerichten(ModelUtil.nullSafeGet(FilterForm.this.getModel()));
				}
			});
		}
	}

	private void berichtOpnieuwAanbieden(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target)
	{
		ongeldigeBerichtenService.berichtOpnieuwAanbieden(model.getObject());
		info("Ontvangen bericht is opnieuw aangeboden ter verwerking door de batch applicatie.");
		addOrReplaceTable(target);
	}

	private void verwijderMelding(IModel<MeldingOngeldigCdaBericht> model, AjaxRequestTarget target)
	{
		ongeldigeBerichtenService.verwijderenOngeldigBericht(model.getObject());

		StringBuilder berichtInformatie = new StringBuilder();
		MeldingOngeldigCdaBericht ontvangenMelding = model.getObject();
		if (ontvangenMelding.getOntvangenCdaBericht().getBerichtType().equals(BerichtType.MDL_VERSLAG))
		{
			berichtInformatie.append(" Patient bsn: ");
			berichtInformatie.append(ontvangenMelding.getBsn());
		}
		else if (ontvangenMelding.getOntvangenCdaBericht().getBerichtType().equals(BerichtType.PA_LAB_VERSLAG))
		{
			berichtInformatie.append(" T-nummer: ");
			berichtInformatie.append(ontvangenMelding.getOntvangenCdaBericht().getSetId());
		}

		logService.logGebeurtenis(LogGebeurtenis.VERWIJDEREN_ONGELDIG_BERICHT, ScreenitSession.get().getLoggedInAccount(), getMelding(model, berichtInformatie, ontvangenMelding),
			Bevolkingsonderzoek.COLON);
		info("Melding is verwijderd.");
		addOrReplaceTable(target);
	}

	private String getMelding(IModel<MeldingOngeldigCdaBericht> model, StringBuilder berichtInformatie, MeldingOngeldigCdaBericht ontvangenMelding)
	{
		StringBuilder melding = new StringBuilder();
		if (model.getObject() != null)
		{
			melding.append("Bericht: ");
			melding.append(model.getObject().getMelding());
		}
		if (berichtInformatie != null)
		{
			melding.append(berichtInformatie.toString());
		}
		if (ontvangenMelding != null && ontvangenMelding.getUitvoerendeOrganisatie() != null)
		{
			melding.append(" Organisatie: ");
			melding.append(ontvangenMelding.getUitvoerendeOrganisatie().getNaam());
		}
		return melding.toString();
	}
}
