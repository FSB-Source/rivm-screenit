
package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.BooleanStringPropertyColumn;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;

public abstract class AfgeslotenOvereenkomstPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private final BootstrapDialog editOvereenkomstDialog = new BootstrapDialog("editOvereenkomstDialog");;

	private final WebMarkupContainer afgeslotenOvereenkomstenContainer = new WebMarkupContainer("afgeslotenOvereenkomsten");

	@SpringBean
	private HibernateService hibernateService;

	public AfgeslotenOvereenkomstPanel(String id, Actie actie, Gebruiker gebruiker, SortableDataProvider<AbstractAfgeslotenOvereenkomst, String> dataprovider,
		final IModel<Boolean> actiefModel)
	{
		super(id);
		add(new MedewerkerPaspoortPanel("paspoort", ModelUtil.cModel(gebruiker)));
		constructPanel(actie, dataprovider, actiefModel);
	}

	public AfgeslotenOvereenkomstPanel(String id, Actie actie, Instelling selectedOrganisatie, SortableDataProvider<AbstractAfgeslotenOvereenkomst, String> dataprovider,
		final IModel<Boolean> actiefModel)
	{

		super(id);
		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.cRModel(selectedOrganisatie)));
		constructPanel(actie, dataprovider, actiefModel);
	}

	private void constructPanel(Actie actie, SortableDataProvider<AbstractAfgeslotenOvereenkomst, String> dataprovider,
		final IModel<Boolean> actiefModel)
	{
		final BootstrapDialog confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		final boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);

		add(editOvereenkomstDialog);

		add(new AjaxLink<Void>("nieuweOvereenkomst")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				editOvereenkomstDialog.openWith(target, new AfgeslotenOvereenkomstEditPanel(BootstrapDialog.CONTENT_ID, createAfgeslotenOvereenkomst())
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onSubmit(AjaxRequestTarget target)
					{
						editOvereenkomstDialog.close(target);
						target.add(afgeslotenOvereenkomstenContainer);
					}

				});
			}
		}.setVisible(isMinimumActie(actie, Actie.TOEVOEGEN)));

		List<IColumn<AbstractAfgeslotenOvereenkomst, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.codeovereenkomst"), "code", "code"));
		columns.add(new PropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.naamovereenkomst"), "overeenkomst.naam", "overeenkomst.naam"));
		columns.add(new PropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.screeningsorganisatie"), "screeningOrganisatie.naam",
			"screeningOrganisatie.naam"));
		columns.add(new DateTimePropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.startdatum"), "startDatum", "startDatum",
			new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new DateTimePropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.einddatum"), "eindDatum", "eindDatum",
			new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new DateTimePropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.akkoorddatum"), "akkoordDatum", "akkoordDatum",
			new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new BooleanStringPropertyColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.nieuwereovereenkomst"),
			Constants.getBooleanWeergave(), "nieuwereOvereenkomst", "nieuwereOvereenkomst"));
		if (!inzien)
		{
			columns.add(new GeneratedDocumentDownloadColumn(new SimpleStringResourceModel("label.downloaden"), "overeenkomst.document"));
		}
		columns.add(new UploadDocumentDownloadColumn<AbstractAfgeslotenOvereenkomst, String>(new SimpleStringResourceModel("label.gescanddocumentdownloaden"), "gescandDocument"));
		columns
			.add(new BooleanStringPropertyColumn<AbstractAfgeslotenOvereenkomst, String>(Model.of("Te accorderen"), Constants.getBooleanWeergave(), "teAccoderen", "teAccoderen"));
		columns.add(new AbstractColumn<AbstractAfgeslotenOvereenkomst, String>(new Model<>(""))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<AbstractAfgeslotenOvereenkomst>> cellItem, String componentId, IModel<AbstractAfgeslotenOvereenkomst> rowModel)
			{
				cellItem.add(new AfgeslotenOvereenkomstActiefCellPanel<AbstractAfgeslotenOvereenkomst>(componentId, rowModel, null, null)
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected boolean isActief(IModel<AbstractAfgeslotenOvereenkomst> rowModel)
					{
						LocalDate start = new LocalDate(rowModel.getObject().getStartDatum());
						LocalDate eind = new LocalDate(rowModel.getObject().getEindDatum());
						return (start.isBefore(new LocalDate()) || start.isEqual(new LocalDate())) && (eind.isAfter(new LocalDate()) || eind.isEqual(new LocalDate()));
					}

					@Override
					protected void onToggleActief(AjaxRequestTarget target, AbstractAfgeslotenOvereenkomst actiefObject)
					{
						if (!inzien)
						{
							final IModel<AbstractAfgeslotenOvereenkomst> overeenkomst = new SimpleHibernateModel<AbstractAfgeslotenOvereenkomst>(actiefObject);
							if ((actiefObject.getEindDatum() == null || !actiefObject.getEindDatum().before(new Date())) && actiefObject.getStartDatum().before(new Date()))
							{
								confirmDialog.setContent(new ConfirmPanel(IDialog.CONTENT_ID, new SimpleStringResourceModel("label.afgeslotenovereenkomstdeactiveren"), null,
									new DefaultConfirmCallback()
									{

										private static final long serialVersionUID = 1L;

										@Override
										public void onYesClick(AjaxRequestTarget target)
										{
											overeenkomst.getObject().setEindDatum(new DateTime().minusDays(1).toDate());
											hibernateService.saveOrUpdate(overeenkomst.getObject());
											target.add(afgeslotenOvereenkomstenContainer);
											confirmDialog.close(target);
										}

									}, confirmDialog));
								confirmDialog.open(target);
							}
							else
							{
								info(getString("label.afgeslotenovereenkomstheractiveren"));
							}
						}
					}
				});
			}

			@Override
			public Component getHeader(String componentId)
			{
				return new AfgeslotenOvereenkomstActiefHeaderPanel(componentId, afgeslotenOvereenkomstenContainer, actiefModel);
			}
		});

		afgeslotenOvereenkomstenContainer.setOutputMarkupId(true);
		add(afgeslotenOvereenkomstenContainer);
		afgeslotenOvereenkomstenContainer
			.add(new ScreenitDataTable<AbstractAfgeslotenOvereenkomst, String>("afgeslotenOvereenkomsten", columns, dataprovider, new Model<String>("overeenkomsten"))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<AbstractAfgeslotenOvereenkomst> model)
				{
					if (!inzien)
					{
						super.onClick(target, model);
						editOvereenkomstDialog.openWith(target, new AfgeslotenOvereenkomstEditPanel(BootstrapDialog.CONTENT_ID, model.getObject())
						{

							private static final long serialVersionUID = 1L;

							@Override
							public void onSubmit(AjaxRequestTarget target)
							{
								editOvereenkomstDialog.close(target);
								target.add(afgeslotenOvereenkomstenContainer);
							}

						});
					}
				}

			});
	}

	protected boolean isMinimumActie(Actie actie, Actie minimaal)
	{
		return (Actie.INZIEN.equals(minimaal) && actie == null) || (actie != null && actie.getNiveau() >= minimaal.getNiveau());
	}

	protected abstract AbstractAfgeslotenOvereenkomst createAfgeslotenOvereenkomst();
}
