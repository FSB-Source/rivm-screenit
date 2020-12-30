
package nl.rivm.screenit.main.web.gebruiker.clienten.project;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.model.GebeurtenisBron;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.modal.IDialogCloseCallback;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.GebeurtenisComparator;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientProjectGegevens extends ClientPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private WebMarkupContainer gebeurtenissenContainer;

	private final BootstrapDialog dialog;

	private IModel<Client> clientModel;

	private IModel<ProjectClientAttribuut> filterPapcModel;

	private IModel<ProjectClient> projectClientModel;

	public ClientProjectGegevens(IModel<ProjectClient> projectClient)
	{
		super(ModelUtil.cModel(projectClient.getObject().getClient()));
		clientModel = (IModel<Client>) getDefaultModel();
		projectClientModel = projectClient;
		filterPapcModel = ModelUtil.cModel(new ProjectClientAttribuut());
		filterPapcModel.getObject().setProjectClient(projectClient.getObject());

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		gebeurtenissenContainer = getGebeurtenissenContainer();
		add(gebeurtenissenContainer);

		WebMarkupContainer attributenContainer = getAttributenContainer();
		add(attributenContainer);

		ProjectClient pc = projectClientModel.getObject();

		add(new Label("title.project.naam", Model.of(pc.getProject().getNaam())));

		add(new Label("project.naam", Model.of(pc.getProject().getNaam())));
		add(new Label("project.status", Model.of(getString(EnumStringUtil.getPropertyString(ProjectUtil.getStatus(pc.getProject(), currentDateSupplier.getDate()))))));

		add(new Label("groep.naam", Model.of(pc.getGroep().getNaam())));
		add(new Label("groep.status", Model.of(pc.getGroep().getActief() ? "Actief" : "Inactief")));

		add(new Label("client.status", Model.of(pc.getActief() ? "Actief" : "Inactief")));

		WebMarkupContainer inactiveerContainer = new WebMarkupContainer("inactiveerContainer");

		String inactiveerReden = null;
		if (pc.getProjectInactiefReden() != null)
		{
			inactiveerReden = getString(EnumStringUtil.getPropertyString(pc.getProjectInactiefReden()));
		}
		inactiveerContainer.add(new Label("client.inactiveerreden", Model.of(inactiveerReden)));
		inactiveerContainer.add(DateLabel.forDatePattern("client.inactiveerdatum", Model.of(pc.getProjectInactiefDatum()), "dd-MM-yyyy"));
		inactiveerContainer.setVisible(!pc.getActief());
		add(inactiveerContainer);

		add(new ClientPaspoortPanel("paspoort", clientModel));

		add(new ConfirmingIndicatingAjaxLink<Void>("inactiveren", dialog, "question.project.inactiveer.client")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				clientService.projectClientInactiveren(clientModel.getObject(), ProjectInactiefReden.VIA_INFOLIJN, null);
				setResponsePage(new ClientProjectenPage(clientModel));
			}

			@Override
			public boolean isVisible()
			{
				ProjectClient pClient = projectClientModel.getObject();
				boolean pClientActief = pClient.getActief();
				boolean groepActief = pClient.getGroep().getActief();
				boolean projectActief = ProjectStatus.ACTIEF.equals(ProjectUtil.getStatus(pClient.getProject(), currentDateSupplier.getDate()));
				return pClientActief && groepActief && projectActief;
			}
		});

		add(new ConfirmingIndicatingAjaxLink<Void>("activeren", dialog, "question.project.activeer.client")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ProjectClient pClient = projectClientModel.getObject();
				String foutMelding = clientService.projectClientActiveren(pClient);
				if (foutMelding != null)
				{
					error(getString(foutMelding));
				}
				else
				{
					setResponsePage(new ClientProjectenPage(clientModel));
				}
			}

			@Override
			public boolean isVisible()
			{
				ProjectClient pClient = projectClientModel.getObject();
				boolean pClientNietActief = !pClient.getActief();
				boolean groepActief = pClient.getGroep().getActief();
				boolean projectActief = ProjectStatus.ACTIEF.equals(ProjectUtil.getStatus(pClient.getProject(), currentDateSupplier.getDate()));
				return pClientNietActief && groepActief && projectActief;
			}
		});
	}

	private WebMarkupContainer getGebeurtenissenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupId(true);

		ProjectClient pClient = projectClientModel.getObject();

		List<ScreeningRondeGebeurtenis> dg = dossierService.getProjectGebeurtenissen(pClient);
		IModel<List<ScreeningRondeGebeurtenis>> dossierModel = new DetachableListModel<ScreeningRondeGebeurtenis>(dg);

		PropertyListView<ScreeningRondeGebeurtenis> gebeurtenissen = new PropertyListView<ScreeningRondeGebeurtenis>("gebeurtenissen",
			new SortingListModel<ScreeningRondeGebeurtenis>(dossierModel, new GebeurtenisComparator()))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<ScreeningRondeGebeurtenis> item)
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = item.getModelObject();
				final TypeGebeurtenis gebeurtenis = screeningRondeGebeurtenis.getGebeurtenis();
				item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
				item.add(new EnumLabel<TypeGebeurtenis>("gebeurtenis"));
				item.add(new EnumLabel<GebeurtenisBron>("bron"));

				if (gebeurtenis.getDetailPanelClass() != null)
				{
					item.add(new AttributeAppender("class", new Model<String>("badge-clickable"), " "));
				}
				else
				{
					item.add(new AttributeAppender("class", new Model<String>("badge-not-clickable"), " "));
				}

				item.add(new Label("extraOmschrijving", new IModel<String>()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public String getObject()
					{
						ScreeningRondeGebeurtenis gebeurtenis2 = item.getModelObject();
						String[] extraOmschrijvingen = gebeurtenis2.getExtraOmschrijving();
						String extraOmschrijving = "";
						if (extraOmschrijvingen != null)
						{
							for (String omschrijving : extraOmschrijvingen)
							{
								if (omschrijving != null)
								{
									if (StringUtils.isNotBlank(extraOmschrijving))
									{
										extraOmschrijving += ", ";
									}
									else
									{
										extraOmschrijving = "(";
									}
									extraOmschrijving += getString(omschrijving, null, omschrijving);
								}
							}
						}
						if (StringUtils.isNotBlank(extraOmschrijving))
						{
							extraOmschrijving += ")";
						}
						return extraOmschrijving;
					}
				})
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onConfigure()
					{
						super.onConfigure();
						setVisible(item.getModelObject().getExtraOmschrijving() != null);
					}
				});

				if (gebeurtenis.getDetailPanelClass() != null && ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(gebeurtenis.getDetailPanelClass()))
				{
					item.add(new AjaxEventBehavior("click")
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							if (gebeurtenis.equals(TypeGebeurtenis.PROJECT_BRIEF_TEGENHOUDEN) || gebeurtenis.equals(TypeGebeurtenis.PROJECT_BRIEF_AANGEMAAKT))
							{
								dialog.setCloseCallback(new IDialogCloseCallback()
								{

									private static final long serialVersionUID = 1L;

									@Override
									public void onCloseClick(AjaxRequestTarget target)
									{
										WebMarkupContainer nieuwGebCont = getGebeurtenissenContainer();
										gebeurtenissenContainer.replaceWith(nieuwGebCont);
										gebeurtenissenContainer = nieuwGebCont;
										target.add(gebeurtenissenContainer);
									}
								});
								dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
							}
							else
							{
								dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
							}
						}

					});
				}
			}
		};
		container.add(gebeurtenissen);

		return container;
	}

	private WebMarkupContainer getAttributenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("attributenContainer");
		container.setOutputMarkupId(true);

		List<IColumn<ProjectClientAttribuut, String>> columns = new ArrayList<IColumn<ProjectClientAttribuut, String>>();
		columns.add(new PropertyColumn<ProjectClientAttribuut, String>(Model.of("Attribuutnaam"), "attribuut.naam", "attribuut.naam"));
		columns.add(new PropertyColumn<ProjectClientAttribuut, String>(Model.of("Attribuutwaarde"), "value", "value")
		{
			@Override
			public IModel<?> getDataModel(IModel<ProjectClientAttribuut> rowModel)
			{
				ProjectClientAttribuut projectClientAttribuut = rowModel.getObject();
				if (Boolean.TRUE.equals(projectClientAttribuut.getAttribuut().getNietZichtbaarInClientDossier()))
				{
					projectClientAttribuut.setValue("*****");
				}
				return super.getDataModel(Model.of(projectClientAttribuut));
			}
		});

		ScreenitDataTable<ProjectClientAttribuut, String> dataTable = new ScreenitDataTable<ProjectClientAttribuut, String>("attributen", columns,
			new ClientProjectAttributenDataProvider(filterPapcModel), 5, Model.of("Attributen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean isRowClickable(IModel<ProjectClientAttribuut> rowModel)
			{
				return false;
			}
		};
		container.add(dataTable);

		return container;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientModel);
		ModelUtil.nullSafeDetach(projectClientModel);
		ModelUtil.nullSafeDetach(filterPapcModel);
	};
}
