package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.documentupload.wicket.UploadDocumentLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_ORGANISATIE_DOCUMENTEN,
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class OrganisatieDocumentenPage extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	private BootstrapDialog addDocumentDialog;

	private BootstrapDialog confirmDialog;

	private IModel<Instelling> organisatieModel;

	public OrganisatieDocumentenPage()
	{

		Instelling organisatie = getCurrentSelectedOrganisatie();
		organisatieModel = ModelUtil.sModel(organisatie);
		add(new OrganisatiePaspoortPanel("paspoort", organisatieModel));

		Form<Void> form = new Form<>("form");
		add(form);

		final WebMarkupContainer documentenContainer = new WebMarkupContainer("documentenContainer");
		documentenContainer.setOutputMarkupId(true);
		form.add(documentenContainer);

		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		addDocumentDialog = new BootstrapDialog("organisatieDocumentPopup");
		add(addDocumentDialog);

		documentenContainer.add(new PropertyListView<UploadDocument>("documenten", new PropertyModel<List<UploadDocument>>(organisatieModel, "documents"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<UploadDocument> item)
			{
				item.add(new Label("documentNaam", new PropertyModel<>(item.getModel(), "naam")));
				item.add(new UploadDocumentLink("download", item.getModel(), true).setVisible(item.getModelObject() != null));
				AjaxLink<UploadDocument> verwijderen = new ConfirmingIndicatingAjaxLink<UploadDocument>("delete", confirmDialog, "question.remove.document")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						instellingService.deleteDocumentForInstelling(item.getModelObject(), organisatieModel.getObject());
						target.add(documentenContainer);

					}
				};
				verwijderen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_ORGANISATIE_DOCUMENTEN, Actie.VERWIJDEREN));
				item.add(verwijderen);
			}

		});

		AjaxSubmitLink toevoegen = new AjaxSubmitLink("documentToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				UploadDocument nieuwDocument = new UploadDocument();
				addDocumentDialog.openWith(target, new OrganisatieDocumentPopupPanel(IDialog.CONTENT_ID, ModelUtil.cModel(nieuwDocument), organisatieModel, documentenContainer)
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void close(AjaxRequestTarget target)
					{
						addDocumentDialog.close(target);
						BasePage.markeerFormulierenOpgeslagen(target);

					}
				});

			}

		};
		toevoegen.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_ORGANISATIE_DOCUMENTEN, Actie.TOEVOEGEN));
		form.add(toevoegen);

		documentenContainer.add(new Label("aantalDocumenten", new PropertyModel<>(organisatieModel, "documents.size")));

	}
}
