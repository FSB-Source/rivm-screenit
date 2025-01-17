package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.UploadDocument_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst_;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_BEHEER_OVEREENKOMSTEN_MODELLEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class OvereenkomstenBeheerPage extends AlgemeenPage
{
	private final WebMarkupContainer overeenkomstenContainer;

	@SpringBean
	private OvereenkomstService overeenkomstService;

	private final BootstrapDialog editDialog;

	public OvereenkomstenBeheerPage()
	{
		overeenkomstenContainer = new WebMarkupContainer("overeenkomstenContainer");
		overeenkomstenContainer.setOutputMarkupId(true);
		add(overeenkomstenContainer);

		final OvereenkomstEditPanel overeenkomstEditPanel = new OvereenkomstEditPanel(IDialog.CONTENT_ID)
		{
			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				target.add(overeenkomstenContainer);
				editDialog.close(target);
			}
		};
		editDialog = new BootstrapDialog("editDialog", overeenkomstEditPanel);
		add(editDialog);
		BootstrapDialog confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		add(new AjaxLink<Void>("toevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				overeenkomstEditPanel.updateModel(new Overeenkomst());
				editDialog.open(target);
			}
		});

		List<IColumn<Overeenkomst, String>> columns = new ArrayList<>();
		columns.add(new AbstractColumn<>(new SimpleStringResourceModel("label.type"), "overeenkomst")
		{
			@Override
			public void populateItem(Item<ICellPopulator<Overeenkomst>> cellItem, String componentId, IModel<Overeenkomst> rowModel)
			{
				cellItem.add(new Label(componentId, getString("label.overeenkomsttype." + rowModel.getObject().getOvereenkomst().name().toLowerCase())));
			}
		});
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.naamovereenkomst"), Overeenkomst_.NAAM, Overeenkomst_.NAAM));
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.organisatieType"), Overeenkomst_.ORGANISATIE_TYPE, "organisatieType.naam"));
		columns.add(new PropertyColumn<>(new SimpleStringResourceModel("label.naambestand"), propertyChain(Overeenkomst_.DOCUMENT, UploadDocument_.NAAM),
			propertyChain(Overeenkomst_.DOCUMENT, UploadDocument_.NAAM)));
		columns.add(
			new DateTimePropertyColumn<>(new SimpleStringResourceModel("label.laatstgewijzigd"), Overeenkomst_.LAATSTE_UPDATE_DOCUMENT, Overeenkomst_.LAATSTE_UPDATE_DOCUMENT,
				Constants.getDateTimeSecondsFormat()));
		columns.add(new UploadDocumentDownloadColumn<>(new SimpleStringResourceModel("label.downloaden"), Overeenkomst_.DOCUMENT));

		IModel<Overeenkomst> zoekObject = new Model<>(new Overeenkomst());
		zoekObject.getObject().setActief(null);
		columns
			.add(new ActiefPropertyColumn<>(Model.of(""), Overeenkomst_.ACTIEF, overeenkomstenContainer, zoekObject, true, confirmDialog, "label.overeenkomstdeactiveren")
			{

				@Override
				protected void onAfterToggleActief(AjaxRequestTarget target, Overeenkomst actiefObject)
				{
					super.onAfterToggleActief(target, actiefObject);
					overeenkomstService.updateOvereenkomst(actiefObject, ScreenitSession.get().getLoggedInAccount());
				}

			});

		overeenkomstenContainer.add(
			new ScreenitDataTable<>("overeenkomsten", columns, new OvereenkomstenDataProvider(zoekObject), new SimpleStringResourceModel("label.overeenkomsten"))
			{
				@Override
				public void onClick(AjaxRequestTarget target, IModel<Overeenkomst> model)
				{
					overeenkomstEditPanel.updateModel(model.getObject());
					editDialog.open(target);
				}

			});
	}
}
