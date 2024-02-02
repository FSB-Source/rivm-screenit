package nl.rivm.screenit.main.web.gebruiker.algemeen.nieuws;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.NieuwsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.basic.MultiLineLabel;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class NieuwsPage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private NieuwsService nieuwsService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier iCurrentDateSupplier;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		Gebruiker gebruiker = ((ScreenitSession) getSession()).getLoggedInInstellingGebruiker().getMedewerker();

		WebMarkupContainer nieuwsContainer = new WebMarkupContainer("nieuws");
		WebMarkupContainer geenNieuwsContainer = new WebMarkupContainer("geenNieuws");

		List<Long> nieuwsItemIdsGebruiker = nieuwsService.getNieuwsItemIdsGebruiker(gebruiker);
		List<NieuwsItem> nieuwsItemsGebruiker = new ArrayList<>();
		for (Long nieuwItemId : nieuwsItemIdsGebruiker)
		{
			nieuwsItemsGebruiker.add(hibernateService.load(NieuwsItem.class, nieuwItemId));
		}
		IModel<List<NieuwsItem>> nieuwsItemsGebruikerModel = ModelUtil.listRModel(nieuwsItemsGebruiker);
		if (nieuwsItemsGebruikerModel.getObject() != null && !nieuwsItemsGebruikerModel.getObject().isEmpty())
		{
			ListView<NieuwsItem> nieuwsItemForms = new ListView<NieuwsItem>("nieuwsItem", nieuwsItemsGebruikerModel)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<NieuwsItem> item)
				{
					EditForm nieuwsItemForm = new EditForm("form", ModelUtil.cModel(item.getModelObject()));
					item.add(nieuwsItemForm);
				}
			};

			nieuwsContainer.add(nieuwsItemForms);
			geenNieuwsContainer.setVisible(false);
		}
		else
		{
			nieuwsContainer.setVisible(false);
		}

		add(nieuwsContainer);
		add(geenNieuwsContainer);
	}

	private class EditForm extends ScreenitForm<NieuwsItem>
	{
		private static final long serialVersionUID = 1L;

		public EditForm(String id, IModel<NieuwsItem> model)
		{
			super(id, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();

			Label titel = new Label("titel");
			MultiLineLabel tekst = new MultiLineLabel("tekst");

			String gewijzigdText = "";
			NieuwsItem nieuwsItem = (NieuwsItem) getDefaultModelObject();
			if (nieuwsItem.getGewijzigd() != null)
			{
				SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
				gewijzigdText = "Gewijzigd op " + format.format(nieuwsItem.getGewijzigd());
			}
			Label gewijzigd = new Label("gewijzigd", gewijzigdText);

			AjaxSubmitLink gelezen = new AjaxSubmitLink("gelezen")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					Gebruiker gebruiker = ((ScreenitSession) getSession()).getLoggedInInstellingGebruiker().getMedewerker();

					NieuwsItem formNieuwsItem = (NieuwsItem) EditForm.this.getDefaultModelObject();

					GebruikerNieuwsItem gebruikerNieuwsItem = null;
					for (GebruikerNieuwsItem item : gebruiker.getGebruikerNieuwsItems())
					{
						if (item.getNieuwsItem().equals(formNieuwsItem))
						{
							gebruikerNieuwsItem = item;
							break;
						}
					}

					if (gebruikerNieuwsItem == null)
					{
						gebruikerNieuwsItem = new GebruikerNieuwsItem();
						gebruikerNieuwsItem.setGebruiker(gebruiker);
						gebruikerNieuwsItem.setNieuwsItem(formNieuwsItem);

						formNieuwsItem.getGebruikerNieuwsItems().add(gebruikerNieuwsItem);
						gebruiker.getGebruikerNieuwsItems().add(gebruikerNieuwsItem);
					}

					gebruikerNieuwsItem.setNietZichtbaarVanaf(iCurrentDateSupplier.getDate());
					hibernateService.saveOrUpdate(gebruikerNieuwsItem);
				}

				@Override
				protected void onAfterSubmit(AjaxRequestTarget target)
				{
					super.onAfterSubmit(target);
					setResponsePage(new NieuwsPage());
				}
			};

			add(titel);
			add(tekst);
			add(gewijzigd);
			add(gelezen);
		}
	}
}
