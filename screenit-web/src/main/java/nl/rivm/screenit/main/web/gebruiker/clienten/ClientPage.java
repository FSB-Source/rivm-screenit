
package nl.rivm.screenit.main.web.gebruiker.clienten;

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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.dao.ClientContactDao;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.agenda.ClientAgendaPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.cis.ClientCISHistoriePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.complicatie.ClientComplicatiePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.documenten.ClientDocumentenPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.project.ClientProjectenPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.verslag.ClientVerslagenPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class ClientPage extends GebruikerBasePage
{

	private static final Logger LOG = LoggerFactory.getLogger(ClientPage.class);

	@SpringBean
	private ClientContactDao clientContactDao;

	private static final long serialVersionUID = 1L;

	private List<Component> postfixes = new ArrayList<>();

	public static Object[][] CLIENT_DOSSIER_TABS = new Object[][] 
	{
		{ "label.clientinzien", ClientInzienPage.class },
		{ "label.clientagenda", ClientAgendaPage.class },
		{ "label.rondes", ClientDossierPage.class },
		{ "label.clientcontact", ClientContactPage.class },
		{ "label.clientverslagen", ClientVerslagenPage.class },
		{ "label.clientcomplicaties", ClientComplicatiePage.class },
		{ "label.clientdocumenten", ClientDocumentenPage.class },
		{ "label.clientprojecten", ClientProjectenPage.class },
		{ "label.cis-historie", ClientCISHistoriePage.class },
	};

	public ClientPage(IModel<Client> client)
	{
		setDefaultModel(client);
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.CLIENTEN;
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return ClientZoekenPage.class;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.clientzoeken", ClientZoekenPage.class));
		for (Object[] menuItem : CLIENT_DOSSIER_TABS)
		{
			contextMenuItems.add(new ClientGebruikerMenuItem((String) menuItem[0], (Class<ClientPage>) menuItem[1])
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected ClientPage createPage()
				{
					List<Object> params = new ArrayList<>();
					params.add(getClientModel());
					ClientPage page = null;
					try
					{
						page = (ClientPage) ConstructorUtils.invokeConstructor(getTargetPageClass(), params.toArray());
					}
					catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
					{
						LOG.error("Fout bij aanmaken van " + getTargetPageClass(), e);
					}
					return page;
				}

				@Override
				public Component getPostfix(String id)
				{
					if (menuItem[0] == "label.clientcontact")
					{
						return getAantalPostfixLabel(id, getClientModel().getObject().getId());
					}
					else
					{
						return super.getPostfix(id);
					}
				}

			});
		}
		return contextMenuItems;
	}

	private IModel<Client> getClientModel()
	{
		return new SimpleHibernateModel<Client>(Client.class, ((Client) getDefaultModelObject()).getId());
	}

	private abstract class ClientGebruikerMenuItem extends GebruikerMenuItem
	{

		private static final long serialVersionUID = 1L;

		@SuppressWarnings("unchecked")
		public ClientGebruikerMenuItem(String resourceTag, Class<? extends ClientPage> targetPageClass)
		{
			super(resourceTag, targetPageClass);
		}

		@Override
		public IndicatingAjaxLink<?> createWicketLink(String markupId)
		{
			return new IndicatingAjaxLink<Client>(markupId, getClientModel())
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					setResponsePage(createPage());
				}
			};
		}

		protected abstract ClientPage createPage();
	}

	private <MBP extends ClientContact> Component getAantalPostfixLabel(String id, Long clientId)
	{
		Label label = new Label(id, new LoadableDetachableModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected String load()
			{
				long aantalContactenMetVrijeTekstvelden = clientContactDao.countClientContactenMetOpmerking(clientId);

				if (aantalContactenMetVrijeTekstvelden > 0L)
				{
					return "" + aantalContactenMetVrijeTekstvelden;
				}
				else
				{
					return "";
				}
			}
		});
		label.setOutputMarkupId(true);
		label.add(new AttributeAppender("class", "number-alert"));
		postfixes.add(label);
		return label;
	}

}
