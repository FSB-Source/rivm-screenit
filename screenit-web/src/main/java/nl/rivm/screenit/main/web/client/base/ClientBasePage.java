
package nl.rivm.screenit.main.web.client.base;

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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;
import nl.rivm.screenit.main.web.client.dashboard.home.ClientHomeDashboardPage;
import nl.rivm.screenit.main.web.client.digid.ClientportaalDigidLoginPage;
import nl.rivm.screenit.main.web.component.panels.PrivacyreglementPanel;
import nl.rivm.screenit.main.web.component.panels.ResponsibleDisclosurePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.flow.RedirectToUrlException;
import org.apache.wicket.request.mapper.parameter.PageParameters;

public abstract class ClientBasePage extends BasePage
{

	private final Panel feedbackPanel;

	public ClientBasePage()
	{
		feedbackPanel = new BootstrapFeedbackPanel("feedback")
		{

			@Override
			protected Panel getSuccessFeedback(String id)
			{
				return new ClientSuccessLevelFeedbackPanel(id);
			}

		};
		add(feedbackPanel);
		add(new ResponsibleDisclosurePanel("responsibleDisclosure"));
		add(new PrivacyreglementPanel("privacyreglement"));

		addMenu();
		addLogo();

		add(new Label("clientNaam", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(getIngelogdeClient())).setVisible(getIngelogdeClient() != null));
		add(new Link<Void>("uitloggen")
		{

			@Override
			public void onClick()
			{
				String url = ScreenitSession.get().getAfkomstigURLRegioCode();
				String regioCode = ScreenitSession.get().getRegioCode();
				ScreenitSession.get().logout();
				PageParameters pageParameters = new PageParameters();
				if (StringUtils.isNotBlank(url))
				{
					pageParameters.add("url", url);
				}
				if (StringUtils.isNotBlank(regioCode))
				{
					pageParameters.add("regiocode", regioCode);
				}
				setResponsePage(ClientportaalDigidLoginPage.class, pageParameters);
			}

		}.setVisible(getIngelogdeClient() != null));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		if (getIngelogdeClient() != null)
		{
			Gemeente gbaGemeente = getIngelogdeClient().getPersoon().getGbaAdres().getGbaGemeente();
			if (gbaGemeente.getScreeningOrganisatie() != null && gbaGemeente.getScreeningOrganisatie().getRegioCode() != null)
			{
				String regioCode = gbaGemeente.getScreeningOrganisatie().getRegioCode();
				if (!regioCode.equals(ScreenitSession.get().getRegioCode()))
				{
					ScreenitSession.get().setRegioCode(regioCode);
				}
			}
		}
		addBreadCrumb();
	}

	private void addLogo()
	{
		Link<Void> logo = new Link<Void>("logo")
		{

			@Override
			public void onClick()
			{
				if (getIngelogdeClient() != null)
				{
					setResponsePage(ClientHomeDashboardPage.class);
				}
				else
				{
					String url = ScreenitSession.get().getAfkomstigURLRegioCode();
					if (url != null)
					{
						if (url.startsWith("http"))
						{
							throw new RedirectToUrlException(url);
						}
						else
						{
							throw new RedirectToUrlException("http://" + url);
						}
					}
				}
			}

		};
		add(logo);
	}

	private void addMenu()
	{
		add(new ListView<ClientHoofdMenuitem>("menu", getMenuItems())
		{

			@Override
			protected void populateItem(ListItem<ClientHoofdMenuitem> item)
			{
				if (item.getModelObject() == getHoofdMenuitem())
				{
					item.add(new AttributeAppender("class", new Model<String>("active")));
				}

				Link<ClientHoofdMenuitem> link = new Link<ClientHoofdMenuitem>("menuLink", item.getModel())
				{

					@Override
					public void onClick()
					{
						if (getIngelogdeClient() != null)
						{
							setResponsePage(getModelObject().getTargetClass());
						}
					}

				};
				link.add(new AttributeAppender("title", getString(EnumStringUtil.getPropertyString(item.getModelObject()) + ".title")));

				link.add(new Label("menuLabel", getString(EnumStringUtil.getPropertyString(item.getModelObject()))));

				item.add(link);
			}
		});
	}

	protected List<ClientHoofdMenuitem> getMenuItems()
	{
		List<ClientHoofdMenuitem> menuItems = new ArrayList<>(Arrays.asList(ClientHoofdMenuitem.values()));
		Client client = getIngelogdeClient();
		if (client != null && !Geslacht.VROUW.equals(client.getPersoon().getGeslacht()))
		{
			menuItems.remove(ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_CERVIX);
			menuItems.remove(ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA);
		}
		return menuItems;
	}

	private void addBreadCrumb()
	{
		WebMarkupContainer breadCrumbContainer = new WebMarkupContainer("breadCrumbContainer");
		breadCrumbContainer.setVisible(showBreadCrumb());
		add(breadCrumbContainer);

		Link<ClientHoofdMenuitem> link = new Link<ClientHoofdMenuitem>("hoofdLink", new PropertyModel<ClientHoofdMenuitem>(ClientBasePage.this, "hoofdMenuitem"))
		{

			@Override
			public void onClick()
			{
				setResponsePage(getModelObject().getTargetClass());
			}
		};
		breadCrumbContainer.add(link);
		link.add(new Label("hoofdLabel", new IModel<String>()
		{

			@Override
			public String getObject()
			{
				return getString(EnumStringUtil.getPropertyString(getHoofdMenuitem()));
			}
		}));

		breadCrumbContainer.add(new Label("currentPage", getPageName()));

		TransparentWebMarkupContainer body = new TransparentWebMarkupContainer("body");

		String regioCode = ScreenitSession.get().getRegioCode();

		if (regioCode == null)
		{
			ScreeningOrganisatie screeningOrganisatie = getScreeningsOrganistatie();
			if (screeningOrganisatie != null)
			{
				regioCode = screeningOrganisatie.getRegioCode();

				if (regioCode == null)
				{
					regioCode = "ZU";
				}
			}
		}
		if (regioCode != null)
		{
			body.add(new AttributeAppender("class", " regio-" + regioCode));
		}
		add(body);
	}

	protected boolean showBreadCrumb()
	{
		return !getHoofdMenuitem().getTargetClass().equals(ClientBasePage.this.getClass()) && getIngelogdeClient() != null;
	}

	public abstract IModel<String> getPageName();

	public abstract ClientHoofdMenuitem getHoofdMenuitem();

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{
		target.add(feedbackPanel);
	}

	@Override
	public ScreenitContext getContext()
	{
		return ScreenitContext.CLIENT;
	}

	public Client getIngelogdeClient()
	{
		if (ScreenitSession.get().getLoggedInAccount() instanceof Client)
		{
			return (Client) ScreenitSession.get().getLoggedInAccount();
		}
		else
		{
			return null;
		}
	}

	protected ScreeningOrganisatie getScreeningsOrganistatie()
	{
		if (getIngelogdeClient() != null)
		{
			return getIngelogdeClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		}
		return null;
	}

	protected String getSoWebsite()
	{
		ScreeningOrganisatie screeningOrganisatie = getScreeningsOrganistatie();
		String soWebsite = "";
		if (screeningOrganisatie != null)
		{
			soWebsite = screeningOrganisatie.getWebsite();
			if (soWebsite == null)
			{
				soWebsite = "";
			}
			else if (!soWebsite.toLowerCase().contains("http://"))
			{
				soWebsite = "http://" + soWebsite;
			}
		}
		return soWebsite;
	}
}
