package nl.rivm.screenit.main.web.client.dashboard.mamma;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.service.BaseClientGebeurtenisService;
import nl.rivm.screenit.model.ClientGebeurtenis;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.client.dashboard.ClientDashboardPersoonsgegevensPanel;
import nl.rivm.screenit.main.web.client.dashboard.ClientHistoryPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.RestartResponseException;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class ClientMammaDashboardPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private BaseClientGebeurtenisService clientGebeurtenisService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	public ClientMammaDashboardPage()
	{
		Boolean toonVervangendeTekst = preferenceService.getBoolean(PreferenceKey.MAMMA_CLIENTPORTAAL_TOON_VERVANGENDE_TEKST.name(), Boolean.FALSE);
		if (Boolean.TRUE.equals(toonVervangendeTekst))
		{
			throw new RestartResponseException(ClientMammaDashboardVervangendeTekstPage.class);
		}
		hibernateService.reload(getIngelogdeClient());
		IModel<Client> client = ModelUtil.sModel(getIngelogdeClient());

		add(new ExternalLink("soWebsite", getSoWebsite()));
		add(new ClientDashboardPersoonsgegevensPanel("persoonsgegegevens", client, getHoofdMenuitem()));
		add(new ClientHistoryPanel("history", client)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected List<ClientGebeurtenis> getGebeurtenissen(IModel<Client> modelClient)
			{
				return clientGebeurtenisService.getClientMammaGebeurtenissen(ModelProxyHelper.deproxy(modelClient.getObject()));
			}

		});
		add(new ClientMammaAfspraakPanel("afspraak", client));
		add(new ClientMammaOverigeDashboardActiesPanel("overige", client));
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_MAMMA;
	}

	@Override
	public IModel<String> getPageName()
	{
		return new Model<>("");
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(OnDomReadyHeaderItem.forScript("showSuccessFeedbackMsg();"));
	}
}
