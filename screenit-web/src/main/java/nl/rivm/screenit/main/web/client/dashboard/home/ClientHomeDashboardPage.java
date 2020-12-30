package nl.rivm.screenit.main.web.client.dashboard.home;

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

import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.client.dashboard.ClientDashboardPersoonsgegevensPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ClientHomeDashboardPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	public ClientHomeDashboardPage()
	{
		hibernateService.reload(getIngelogdeClient());
		IModel<Client> client = ModelUtil.sModel(getIngelogdeClient());
		String soNaam = "";

		ScreeningOrganisatie screeningOrganisatie = getScreeningsOrganistatie();
		if (screeningOrganisatie != null)
		{
			soNaam = screeningOrganisatie.getNaam();
			if (soNaam == null)
			{
				soNaam = "Screeningsorganisatie onbekend";
			}
		}

		add(new Label("soNaam", soNaam));
		add(new ClientDashboardPersoonsgegevensPanel("persoonsgegegevens", client, getHoofdMenuitem()));
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK;
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
