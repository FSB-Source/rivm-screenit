package nl.rivm.screenit.main.web.client.dashboard;

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

import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class ClientBaseDashboardVervangendeTekstPage extends ClientBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	public ClientBaseDashboardVervangendeTekstPage(Bevolkingsonderzoek bvo)
	{
		hibernateService.reload(getIngelogdeClient());
		IModel<Client> client = ModelUtil.ccModel(getIngelogdeClient());

		add(new ExternalLink("soWebsite", getSoWebsite()));
		add(new ClientDashboardPersoonsgegevensPanel("persoonsgegegevens", client, getHoofdMenuitem()));
		add(new Label("bvo", bvo.getNaam().toLowerCase()));
		add(new Label("bvo1", bvo.getNaam().toLowerCase()));
		String vervangendeTekst = preferenceService.getString(bvo + "_CLIENTPORTAAL_VERVANGENDE_TEKST").replaceAll("\n", "<br>");
		add(new Label("algemene-info", vervangendeTekst).setEscapeModelStrings(false));
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

	@Override
	protected boolean showBreadCrumb()
	{
		return false;
	}
}
