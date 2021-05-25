
package nl.rivm.screenit.main.web.client.digid;

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

import javax.servlet.http.HttpSession;

import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientLoginFoutPage;
import nl.topicuszorg.digid.web.digidloginpage.DigidLoginPage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.wicket.Page;
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ScreenitDigidLoginPage extends DigidLoginPage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ScreenitDigidLoginPage.class);

	@SpringBean(name = "portaalUrl")
	private String portaalUrl;

	public ScreenitDigidLoginPage()
	{
		super();
		LOG.info("Pagina " + this.getClass().getName() + " wordt geladen");
	}

	public ScreenitDigidLoginPage(PageParameters parameters)
	{
		super(parameters);
		LOG.info("Pagina " + this.getClass().getName() + " wordt geladen");
	}

	@Override
	protected String getDigiDPageUrl()
	{
		return portaalUrl + "/" + ScreenitApplication.DIGID_MOUNT;
	}

	@Override
	protected Page createErrorResponsePage(String error)
	{
		ScreenitSession.get();
		Request request = RequestCycle.get().getRequest();
		HttpSession httpSession = ((ServletWebRequest) request).getContainerRequest().getSession();
		httpSession.setMaxInactiveInterval(15 * 60);
		return new ClientLoginFoutPage(error);
	}

}
