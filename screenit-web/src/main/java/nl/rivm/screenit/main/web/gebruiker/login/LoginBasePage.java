
package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;
import nl.rivm.screenit.main.web.component.panels.ApplicatieInfoPanel;
import nl.rivm.screenit.main.web.component.panels.ResponsibleDisclosurePanel;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;

public abstract class LoginBasePage extends BasePage
{
	public static final String PAGE_PARAMETER_UITWISSELPORTAAL = "up";

	private final Panel feedbackPanel;

	public LoginBasePage()
	{
		add(new ResponsibleDisclosurePanel("responsibleDisclosure"));
		add(new ApplicatieInfoPanel("applicatieInfo"));
		feedbackPanel = new BootstrapFeedbackPanel("feedback");
		add(feedbackPanel);
		add(new Link<Void>("home")
		{

			@Override
			public void onClick()
			{
				goToBeginScherm();
			}

		});
		add(new Link<Void>("home1")
		{

			@Override
			public void onClick()
			{
				goToBeginScherm();
			}

		});
	}

	protected void goToBeginScherm()
	{
		ScreenitSession.get().getZorgIdSession().clear();
		ScreenitSession.get().setUziPasTokenAfgekeurd(false);
		setResponsePage(new MedewerkerLoginMethodPage(false));
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{
		target.add(feedbackPanel);
	}

	@Override
	public ScreenitContext getContext()
	{
		return ScreenitContext.GEBRUIKER;
	}
}
