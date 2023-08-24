package nl.rivm.screenit.main.web.error;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;
import nl.rivm.screenit.main.web.component.panels.ApplicatieInfoPanel;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.util.FoutmeldingsCodeUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Application;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.SubmitLink;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ErrorPage extends BasePage
{

	private static final Logger LOG = LoggerFactory.getLogger(ErrorPage.class);

	private final Model<String> errorId;

	@SpringBean(name = "applicationInstance")
	private String applicatieInstance;

	public ErrorPage()
	{
		this.errorId = Model.of(getNextErrorId());

		String gebruikerId = "";
		Account loggedInAccount = ScreenitSession.get().getLoggedInAccount();
		if (loggedInAccount != null)
		{
			if (loggedInAccount instanceof InstellingGebruiker)
			{
				gebruikerId = "IG";
			}
			else if (loggedInAccount instanceof Gebruiker)
			{
				gebruikerId = "G";
			}
			else if (loggedInAccount instanceof Client)
			{
				gebruikerId = "C";
			}
			gebruikerId += loggedInAccount.getId().toString();
		}

		String errorCode = errorId.getObject();
		LOG.error(String.format("Fout in applicatie (ErrorCode: %s, gebruiker: %s)", errorCode, gebruikerId));

	}

	protected void logFeedback(String feedback)
	{
		LOG.error(String.format("Feedback voor melding: %s - %s", errorId.getObject(), feedback));
	}

	protected IModel<String> getErrorId()
	{
		return errorId;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new Label("errorId", applicatieInstance + "_" + getErrorId().getObject()));

		add(new ApplicatieInfoPanel("applicatieInfo"));

		final IModel<String> feedback = Model.of("");

		Form<Void> form = new Form<Void>("feedbackForm")
		{
			@Override
			protected void onSubmit()
			{
				String feedbackObject = feedback.getObject();
				if (StringUtils.isNotBlank(feedbackObject))
				{
					logFeedback(feedbackObject);
				}

				Account loggedInAccount = ScreenitSession.get().getLoggedInAccount();
				if (loggedInAccount != null)
				{
					setResponsePage(Application.get().getHomePage());
				}
			}
		};
		add(form);

		form.add(new TextArea<String>("feedback", feedback));
		form.add(new SubmitLink("submit"));

	}

	@Override
	protected void configureResponse(WebResponse response)
	{
		super.configureResponse(response);
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}

	private static synchronized String getNextErrorId()
	{
		return FoutmeldingsCodeUtil.getFoutmeldingsCode("");
	}

	@Override
	public ScreenitContext getContext()
	{
		return null;
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{
	}
}
