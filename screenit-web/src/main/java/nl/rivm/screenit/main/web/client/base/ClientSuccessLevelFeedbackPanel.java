
package nl.rivm.screenit.main.web.client.base;

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

import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.FeedbackPanel;
import org.apache.wicket.markup.html.panel.Panel;

public class ClientSuccessLevelFeedbackPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	private FeedbackPanel feedbackPanel;

	public ClientSuccessLevelFeedbackPanel(String id)
	{
		super(id);
		WebMarkupContainer container = new WebMarkupContainer("container");

		feedbackPanel = new FeedbackPanel("feedback", new IFeedbackMessageFilter()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public boolean accept(FeedbackMessage message)
			{
				return message.getLevel() == FeedbackMessage.SUCCESS;
			}
		});
		container.add(feedbackPanel);

		add(container);
	}

	@Override
	public boolean isVisible()
	{
		return feedbackPanel.anyMessage();
	}

}
