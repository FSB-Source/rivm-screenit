
package nl.rivm.screenit.main.web.gebruiker.algemeen.batch;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.AjaxLazyLoadPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.UncategorizedJmsException;

public class BatchTriggerPage extends BatchBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(BatchTriggerPage.class);

	private BootstrapDialog dialog;

	public BatchTriggerPage()
	{
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new AjaxLink<Void>("addTrigger")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new AddTriggerPanel(BootstrapDialog.CONTENT_ID));
			}
		}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BATCH_STATUS, Actie.TOEVOEGEN)));

		try
		{
			add(new AjaxLazyLoadPanel("triggerPanel")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public Component getLazyLoadComponent(String markupId)
				{
					return new QuartzTriggerPanel(markupId);
				}
			});
		}
		catch (UncategorizedJmsException e)
		{
			LOG.error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder", e);
			error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder");
		}
	}

}
