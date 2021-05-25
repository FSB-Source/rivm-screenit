
package nl.rivm.screenit.main.web.gebruiker.algemeen.batch;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.BatchService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.batch.Trigger;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.commons.lang.BooleanUtils;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.UncategorizedJmsException;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class QuartzTriggerPanel extends BatchBvoFilterPanel
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(QuartzTriggerPanel.class);

	@SpringBean
	private BatchService batchService;

	private List<Trigger> triggers;

	private WebMarkupContainer container;

	public QuartzTriggerPanel(String id)
	{
		super(id);

		container = new WebMarkupContainer("container");
		container.setOutputMarkupId(true);
		addOrReplace(container);

		triggers = null;
		boolean error = false;

		try
		{
			triggers = batchService.getScheduledTriggers();
		}
		catch (UncategorizedJmsException e)
		{
			LOG.error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder", e);
			error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder");
			error = true;
		}

		if (triggers == null && !error)
		{
			error("Ophalen van triggers mislukt");
		}

		ListView<Trigger> triggerListView = new ListView<Trigger>("triggers", new IModel<List<Trigger>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<Trigger> getObject()
			{
				return getVisibleTriggers();
			}

		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<Trigger> item)
			{
				Trigger trigger = item.getModelObject();
				item.add(new Label("bvoLabel", Bevolkingsonderzoek.getAfkortingen(trigger.getJobType().getBevolkingsOnderzoeken())));
				item.add(new EnumLabel<JobType>("job", trigger.getJobType()));
				item.add(new Label("cron", trigger.getCronExpressie()));
				item.add(DateLabel.forDatePattern("nextFireTime", Model.of(trigger.getNextFireTime()), "dd-MM-yyyy HH:mm:ss"));

				item.add(new IndicatingAjaxLink<String>("verwijderen", new Model<>(trigger.getTriggerNaam()))
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						Boolean result = batchService.removeTrigger(getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
						if (BooleanUtils.isTrue(result))
						{
							Session.get().info("Trigger is verwijderd");
							setResponsePage(BatchTriggerPage.class);
						}
						else
						{
							error("Trigger verwijderen mislukt");
						}
					}
				}.setBody(new Model<>("Verwijder trigger")).setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BATCH_STATUS, Actie.VERWIJDEREN)));
			}
		};
		container.add(triggerListView);
		triggerListView.setOutputMarkupId(true);
	}

	protected List<Trigger> getVisibleTriggers()
	{
		List<Trigger> visibleTriggers = new ArrayList<>();
		if (triggers != null)
		{
			for (Trigger trigger : triggers)
			{
				boolean heeftBVO = false;
				for (Bevolkingsonderzoek bvo : Arrays.asList(trigger.getJobType().getBevolkingsOnderzoeken()))
				{
					if (getBatchJobZoekCriteria().getBevolkingsonderzoeken().contains(bvo) || getBatchJobZoekCriteria().getBevolkingsonderzoeken().isEmpty())
					{
						heeftBVO = true;
					}
				}
				if (heeftBVO)
				{
					visibleTriggers.add(trigger);
				}
			}
		}
		return visibleTriggers;
	}

	@Override
	protected void bvoFilterChanged(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
	{
		target.add(container);
	}
}
