
package nl.rivm.screenit.main.web.gebruiker.algemeen.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.batch.BatchServerStatus;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.batch.Job;
import nl.rivm.screenit.model.batch.JobInstance;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.UncategorizedJmsException;

public class BatchStatusPanel extends BatchBvoFilterPanel
{

	private static final Logger LOG = LoggerFactory.getLogger(BatchStatusPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private BatchService batchService;

	private WebMarkupContainer container;

	private List<BatchServerStatus> batchServerStatussen;

	public BatchStatusPanel(String id)
	{
		super(id);
		container = new WebMarkupContainer("container");
		container.setOutputMarkupId(true);
		addOrReplace(container);
		boolean error = false;
		final List<JobType> jobQueue = new ArrayList<>();
		try
		{
			batchServerStatussen = batchService.getBatchServerStatus();
			jobQueue.addAll(batchService.getBatchQueue());
		}
		catch (UncategorizedJmsException e)
		{
			LOG.error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder", e);
			error("Er is een mogelijk probleem met ActiveMq, waarschuw een beheerder");
			error = true;
		}

		if (CollectionUtils.isEmpty(batchServerStatussen) && !error)
		{
			error("Ophalen van batchstatus mislukt");
		}

		container.add(new ListView<JobType>("jobs", new IModel<List<JobType>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<JobType> getObject()
			{
				return getVisibleJobs();
			}

		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<JobType> item)
			{
				item.add(new Label("bvoLabel", Bevolkingsonderzoek.getAfkortingen(item.getModelObject().getBevolkingsOnderzoeken())));
				item.add(new EnumLabel<JobType>("job", item.getModelObject()));
				boolean jobFound = false;
				if (batchServerStatussen != null)
				{
					top: for (BatchServerStatus status : batchServerStatussen)
					{
						for (Job job : status.getJobs())
						{
							JobType jobType = JobType.valueOf(job.getJobName().toUpperCase());
							if (item.getModelObject().equals(jobType))
							{
								item.add(new Label("server", status.getInstanceName()));
								item.add(new ListView<String>("status", bepaalStatus(job, jobType, jobQueue))
								{

									private static final long serialVersionUID = 1L;

									@Override
									protected void populateItem(ListItem<String> item)
									{
										item.add(new Label("tekst", item.getModelObject()));

									}

								});
								item.add(new ListView<JobInstance>("instances", job.getInstances())
								{

									private static final long serialVersionUID = 1L;

									@Override
									protected void populateItem(ListItem<JobInstance> item)
									{
										item.add(new Label("samenvatting", item.getModelObject().getSamenvatting()));
									}
								});
								jobFound = true;
								break top;
							}
						}
					}
				}
				if (!jobFound)
				{
					item.add(new Label("server", ""));
					item.add(new WebMarkupContainer("status").setVisible(false));
					item.add(new ListView<JobInstance>("instances", new ArrayList<JobInstance>())
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void populateItem(ListItem<JobInstance> item)
						{
							item.add(new Label("samenvatting", item.getModelObject().getSamenvatting()));
						}

					});
				}
			}

		});
	}

	protected IModel<List<String>> bepaalStatus(Job job, JobType jobType, List<JobType> jobQueue)
	{
		IModel<List<String>> status = new ListModel<>(new ArrayList<>());
		if (!job.getInstances().isEmpty())
		{
			status.getObject().add(getString("actief"));
		}
		for (int i = 0; i < jobQueue.size(); i++)
		{
			JobType type = jobQueue.get(i);
			if (jobType.equals(type))
			{
				status.getObject().add(new StringResourceModel("pending", this, Model.of()).setParameters(new Object[] { i + 1 }).getString());
			}
		}
		return status;
	}

	private List<JobType> getVisibleJobs()
	{
		List<JobType> jobs = new ArrayList<>();

		for (JobType jobType : JobType.values())
		{
			Boolean heeftBVO = Boolean.FALSE;
			for (Bevolkingsonderzoek bvo : Arrays.asList(jobType.getBevolkingsOnderzoeken()))
			{
				if (getBatchJobZoekCriteria().getBevolkingsonderzoeken().contains(bvo) || getBatchJobZoekCriteria().getBevolkingsonderzoeken().isEmpty())
				{
					heeftBVO = Boolean.TRUE;
				}
			}
			if (heeftBVO.equals(Boolean.TRUE))
			{
				jobs.add(jobType);
			}
		}
		return jobs;
	}

	@Override
	protected void bvoFilterChanged(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
	{
		target.add(container);
	}

}
