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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobFlag;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.input.behavior.IndicatingAjaxFormComponentUpdatingBehavior;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class JobStartPanel extends BatchBvoFilterPanel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private JobService jobService;

	private WebMarkupContainer jobsContainer;

	private BootstrapDialog dialog;

	public JobStartPanel(String id)
	{
		super(id);

		dialog = new BootstrapDialog("startJobDialog");
		add(dialog);

		maakListView(null);

	}

	private void maakListView(AjaxRequestTarget target)
	{
		jobsContainer = new WebMarkupContainer("jobsContainer");
		jobsContainer.setOutputMarkupId(true);
		addOrReplace(jobsContainer);

		List<JobType> jobs = new ArrayList<>();

		for (JobType jobType : JobType.values())
		{
			Boolean heeftBVO = Boolean.FALSE;
			for (Bevolkingsonderzoek bvo : jobType.getBevolkingsOnderzoeken())
			{
				if (getBatchJobZoekCriteria().getBevolkingsonderzoeken().contains(bvo) || getBatchJobZoekCriteria().getBevolkingsonderzoeken().isEmpty())
				{
					heeftBVO = Boolean.TRUE;
				}
			}
			if (!jobType.hasJobFlag(JobFlag.BLOCK_MANUAL_START) && heeftBVO.equals(Boolean.TRUE))
			{
				jobs.add(jobType);
			}
		}
		boolean magToevoegen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BATCH_STATUS, Actie.TOEVOEGEN);
		jobsContainer.add(new WebMarkupContainer("startHeader").setVisible(magToevoegen));
		jobsContainer.add(new WebMarkupContainer("configHeader").setVisible(magToevoegen));
		jobsContainer.add(new PropertyListView<>("jobs", jobs)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<JobType> item)
			{
				JobType jobType = item.getModelObject();
				IModel<Boolean> selectieHerstart = Model.of(false);
				item.add(new Label("bvoLabel", Bevolkingsonderzoek.getAfkortingen(jobType.getBevolkingsOnderzoeken())));
				item.add(new EnumLabel<>("naam", jobType));
				item.add(new Label("beschrijving", getString(EnumStringUtil.getPropertyString(jobType) + ".beschrijving")));
				item.add(new ConfirmingIndicatingAjaxLink<>("start", item.getModel(), dialog, null)
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						BatchJob batchJob = new BatchJob();
						batchJob.setJobType(getModelObject());
						if (JobType.CLIENT_SELECTIE.equals(getModelObject()))
						{
							batchJob.getJobParameters().put(JobStartParameter.COLON_SELECTIE_HERSTART.name(), selectieHerstart.getObject());
						}
						jobService.startJob(batchJob, ScreenitSession.get().getLoggedInInstellingGebruiker());
					}

					@Override
					protected IModel<String> getHeaderStringModel()
					{
						JobType job = item.getModelObject();
						return Model.of(getJobName(job));
					}

					@Override
					protected IModel<String> getContentStringModel()
					{
						JobType job = item.getModelObject();
						String beginQuestion = "Weet u zeker dat u de batchjob ";
						String endQuestion = " wilt starten?";
						String naam = getJobName(job);
						return Model.of(beginQuestion + naam + endQuestion);
					}

					@Override
					protected Component createCustomComponent(String id)
					{
						if (JobType.CLIENT_SELECTIE.equals(getModelObject()))
						{
							return new CheckboxFragment(id, selectieHerstart, JobStartParameter.COLON_SELECTIE_HERSTART);
						}
						return super.createCustomComponent(id);
					}
				}.setVisible(magToevoegen));

				WebMarkupContainer configButtonContainer = new WebMarkupContainer("configButtonContainer");
				Component config = new IndicatingAjaxLink<JobType>("config")
				{

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						dialog.openWith(target, new EditBatchJobParametersPopup(IDialog.CONTENT_ID, jobType)
						{

							@Override
							protected void close(AjaxRequestTarget target)
							{
								dialog.close(target);
							}

						});
					}
				};
				config.setVisible(jobType.getJobParameters() != null);
				configButtonContainer.add(config);
				configButtonContainer.setVisible(magToevoegen);
				item.add(configButtonContainer);
			}
		});

	}

	@Override
	protected void bvoFilterChanged(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
	{
		maakListView(target);
		target.add(jobsContainer);
	}

	private class CheckboxFragment extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public CheckboxFragment(String id, IModel<Boolean> model, JobStartParameter jobStartParameter)
		{
			super(id, "checkboxFragment", JobStartPanel.this);

			add(new EnumLabel<>("parameterOmschrijving", jobStartParameter));
			CheckBox checkBox = ComponentHelper.newCheckBox("checkbox", model);

			checkBox.add(new IndicatingAjaxFormComponentUpdatingBehavior("click", checkBox)
			{
				@Override
				protected void onComponentUpdate(AjaxRequestTarget target)
				{

				}
			});

			add(checkBox);
		}
	}
}
