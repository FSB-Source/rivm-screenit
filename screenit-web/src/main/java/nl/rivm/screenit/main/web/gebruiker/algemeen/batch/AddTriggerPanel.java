
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.BatchService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.batch.Trigger;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobFlag;
import nl.rivm.screenit.model.enums.JobType;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;
import org.quartz.CronExpression;

public class AddTriggerPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private BatchService batchService;

	public AddTriggerPanel(String id)
	{
		super(id);

		Form<Trigger> form = new Form<>("form", new CompoundPropertyModel<>(new Model<>(new Trigger())));
		add(form);

		List<JobType> jobtypes = new ArrayList<>();

		for (JobType jt : JobType.values())
		{
			if (!jt.hasJobFlag(JobFlag.BLOCK_MANUAL_START))
			{
				jobtypes.add(jt);
			}
		}

		form.add(new ScreenitDropdown<>("jobType", jobtypes, new EnumChoiceRenderer<JobType>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public Object getDisplayValue(JobType jobType)
			{
				Object displayValue = super.getDisplayValue(jobType);

				displayValue = displayValue.toString() + " (" + Bevolkingsonderzoek.getAfkortingen(jobType.getBevolkingsOnderzoeken()) + ")";

				return displayValue;
			}

		}).setRequired(true));
		form.add(new TextField<String>("cronExpressie").add(new IValidator<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void validate(IValidatable<String> validatable)
			{
				if (!CronExpression.isValidExpression(validatable.getValue()))
				{
					validatable.error(new ValidationError("Invalide cron expressie"));
				}
			}
		}).setRequired(true));

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Date firstScheduldedDate = batchService.addTrigger((Trigger) form.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (firstScheduldedDate == null)
				{
					error("Toevoegen van trigger mislukt");
				}
				else
				{
					Session.get()
						.info("Trigger toegevoegd, eerst volgende datum waarop de trigger afgaat: " + Constants.getDateTimeSecondsFormat().format(firstScheduldedDate));
					setResponsePage(BatchTriggerPage.class);
				}
			}

		});
	}
}
