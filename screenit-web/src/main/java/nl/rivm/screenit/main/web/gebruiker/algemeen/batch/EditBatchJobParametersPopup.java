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

import java.util.Arrays;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.EditOrganisatieParametersPanel;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class EditBatchJobParametersPopup extends GenericPanel<JobType>
{

	@SpringBean
	private InstellingService instellingService;

	public EditBatchJobParametersPopup(String id, JobType jobType)
	{
		super(id, Model.of(jobType));

		add(new EnumLabel<JobType>("jobType", jobType));
		add(new Label("bvos", Bevolkingsonderzoek.getAfkortingen(jobType.getBevolkingsOnderzoeken())));
		add(new EditOrganisatieParametersPanel("parameters", Arrays.asList(jobType.getJobParameters()), true)
		{

			@Override
			protected void addOpslaanButton(Form<Void> form)
			{
				EditBatchJobParametersPopup.this.add(new IndicatingAjaxSubmitLink("opslaan", form)
				{
					@Override
					protected void onSubmit(AjaxRequestTarget target)
					{
						super.onSubmit(target);
						instellingService.saveOrUpdateOrganisatieParameters(getAllParameters(), ScreenitSession.get().getLoggedInInstellingGebruiker());
						info("Parameters zijn opgeslagen");
						EditBatchJobParametersPopup.this.close(target);
					}
				});
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

}
