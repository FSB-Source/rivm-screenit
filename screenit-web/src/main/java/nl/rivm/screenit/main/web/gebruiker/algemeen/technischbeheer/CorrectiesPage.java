package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.BatchService;
import nl.rivm.screenit.main.service.CorrectieService;
import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.JobService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CorrectiesPage extends TechnischBeheerPage
{
	@SpringBean
	private JobService jobService;

	@SpringBean
	private CorrectieService correctieService;

	@SpringBean
	private BatchService batchService;

	@SpringBean
	private VerslagService verslagService;

	private IModel<String> berichtIds = new Model<>("");

	public CorrectiesPage()
	{
		add(new IndicatingAjaxLink<Void>("resumeClientSelectie")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				jobService.resumeClientSelectie();
				info("Client selectie DK is resumed");
			}
		});

		add(new IndicatingAjaxLink<Void>("opruimenGefaaldeBatches")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				correctieService.opruimenGefaaldeBatchJobs();
				info("Gefaalde batchjobs opgeruimd");
			}
		});

		add(new IndicatingAjaxLink<Void>("stopAllRunningBatches")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				batchService.getStopAllBatchJobs();
				info("Alle draaiende batchjobs gestopt.");
			}
		});

		ScreenitForm<Object> form = new ScreenitForm<>("form");
		add(form);

		form.add(new TextArea<>("berichtIds", berichtIds));
		form.add(new AjaxSubmitLink("berichtenNaarDK")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				berichtenOpieuwVerwerken(Bevolkingsonderzoek.COLON);
			}

		});
		form.add(new AjaxSubmitLink("berichtenNaarBMHK")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				berichtenOpieuwVerwerken(Bevolkingsonderzoek.CERVIX);
			}
		});

	}

	private void berichtenOpieuwVerwerken(Bevolkingsonderzoek bvo)
	{
		List<Long> ids = Arrays.stream(berichtIds.getObject().split(",")).map(Long::valueOf).collect(Collectors.toList());
		verslagService.berichtenOpnieuwVerwerken(ids, bvo);
		info(ids.size() + " berichten worden opgepakt door batch " + bvo.getAfkorting());
	}

}
