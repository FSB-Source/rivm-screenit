package nl.rivm.screenit.batch.jobs.colon.selectie.projectinterval;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.time.LocalDate;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.math.NumberUtils;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class ColonProjectIntervalToepassenWriter extends BaseWriter<ProjectClient>
{

	private final HibernateService hibernateService;

	private final ProjectService projectService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(ProjectClient projectClient)
	{
		LOG.info("gevonden (project) client: Interval wordt voor PCID:{} CID:{} gecorrigeerd", projectClient.getId(), projectClient.getClient().getId());

		String parameter = ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_AFWIJKING_UITNODIGINGSINTERVAL);
		int afwijking = NumberUtils.toInt(parameter, Integer.MAX_VALUE);
		LocalDate projectPeildatum = null;
		var volgendeUitnodiging = projectClient.getClient().getColonDossier().getVolgendeUitnodiging();
		var ouderProjectPeildatum = volgendeUitnodiging.getProjectPeildatum();
		if (ProjectUtil.isClientActiefInProject(projectClient, currentDateSupplier.getDate()) && afwijking != Integer.MAX_VALUE)
		{
			projectPeildatum = DateUtil.toLocalDate(volgendeUitnodiging.getPeildatum()).plusYears(afwijking);
		}
		if (!Objects.equals(projectPeildatum, ouderProjectPeildatum))
		{
			volgendeUitnodiging.setProjectPeildatum(projectPeildatum);
			hibernateService.saveOrUpdate(volgendeUitnodiging);
		}
	}
}
