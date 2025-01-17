package nl.rivm.screenit.batch.jobs.cervix.zaskoppelen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.koppelstep.ZasKoppelReader;
import nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.koppelstep.ZasKoppelWriter;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.service.impl.WebserviceInpakcentrumOpzettenServiceImpl;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.listener.ExecutionContextPromotionListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import generated.KOPPELDATA.VERZONDENUITNODIGING;

@Configuration
public class ZasKoppelenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job zasKoppelenJob(ZasKoppelenListener listener, ExecutionContextPromotionListener koppelPromotionListener, Step koppelenStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_KOPPELDATA_VERWERKING.name())
			.listener(listener)
			.listener(koppelPromotionListener)
			.start(koppelenStep)
			.build();
	}

	@Bean
	public ExecutionContextPromotionListener koppelPromotionListener()
	{
		var listener = new ExecutionContextPromotionListener();
		listener.setKeys(new String[] { "koppelXMLData" });
		listener.setStatuses(new String[] { ExitStatus.COMPLETED.getExitCode(), ExitStatus.FAILED.getExitCode() });
		return listener;
	}

	@Bean
	public Step koppelenStep(ZasKoppelReader reader, ZasKoppelWriter writer)
	{
		return stepBuilderFactory.get("koppelenStep")
			.transactionManager(transactionManager)
			.<VERZONDENUITNODIGING, VERZONDENUITNODIGING> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public WebserviceInpakcentrumOpzettenService webserviceInpakcentrumOpzettenService()
	{
		return new WebserviceInpakcentrumOpzettenServiceImpl();
	}

	@Bean
	public ScreenITLoggingSaver screenITLoggingSaver()
	{
		return new ScreenITLoggingSaver();
	}

}
