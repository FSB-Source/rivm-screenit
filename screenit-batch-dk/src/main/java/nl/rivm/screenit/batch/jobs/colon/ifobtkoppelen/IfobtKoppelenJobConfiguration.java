package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.colon.KoppelPromotionListener;
import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelstep.IFobtKoppelReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelstep.IFobtKoppelWriter;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.service.impl.WebserviceInpakcentrumOpzettenServiceImpl;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import generated.KOPPELDATA;

@Configuration
public class IfobtKoppelenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job koppeldataVerwerkingJob(IfobtKoppelenListener listener, KoppelPromotionListener koppelPromotionListener, Step koppelenStep)
	{
		return jobBuilderFactory.get(JobType.KOPPELDATA_VERWERKING.name())
			.listener(listener)
			.listener(koppelPromotionListener)
			.start(koppelenStep)
			.build();
	}

	@Bean
	public Step koppelenStep(IFobtKoppelReader reader, IFobtKoppelWriter writer)
	{
		return stepBuilderFactory.get("koppelenStep")
			.transactionManager(transactionManager)
			.<KOPPELDATA.VERZONDENUITNODIGING, KOPPELDATA.VERZONDENUITNODIGING> chunk(250)
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
