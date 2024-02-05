package nl.rivm.screenit.batch.jobs.generalis.gba;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.batch.jobs.generalis.gba.dossiers.DossierReader;
import nl.rivm.screenit.batch.jobs.generalis.gba.dossiers.DossierWriter;
import nl.rivm.screenit.batch.jobs.generalis.gba.upload105step.Vo105UploadTasklet;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.GbaVraagItemReader;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.Vo105ItemProcessor;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.Vo105ItemWriter;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.ClientItemWriter;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.IVo107Provider;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.VertrouwdVerbondenVo107Provider;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.Vo107ItemReader;
import nl.rivm.screenit.config.GbaConfig;
import nl.rivm.screenit.model.enums.JobType;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.listener.ExecutionContextPromotionListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
public class GbaJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job gbaJob(GbaListener listener, Step verwerkVo107Step, Step verwerkVo105Step, Step uploadVo105Step, Step dossiersAanmakenStep)
	{
		return jobBuilderFactory.get(JobType.GBA.name())
			.listener(listener)
			.start(verwerkVo107Step)
			.on("*").to(verwerkVo105Step)
			.from(verwerkVo107Step).on(ExitStatus.FAILED.getExitCode()).to(dossiersAanmakenStep)
			.from(verwerkVo105Step)
			.on("*").to(uploadVo105Step)
			.from(verwerkVo105Step).on(ExitStatus.FAILED.getExitCode()).to(dossiersAanmakenStep)
			.from(uploadVo105Step)
			.on("*").to(dossiersAanmakenStep)
			.from(uploadVo105Step).on(ExitStatus.FAILED.getExitCode()).to(dossiersAanmakenStep)
			.from(dossiersAanmakenStep)
			.end().build();
	}

	@Bean
	public Job gbaZonderVo105Job(GbaListener listener, Step verwerkVo107Step, Step dossiersAanmakenStep)
	{
		return jobBuilderFactory.get(JobType.GBA_ZONDER_VO105.name())
			.listener(listener)
			.start(verwerkVo107Step)
			.on("*").to(dossiersAanmakenStep)
			.from(verwerkVo107Step).on(ExitStatus.FAILED.getExitCode()).to(dossiersAanmakenStep)
			.from(dossiersAanmakenStep)
			.end().build();
	}

	@Bean
	public Step verwerkVo107Step(ExecutionContextPromotionListener gbaPromotionListener, Vo107ItemReader reader, ClientItemWriter writer)
	{
		return stepBuilderFactory.get("verwerkVo107Step")
			.transactionManager(transactionManager)
			.listener(gbaPromotionListener)
			.<Vo107Bericht, Vo107Bericht> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step verwerkVo105Step(ExecutionContextPromotionListener gbaPromotionListener, GbaVraagItemReader reader, Vo105ItemProcessor processor, Vo105ItemWriter writer)
	{
		return stepBuilderFactory.get("verwerkVo105Step")
			.transactionManager(transactionManager)
			.listener(gbaPromotionListener)
			.<Long, Vo105Bericht> chunk(10)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uploadVo105Step(Vo105UploadTasklet tasklet)
	{
		return stepBuilderFactory.get("uploadVo105Step")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public Step dossiersAanmakenStep(DossierReader reader, DossierWriter writer)
	{
		return stepBuilderFactory.get("dossiersAanmakenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	@Profile("!test")
	public IVo107Provider vo107Provider(GbaConfig gbaConfig)
	{
		return new VertrouwdVerbondenVo107Provider(gbaConfig);
	}

	@Bean
	public Vo107ItemReader vo107ItemReader(GbaConfig gbaConfig, IVo107Provider vo107Provider)
	{
		var reader = new Vo107ItemReader();
		reader.setVo107Provider(vo107Provider);
		reader.setVoFileStorePath(gbaConfig.voFileStorePath());
		return reader;
	}

	@Bean
	public ExecutionContextPromotionListener gbaPromotionListener()
	{
		var listener = new ExecutionContextPromotionListener();
		listener.setKeys(new String[] { "key.vo105bestand", "key.verwerkingslogid" });
		listener.setStatuses(new String[] { FlowExecutionStatus.COMPLETED.getName(), FlowExecutionStatus.FAILED.getName() });
		return listener;
	}

}
