package nl.rivm.screenit.batch.jobs.colon.selectie;

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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep.ColonVerlopenRondesReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.afrondenstep.ColonVerlopenRondesWriter;
import nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep.UitnodigingPushMaxLeeftijdDecider;
import nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep.UitnodigingU1PushMaxLeeftijdReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep.UitnodigingU2PushMaxLeeftijdReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.projectinterval.ColonProjectIntervalToepassenReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.projectinterval.ColonProjectIntervalToepassenWriter;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ClientSelectieItemReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ClientSelectieMetCapaciteitItemReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.uitnodingingpushstep.UitnodigingU1PushProjectReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.uitnodingingpushstep.UitnodigingU2PushProjectReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.vooraankondigingnavervolgonderzoek.ColonVooraankondigingNaVervolgonderzoekReader;
import nl.rivm.screenit.batch.jobs.colon.selectie.vooraankondigingnavervolgonderzoek.ColonVooraankondigingNaVervolgonderzoekWriter;
import nl.rivm.screenit.batch.jobs.preselectie.ClientPreSelectieTasklet;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.FlowBuilder;
import org.springframework.batch.core.job.flow.Flow;
import org.springframework.batch.core.job.flow.support.SimpleFlow;
import org.springframework.batch.core.listener.ExecutionContextPromotionListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class SelectieJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job selectieJob(SelectieListener listener, Flow mainFlow, Flow selectieSplitFlow)
	{
		return jobBuilderFactory.get(JobType.CLIENT_SELECTIE.name())
			.listener(listener)
			.start(mainFlow)
			.next(selectieSplitFlow)
			.end().build();
	}

	@Bean
	public Flow mainFlow(Step preSelectieStep, Flow uitnodigingU1PushMaxLeeftijdFlow, Flow uitnodigingU2PushMaxLeeftijdFlow, Step uitnodigingU1PushStep, Step uitnodigingU2PushStep,
		Step afrondenStep, Step projectIntervalToepassenStep, Step vooraankondigingNaVervolgonderzoekStep)
	{
		return new FlowBuilder<SimpleFlow>("mainFlow")
			.start(preSelectieStep)
			.next(projectIntervalToepassenStep)
			.next(uitnodigingU1PushMaxLeeftijdFlow)
			.next(uitnodigingU2PushMaxLeeftijdFlow)
			.next(uitnodigingU1PushStep)
			.next(uitnodigingU2PushStep)
			.next(vooraankondigingNaVervolgonderzoekStep)
			.next(afrondenStep)
			.build();
	}

	@Bean
	public Flow selectieSplitFlow(TaskExecutor threadTaskExecutor, Flow selectieGebiedFlow, Flow selectieFlow)
	{
		return new FlowBuilder<SimpleFlow>("selectieSplitFlow")
			.split(threadTaskExecutor)
			.add(selectieGebiedFlow, selectieFlow)
			.build();
	}

	@Bean
	public Step preSelectieStep(ClientPreSelectieTasklet tasklet)
	{
		return stepBuilderFactory.get("preSelectieStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public Step uitnodigingU1PushStep(UitnodigingU1PushProjectReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("uitnodigingU1PushStep")
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uitnodigingU2PushStep(UitnodigingU2PushProjectReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("uitnodigingU2PushStep")
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step vooraankondigingNaVervolgonderzoekStep(ColonVooraankondigingNaVervolgonderzoekReader reader, ColonVooraankondigingNaVervolgonderzoekWriter writer)
	{
		return stepBuilderFactory.get("vooraankondigingNaVervolgonderzoekStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step afrondenStep(ColonVerlopenRondesReader reader, ColonVerlopenRondesWriter writer)
	{
		return stepBuilderFactory.get("afrondenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step projectIntervalToepassenStep(ColonProjectIntervalToepassenReader reader, ColonProjectIntervalToepassenWriter writer)
	{
		return stepBuilderFactory.get("projectIntervalToepassenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow selectieGebiedFlow(Step selectieGebiedStep)
	{
		return new FlowBuilder<SimpleFlow>("selectieGebiedFlow")
			.start(selectieGebiedStep)
			.build();
	}

	@Bean
	public Flow selectieFlow(Step selectieStep)
	{
		return new FlowBuilder<SimpleFlow>("selectieFlow")
			.start(selectieStep)
			.build();
	}

	@Bean
	public Step selectieGebiedStep(ExecutionContextPromotionListener selectiePromotionListener, ClientSelectieMetCapaciteitItemReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("selectieGebiedStep")
			.listener(selectiePromotionListener)
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step selectieStep(ExecutionContextPromotionListener selectiePromotionListener, ClientSelectieItemReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("selectieStep")
			.listener(selectiePromotionListener)
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public ClientPreSelectieTasklet preSelectieTasklet()
	{
		var tasklet = new ClientPreSelectieTasklet();
		tasklet.setBvo(Bevolkingsonderzoek.COLON);
		return tasklet;
	}

	@Bean
	public Flow uitnodigingU1PushMaxLeeftijdFlow(Step dummyStep, UitnodigingPushMaxLeeftijdDecider decider, Step uitnodigingU1PushMaxLeeftijdStep)
	{
		return new FlowBuilder<SimpleFlow>("uitnodigingU1PushMaxLeeftijdSFlow")
			.start(dummyStep)
			.next(decider)
			.on(ExitStatus.FAILED.getExitCode()).to(uitnodigingU1PushMaxLeeftijdStep)
			.from(decider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.from(uitnodigingU1PushMaxLeeftijdStep).on("*").to(decider).build();
	}

	@Bean
	public Step uitnodigingU1PushMaxLeeftijdStep(UitnodigingU1PushMaxLeeftijdReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("uitnodigingU1PushMaxLeeftijdStep")
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Flow uitnodigingU2PushMaxLeeftijdFlow(Step dummyStep, UitnodigingPushMaxLeeftijdDecider decider, Step uitnodigingU2PushMaxLeeftijdStep)
	{
		return new FlowBuilder<SimpleFlow>("uitnodigingU2PushMaxLeeftijdSFlow")
			.start(dummyStep)
			.next(decider)
			.on(ExitStatus.FAILED.getExitCode()).to(uitnodigingU2PushMaxLeeftijdStep)
			.from(decider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.from(uitnodigingU2PushMaxLeeftijdStep).on("*").to(decider).build();
	}

	@Bean
	public Step uitnodigingU2PushMaxLeeftijdStep(UitnodigingU2PushMaxLeeftijdReader reader, ClientSelectieItemWriter writer)
	{
		return stepBuilderFactory.get("uitnodigingU2PushMaxLeeftijdStep")
			.transactionManager(transactionManager)
			.<ClientCategorieEntry, ClientCategorieEntry> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public ExecutionContextPromotionListener selectiePromotionListener()
	{
		var listener = new ExecutionContextPromotionListener();
		listener.setKeys(new String[] { "key.selectierapportage" });
		return listener;
	}
}
