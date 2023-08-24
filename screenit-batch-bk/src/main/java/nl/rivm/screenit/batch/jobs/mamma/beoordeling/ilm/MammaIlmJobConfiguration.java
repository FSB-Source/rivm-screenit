package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.ilm.applicatielogging.IlmApplicatieLoggingVerwijderenReader;
import nl.rivm.screenit.batch.jobs.ilm.applicatielogging.IlmApplicatieLoggingVerwijderenWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.MammaIlmDecider;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.MammaBeeldenVerwijderenRetryTasklet;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.bezwaar.MammaBeeldenStatusBezwaarSignalerenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.bezwaar.MammaBeeldenStatusBezwaarSignalerenWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig.MammaBeeldenGunstigReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.gunstig.MammaBeeldenGunstigWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.overig.MammaBeeldenStatusOverigReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.overig.MammaBeeldenStatusOverigWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.signaleren.MammaBeeldenStatusSignalerenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.signaleren.MammaBeeldenStatusSignalerenWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.upload.MammaBeeldenStatusUploadSignalerenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.beelden.upload.MammaBeeldenStatusUploadSignalerenWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.palga.MammaPalgaImportVerslagenVerwijderenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.palga.MammaPalgaImportVerslagenVerwijderenWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.rondes.MammaScreeningRondesVerwijderenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.rondes.MammaScreeningRondesVerwijderenWriter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.flow.JobExecutionDecider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaIlmJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job ilmJob(MammaIlmJobListener listener, Step dummyStep, JobExecutionDecider beeldenStatusSignalerenDecider, Step beeldenSignalerenStep,
		Step uploadBeeldenSignalerenStep,
		Step bezwaarBeeldenSignalerenStep, Step gunstigeBeeldenVerwijderenStep, Step overigeBeeldenVerwijderenStep, Step palgaVerslagenVerwijderenStep, Step rondesVerwijderenStep,
		Step beeldenVerwijderenOpnieuwProberenStep, Step ilmApplicatieLoggingVerwijderenStep, JobExecutionDecider gunstigeBeeldenVerwijderenDecider,
		JobExecutionDecider overigeBeeldenVerwijderenDecider, JobExecutionDecider palgaVerslagenVerwijderenDecider, JobExecutionDecider ilmApplicatieLoggingVerwijderenDecider,
		JobExecutionDecider rondesVerwijderenDecider)
	{
		return jobBuilderFactory.get(JobType.MAMMA_ILM.name())
			.listener(listener)
			.start(dummyStep)

			.next(beeldenStatusSignalerenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(beeldenSignalerenStep)
			.from(beeldenStatusSignalerenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(gunstigeBeeldenVerwijderenDecider)
			.from(beeldenSignalerenStep)
			.on("*").to(uploadBeeldenSignalerenStep)

			.from(uploadBeeldenSignalerenStep)
			.on("*").to(bezwaarBeeldenSignalerenStep)

			.from(bezwaarBeeldenSignalerenStep)
			.on("*").to(beeldenVerwijderenOpnieuwProberenStep)

			.from(beeldenVerwijderenOpnieuwProberenStep)
			.on("*").to(gunstigeBeeldenVerwijderenDecider)

			.from(gunstigeBeeldenVerwijderenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(gunstigeBeeldenVerwijderenStep)
			.from(gunstigeBeeldenVerwijderenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(overigeBeeldenVerwijderenDecider)
			.from(gunstigeBeeldenVerwijderenStep)
			.on("*").to(overigeBeeldenVerwijderenDecider)

			.from(overigeBeeldenVerwijderenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(overigeBeeldenVerwijderenStep)
			.from(overigeBeeldenVerwijderenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(palgaVerslagenVerwijderenDecider)
			.from(overigeBeeldenVerwijderenStep)
			.on("*").to(palgaVerslagenVerwijderenDecider)

			.from(palgaVerslagenVerwijderenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(palgaVerslagenVerwijderenStep)
			.from(palgaVerslagenVerwijderenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(ilmApplicatieLoggingVerwijderenDecider)
			.from(palgaVerslagenVerwijderenStep)
			.on("*").to(ilmApplicatieLoggingVerwijderenDecider)

			.from(ilmApplicatieLoggingVerwijderenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(ilmApplicatieLoggingVerwijderenStep)
			.from(ilmApplicatieLoggingVerwijderenDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(rondesVerwijderenDecider)
			.from(ilmApplicatieLoggingVerwijderenStep)
			.on("*").to(rondesVerwijderenDecider)

			.from(rondesVerwijderenDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).to(rondesVerwijderenStep)
			.from(rondesVerwijderenDecider)
			.on(ExitStatus.FAILED.getExitCode()).end()
			.from(rondesVerwijderenStep)
			.on("*").to(rondesVerwijderenDecider)
			.from(rondesVerwijderenStep).on(ExitStatus.FAILED.getExitCode()).end()
			.end().build();
	}

	@Bean
	public Step beeldenSignalerenStep(MammaBeeldenStatusSignalerenReader reader, MammaBeeldenStatusSignalerenWriter writer)
	{
		return stepBuilderFactory.get("beeldenSignalerenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uploadBeeldenSignalerenStep(MammaBeeldenStatusUploadSignalerenReader reader, MammaBeeldenStatusUploadSignalerenWriter writer)
	{
		return stepBuilderFactory.get("uploadBeeldenSignalerenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step bezwaarBeeldenSignalerenStep(MammaBeeldenStatusBezwaarSignalerenReader reader, MammaBeeldenStatusBezwaarSignalerenWriter writer)
	{
		return stepBuilderFactory.get("bezwaarBeeldenSignalerenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step gunstigeBeeldenVerwijderenStep(MammaBeeldenGunstigReader reader, MammaBeeldenGunstigWriter writer)
	{
		return stepBuilderFactory.get("gunstigeBeeldenVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step overigeBeeldenVerwijderenStep(MammaBeeldenStatusOverigReader reader, MammaBeeldenStatusOverigWriter writer)
	{
		return stepBuilderFactory.get("overigeBeeldenVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step palgaVerslagenVerwijderenStep(MammaPalgaImportVerslagenVerwijderenReader reader, MammaPalgaImportVerslagenVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("palgaVerslagenVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.faultTolerant()
			.noRollback(Exception.class)
			.build();
	}

	@Bean
	public Step ilmApplicatieLoggingVerwijderenStep(IlmApplicatieLoggingVerwijderenReader reader, IlmApplicatieLoggingVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("ilmApplicatieLoggingVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step rondesVerwijderenStep(MammaScreeningRondesVerwijderenReader reader, MammaScreeningRondesVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("rondesVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(MammaIlmJobListener.MAX_AANTAL_RONDES_VERWERKEN_IN_STEP)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step beeldenVerwijderenOpnieuwProberenStep(MammaBeeldenVerwijderenRetryTasklet tasklet)
	{
		return stepBuilderFactory.get("beeldenVerwijderenOpnieuwProberenStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public IlmApplicatieLoggingVerwijderenReader ilmApplicatieLoggingVerwijderenReader()
	{
		var ilmApplicatieLoggingVerwijderenReader = new IlmApplicatieLoggingVerwijderenReader();
		ilmApplicatieLoggingVerwijderenReader.setBvo(Bevolkingsonderzoek.MAMMA);
		return ilmApplicatieLoggingVerwijderenReader;
	}

	@Bean
	public JobExecutionDecider beeldenStatusSignalerenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_BEELDEN_STATUS_SIGNALEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(false);
		return decider;
	}

	@Bean
	public JobExecutionDecider gunstigeBeeldenVerwijderenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_GUNSTIGE_BEELDEN_VERWIJDEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(false);
		return decider;
	}

	@Bean
	public JobExecutionDecider overigeBeeldenVerwijderenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_OVERIGE_BEELDEN_VERWIJDEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(false);
		return decider;
	}

	@Bean
	public JobExecutionDecider palgaVerslagenVerwijderenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_PALGA_IMPORT_VERSLAGEN_VERWIJDEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(false);
		return decider;
	}

	@Bean
	public JobExecutionDecider ilmApplicatieLoggingVerwijderenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_APPLICATIE_LOGGING_VERWIJDEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(false);
		return decider;
	}

	@Bean
	public JobExecutionDecider rondesVerwijderenDecider()
	{
		var decider = new MammaIlmDecider();
		decider.setStepKey(OrganisatieParameterKey.MAMMA_ILM_RONDES_VERWIJDEREN_UITVOEREN.name());
		decider.setObservesTimeLimit(true);
		return decider;
	}

}
