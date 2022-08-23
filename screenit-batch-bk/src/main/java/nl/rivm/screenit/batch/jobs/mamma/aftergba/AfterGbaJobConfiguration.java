package nl.rivm.screenit.batch.jobs.mamma.aftergba;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.mamma.aftergba.adresgewijzigdmarkerreset.MammaAdresGewijzigdMarkerResetReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.adresgewijzigdmarkerreset.MammaAdresGewijzigdMarkerResetWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenAnnuleren.MammaAfsprakenAnnulerenReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenAnnuleren.MammaAfsprakenAnnulerenWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamekansenherzien.MammaDeelnamekansenHerzienReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamekansenherzien.MammaDeelnamekansenHerzienWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamemodus.MammaDeelnamemodusReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamemodus.MammaDeelnamemodusWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.imswijzigingendoorsturen.MammaImsWijzigingenDoorsturenReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.imswijzigingendoorsturen.MammaImsWijzigingenDoorsturenWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.nieuwepostcodes.MammaNieuwePostcodesReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.nieuwepostcodes.MammaNieuwePostcodesWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.ontkoppelentehuis.MammaOntkoppelenTehuisReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.ontkoppelentehuis.MammaOntkoppelenTehuisWriter;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.zonderpostcode.MammaZonderPostcodeReader;
import nl.rivm.screenit.batch.jobs.mamma.aftergba.zonderpostcode.MammaZonderPostcodeWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AfterGbaJobConfiguration extends AbstractJobConfiguration
{

	public static final int AFTER_GBA_JOB_READER_FETCH_SIZE = 50;

	@Bean
	public Job afterGbaJob(AfterGbaListener listener, Step deelnamemodusBijwerkenStep, Step ontkoppelenTehuisStep, Step nieuwePostcodesStep, Step zonderPostcodesStep,
		Step imsWijzigingenStep, Step afsprakenAnnulerenStep, Step deelnamekansenHerzienStep, Step adresGewijzigdMarkerResetStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_NA_GBA.name())
			.listener(listener)
			.start(deelnamemodusBijwerkenStep)
			.on("*").to(ontkoppelenTehuisStep)
			.from(deelnamemodusBijwerkenStep).on(ExitStatus.FAILED.getExitCode()).to(ontkoppelenTehuisStep)
			.from(ontkoppelenTehuisStep).on("*").to(nieuwePostcodesStep)
			.from(ontkoppelenTehuisStep).on(ExitStatus.FAILED.getExitCode()).to(nieuwePostcodesStep)
			.from(nieuwePostcodesStep).on("*").to(zonderPostcodesStep)
			.from(nieuwePostcodesStep).on(ExitStatus.FAILED.getExitCode()).to(zonderPostcodesStep)
			.from(zonderPostcodesStep).on("*").to(imsWijzigingenStep)
			.from(zonderPostcodesStep).on(ExitStatus.FAILED.getExitCode()).to(imsWijzigingenStep)
			.from(imsWijzigingenStep).on("*").to(afsprakenAnnulerenStep)
			.from(imsWijzigingenStep).on(ExitStatus.FAILED.getExitCode()).to(afsprakenAnnulerenStep)
			.from(afsprakenAnnulerenStep).next(deelnamekansenHerzienStep)
			.next(adresGewijzigdMarkerResetStep)
			.end().build();
	}

	@Bean
	public Step deelnamemodusBijwerkenStep(MammaDeelnamemodusReader reader, MammaDeelnamemodusWriter writer)
	{
		return stepBuilderFactory.get("deelnamemodusBijwerkenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step ontkoppelenTehuisStep(MammaOntkoppelenTehuisReader reader, MammaOntkoppelenTehuisWriter writer)
	{
		return stepBuilderFactory.get("ontkoppelenTehuisStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step nieuwePostcodesStep(MammaNieuwePostcodesReader reader, MammaNieuwePostcodesWriter writer)
	{
		return stepBuilderFactory.get("nieuwePostcodesStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step zonderPostcodesStep(MammaZonderPostcodeReader reader, MammaZonderPostcodeWriter writer)
	{
		return stepBuilderFactory.get("zonderPostcodesStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step imsWijzigingenStep(MammaImsWijzigingenDoorsturenReader reader, MammaImsWijzigingenDoorsturenWriter writer)
	{
		return stepBuilderFactory.get("imsWijzigingenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step afsprakenAnnulerenStep(MammaAfsprakenAnnulerenReader reader, MammaAfsprakenAnnulerenWriter writer)
	{
		return stepBuilderFactory.get("afsprakenAnnulerenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step deelnamekansenHerzienStep(MammaDeelnamekansenHerzienReader reader, MammaDeelnamekansenHerzienWriter writer)
	{
		return stepBuilderFactory.get("deelnamekansenHerzienStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step adresGewijzigdMarkerResetStep(MammaAdresGewijzigdMarkerResetReader reader, MammaAdresGewijzigdMarkerResetWriter writer)
	{
		return stepBuilderFactory.get("adresGewijzigdMarkerResetStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
