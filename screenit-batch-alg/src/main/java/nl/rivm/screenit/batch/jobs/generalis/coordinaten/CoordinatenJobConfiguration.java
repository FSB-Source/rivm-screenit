package nl.rivm.screenit.batch.jobs.generalis.coordinaten;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.gemeentecoordstep.GemeenteCoordinatenWriter;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodecoordstep.PostcodeCoordinatenWriter;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenClientKoppelReader;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenClientKoppelWriter;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenIntakeLocatieKoppelReader;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenIntakeLocatieKoppelWriter;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenStandplaatsKoppelReader;
import nl.rivm.screenit.batch.jobs.generalis.coordinaten.postcodekoppelstep.PostcodeCoordinatenStandplaatsKoppelWriter;
import nl.rivm.screenit.model.PostcodeNlProductCode;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@AllArgsConstructor
public class CoordinatenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job coordinatenJob(CoordinatenListener listener, Step postcodeCoordinatenStep, Step gemeenteCoordinatenStep, Step postcodeCoordinatenClientKoppelStep,
		Step postcodeCoordinatenIntakeLocatieKoppelStep, Step postcodeCoordinatenStandplaatsKoppelStep)
	{
		return jobBuilderFactory.get(JobType.COORDINATEN.name())
			.listener(listener)
			.start(postcodeCoordinatenStep)
			.next(gemeenteCoordinatenStep)
			.next(postcodeCoordinatenClientKoppelStep)
			.next(postcodeCoordinatenIntakeLocatieKoppelStep)
			.next(postcodeCoordinatenStandplaatsKoppelStep)
			.build();
	}

	@Bean
	public Step postcodeCoordinatenStep(CoordinatenListener listener, @Qualifier("postcodeCoordinatenReader") PostcodeNlDataReader reader, PostcodeCoordinatenWriter writer)
	{
		return stepBuilderFactory.get("postcodeCoordinatenStep")
			.transactionManager(transactionManager)
			.listener(listener)
			.<String, String> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step gemeenteCoordinatenStep(CoordinatenListener listener, @Qualifier("gemeenteCoordinatenReader") PostcodeNlDataReader reader, GemeenteCoordinatenWriter writer)
	{
		return stepBuilderFactory.get("gemeenteCoordinatenStep")
			.transactionManager(transactionManager)
			.listener(listener)
			.<String, String> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step postcodeCoordinatenClientKoppelStep(CoordinatenListener listener, PostcodeCoordinatenClientKoppelReader reader, PostcodeCoordinatenClientKoppelWriter writer)
	{
		return stepBuilderFactory.get("postcodeCoordinatenClientKoppelStep")
			.transactionManager(transactionManager)
			.listener(listener)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step postcodeCoordinatenIntakeLocatieKoppelStep(CoordinatenListener listener, PostcodeCoordinatenIntakeLocatieKoppelReader reader,
		PostcodeCoordinatenIntakeLocatieKoppelWriter writer)
	{
		return stepBuilderFactory.get("postcodeCoordinatenIntakeLocatieKoppelStep")
			.transactionManager(transactionManager)
			.listener(listener)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step postcodeCoordinatenStandplaatsKoppelStep(CoordinatenListener listener, PostcodeCoordinatenStandplaatsKoppelReader reader,
		PostcodeCoordinatenStandplaatsKoppelWriter writer)
	{
		return stepBuilderFactory.get("postcodeCoordinatenStandplaatsKoppelStep")
			.transactionManager(transactionManager)
			.listener(listener)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public PostcodeNlDataReader postcodeCoordinatenReader()
	{
		var reader = new PostcodeNlDataReader();
		reader.setProductCode(PostcodeNlProductCode.NUM.name());
		return reader;
	}

	@Bean
	public PostcodeNlDataReader gemeenteCoordinatenReader()
	{
		var reader = new PostcodeNlDataReader();
		reader.setProductCode(PostcodeNlProductCode.WPL.name());
		return reader;
	}

}
