package nl.rivm.screenit.batch.base.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import javax.sql.DataSource;

import nl.rivm.screenit.batch.ImprovedSimpleEhCacheInterceptor;

import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.JobRegistry;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.configuration.support.JobRegistryBeanPostProcessor;
import org.springframework.batch.core.configuration.support.MapJobRegistry;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.explore.support.JobExplorerFactoryBean;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.batch.core.launch.support.SimpleJobOperator;
import org.springframework.batch.core.repository.ExecutionContextSerializer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.dao.Jackson2ExecutionContextStringSerializer;
import org.springframework.batch.core.repository.support.JobRepositoryFactoryBean;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.lob.DefaultLobHandler;
import org.springframework.jdbc.support.lob.LobHandler;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

@Configuration
public class BatchConfig
{

	@Bean
	public ImprovedSimpleEhCacheInterceptor cacheInterceptor()
	{
		return new ImprovedSimpleEhCacheInterceptor();
	}

	@Bean
	public JobBuilderFactory jobBuilderFactory(JobRepository jobRepository)
	{
		return new JobBuilderFactory(jobRepository);
	}

	@Bean
	public StepBuilderFactory stepBuilderFactory(JobRepository jobRepository, HibernateTransactionManager transactionManager)
	{
		return new StepBuilderFactory(jobRepository, transactionManager);
	}

	@Bean
	public TaskExecutor taskExecutor()
	{
		return new SyncTaskExecutor();
	}

	@Bean
	public TaskExecutor threadTaskExecutor()
	{
		var taskExecutor = new ThreadPoolTaskExecutor();
		taskExecutor.setCorePoolSize(8);
		taskExecutor.setMaxPoolSize(8);
		return taskExecutor;
	}

	@Bean
	public LobHandler lobHandler()
	{
		return new DefaultLobHandler();
	}

	@Bean
	public ExecutionContextSerializer executionContextSerializer()
	{
		var objectMapper = JsonMapper.builder();
		objectMapper.addModule(new JavaTimeModule());
		objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
		var serializer = new Jackson2ExecutionContextStringSerializer(
			"nl.rivm.screenit.model.logging.IntakeMakenLogEvent",
			"nl.rivm.screenit.model.colon.IntakeMakenLogEventRegel",
			"nl.rivm.screenit.batch.model.dto.MammaIlmRetryDto"
		);
		serializer.setObjectMapper(objectMapper.build());
		return serializer;
	}

	@Bean
	public JdbcTemplate jdbcTemplate(DataSource dataSource)
	{
		var jdbcTemplate = new JdbcTemplate();
		jdbcTemplate.setDataSource(dataSource);
		return jdbcTemplate;
	}

	@Bean
	public SimpleJobLauncher jobLauncher(TaskExecutor taskExecutor, JobRepository jobRepository)
	{
		var jobLauncher = new SimpleJobLauncher();
		jobLauncher.setTaskExecutor(taskExecutor);
		jobLauncher.setJobRepository(jobRepository);
		return jobLauncher;
	}

	@Bean
	public JobRegistry jobRegistry()
	{
		return new MapJobRegistry();
	}

	@Bean
	public JobRepositoryFactoryBean jobRepository(DataSource dataSource, ExecutionContextSerializer executionContextSerializer, HibernateTransactionManager transactionManager,
		LobHandler lobHandler)
	{
		var jobRepository = new JobRepositoryFactoryBean();
		jobRepository.setDataSource(dataSource);
		jobRepository.setTransactionManager(transactionManager);
		jobRepository.setLobHandler(lobHandler);
		jobRepository.setSerializer(executionContextSerializer);
		return jobRepository;
	}

	@Bean
	public JobExplorerFactoryBean jobExplorer(DataSource dataSource, ExecutionContextSerializer executionContextSerializer)
	{
		var jobExplorer = new JobExplorerFactoryBean();
		jobExplorer.setDataSource(dataSource);
		jobExplorer.setSerializer(executionContextSerializer);
		return jobExplorer;
	}

	@Bean
	public JobOperator jobOperator(JobLauncher jobLauncher, JobExplorer jobExplorer, JobRepository jobRepository, JobRegistry jobRegistry)
	{
		var jobOperator = new SimpleJobOperator();
		jobOperator.setJobLauncher(jobLauncher);
		jobOperator.setJobExplorer(jobExplorer);
		jobOperator.setJobRepository(jobRepository);
		jobOperator.setJobRegistry(jobRegistry);
		return jobOperator;
	}

	@Bean
	public JobRegistryBeanPostProcessor jobRegistryBeanPostProcessor(JobRegistry jobRegistry)
	{
		var jobRegistryBeanPostProcessor = new JobRegistryBeanPostProcessor();
		jobRegistryBeanPostProcessor.setJobRegistry(jobRegistry);
		return jobRegistryBeanPostProcessor;
	}

	@Bean
	public Step dummyStep(StepBuilderFactory stepBuilderFactory)
	{
		return stepBuilderFactory.get("dummyStep")
			.tasklet((contribution, chunkContext) -> RepeatStatus.FINISHED)
			.build();
	}

}
