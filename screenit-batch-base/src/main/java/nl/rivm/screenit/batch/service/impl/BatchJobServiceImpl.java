package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.batch.service.BatchJobService;
import nl.rivm.screenit.model.batch.BatchQueue;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.service.DistributedLockService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.reflect.ConstructorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class BatchJobServiceImpl implements BatchJobService
{

	private static final Logger LOG = LoggerFactory.getLogger(BatchJobServiceImpl.class);

	private static final String LOCKING_LOCK = "job_locking_lock";

	private static final String JOB_LOCK_PREFIX = "job_";

	private DistributedLockService distributedLockService;

	private HibernateService hibernateService;

	public BatchJobServiceImpl(DistributedLockService distributedLockService, HibernateService hibernateService)
	{
		this.distributedLockService = distributedLockService;
		this.hibernateService = hibernateService;
	}

	private static String mapToString(Map<String, Serializable> map)
	{
		if (map == null || map.size() <= 0)
		{
			return "";
		}
		StringBuilder stringBuilder = new StringBuilder();

		for (Entry<String, Serializable> entry : map.entrySet())
		{
			if (stringBuilder.length() > 0)
			{
				stringBuilder.append("&");
			}
			stringBuilder.append(entry.getKey());
			stringBuilder.append("=");
			Serializable value = entry.getValue();
			if (value instanceof Number || value instanceof String)
			{
				stringBuilder.append(value.getClass().getName());
				stringBuilder.append("->");
				stringBuilder.append(value.toString());
			}

		}

		return stringBuilder.toString();
	}

	private static Map<String, Serializable> stringToMap(String input)
	{
		Map<String, Serializable> map = new HashMap<>();

		String[] nameValuePairs = input.split("&");
		for (String nameValuePair : nameValuePairs)
		{
			String[] nameValue = nameValuePair.split("=");
			try
			{
				String value = nameValue[1];
				String[] splittedValue = value.split("->");

				Class<?> clazz = Class.forName(splittedValue[0]);
				Serializable valueObject = (Serializable) ConstructorUtils.invokeConstructor(clazz, splittedValue[1]);
				map.put(nameValue[0], valueObject);
			}
			catch (ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
			{
				LOG.error("This method requires UTF-8 encoding support", e);
			}
		}

		return map;
	}

	@Override
	public void enqueue(JobType jobType, Map<String, Serializable> jobArgs)
	{
		LOG.debug("Start enqueue " + jobType.name());
		queueJob(jobType, jobArgs);
		LOG.debug("End enqueue " + jobType.name());
	}

	@Override
	public JobType getHeadOfBatchJobQueue(BatchApplicationType batchApplicationType)
	{
		JobType peeked = peekQueuedEntry(batchApplicationType);
		if (peeked != null)
		{
			LOG.trace("headFromQueue " + peeked.name());
		}
		return peeked;
	}

	@Override
	public Map<String, Serializable> dequeueHead(JobType jobType)
	{
		LOG.debug("Start dequeueHead");
		Map<String, Serializable> jobArgs = pollQueuedEntry(jobType);
		LOG.debug("End dequeueHead " + jobArgs);
		return jobArgs;
	}

	private JobType peekQueuedEntry(BatchApplicationType batchApplicationType)
	{
		JobType jobType = null;
		try
		{
			@SuppressWarnings("unchecked")
			List<String> types = hibernateService.getHibernateSession().createSQLQuery(BatchQueue.SQL_GET_QUEUE).list();

			if (types != null && !types.isEmpty())
			{

				for (String type : types)
				{
					JobType queuedJob = JobType.valueOf(type);

					if (queuedJob.getBatchApplicationType().equals(batchApplicationType))
					{
						jobType = queuedJob;
						break;
					}

					else if (queuedJob.getBatchApplicationType().equals(BatchApplicationType.GENERALIS))
					{
						jobType = null;
						break;
					}
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij dequeue jobType", e);
		}

		return jobType;
	}

	private Map<String, Serializable> pollQueuedEntry(JobType jobType)
	{
		Map<String, Serializable> jobArgs = null;
		try
		{

			List<?> ids = hibernateService.getHibernateSession()
				.createSQLQuery(BatchQueue.SQL_INNER_PEEK_BATCH) 
				.setParameter(BatchQueue.TYPE_PARAM, jobType.name()) 
				.list();

			Long id = null;
			if (ids != null && !ids.isEmpty() && ids.get(0) != null)
			{
				id = ((BigInteger) ids.get(0)).longValue();
			}

			if (id != null)
			{

				Object queueEntry = hibernateService.getHibernateSession().createSQLQuery(BatchQueue.SQL_PEEK_BATCH1) 
					.setParameter(BatchQueue.ID_PARAM, id) 
					.uniqueResult();

				if (queueEntry != null)
				{
					Object[] queueRow = (Object[]) queueEntry;
					JobType selectedJobType = JobType.valueOf((String) queueRow[0]);
					LOG.debug("poll jobType " + selectedJobType.name());
					jobArgs = stringToMap((String) queueRow[1]);
				}

				hibernateService.getHibernateSession().createSQLQuery(BatchQueue.SQL_POLL_BATCH) 
					.setParameter(BatchQueue.ID_PARAM, id) 
					.executeUpdate();
			}

		}
		catch (Exception e)
		{
			LOG.error("Fout bij dequeue jobType", e);
		}

		return jobArgs;
	}

	private void queueJob(JobType jobType, Map<String, Serializable> jobArgs)
	{
		try
		{
			hibernateService.getHibernateSession()
				.createSQLQuery(BatchQueue.SQL_INSERT_JOB) 
				.setParameter(BatchQueue.TYPE_PARAM, jobType.name()) 
				.setParameter(BatchQueue.CONTEX_PARAM, mapToString(jobArgs)) 
				.executeUpdate();

		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}

	@Override
	public void waitForJobLock(JobType jobType)
	{

		distributedLockService.lockAndWait(LOCKING_LOCK);

		List<JobType> dependendJobs = new ArrayList<>(EnumSet.allOf(JobType.class));
		dependendJobs.removeAll(jobType.getParallelleJobs());

		LOG.debug("Afhankelijke jobs locken. (" + dependendJobs.size() + ") jobs.");
		for (JobType jt : dependendJobs)
		{

			distributedLockService.lockAndWait(getJobLockName(jt));
			unlockJob(jt);
		}

		distributedLockService.lockAndWait(getJobLockName(jobType));

		distributedLockService.unlock(LOCKING_LOCK);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public void unlockJob(JobType jobType)
	{
		distributedLockService.unlock(getJobLockName(jobType));
	}

	private String getJobLockName(JobType jobType)
	{
		return JOB_LOCK_PREFIX + jobType.toString();
	}
}
