
package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import javax.persistence.Temporal;

import nl.rivm.screenit.model.envers.ScreenitRevisionEntity;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.hibernate.ObjectNotFoundException;
import org.hibernate.Session;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.RevisionType;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.hibernate.envers.query.criteria.AuditCriterion;
import org.hibernate.proxy.HibernateProxyHelper;
import org.reflections.ReflectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class EntityAuditUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(EntityAuditUtil.class);

	private EntityAuditUtil()
	{

	}

	public final static <H extends HibernateObject> AuditQuery createQuery(H entity, Session session)
	{
		AuditReader reader = AuditReaderFactory.get(session);
		Class clazz = null;
		Serializable id = null;
		try
		{
			clazz = Hibernate.getClass(entity);
			id = entity.getId();
		}
		catch (ObjectNotFoundException e)
		{

			clazz = HibernateProxyHelper.getClassWithoutInitializingProxy(entity);
			id = HibernateHelper.getId(entity);
		}
		AuditQuery query = reader.createQuery().forRevisionsOfEntity(clazz, false, true);

		query.add(AuditEntity.id().eq(id));
		return query;
	}

	public final static <H extends HibernateObject> H getPreviousToLastEntity(H entity, Session session)
	{
		List<Object[]> results = EntityAuditUtil.getEntityHistory(entity, session, null, false, 2);
		if (results.size() > 1)
		{
			return EntityAuditUtil.getRevisionEntity(results.get(1));
		}
		return null;
	}

	public final static <H extends HibernateObject> H getLastEntity(H entity, Session session)
	{
		List<Object[]> results = getEntityHistory(entity, session, null, false, 1);
		if (results.size() > 0)
		{
			return EntityAuditUtil.getRevisionEntity(results.get(0));
		}
		return null;
	}

	public final static <H extends HibernateObject> List<Object[]> getEntityHistory(H entity, Session session, boolean changedOnly)
	{
		return getEntityHistory(entity, session, null, changedOnly, null);
	}

	public final static <H extends HibernateObject> List<Object[]> getEntityHistory(H entity, Session session, AuditCriterion additionalCriteria, boolean changedOnly)
	{
		return getEntityHistory(entity, session, additionalCriteria, changedOnly, null);
	}

	public final static <H extends HibernateObject> List<Object[]> getEntityHistory(H entity, Session session, AuditCriterion additionalCriteria, boolean changedOnly,
		Integer maxResults)
	{
		AuditQuery query = createQuery(entity, session);
		if (additionalCriteria != null)
		{
			query.add(additionalCriteria);
		}
		if (changedOnly)
		{
			query.add(AuditEntity.revisionType().eq(RevisionType.MOD));
		}
		if (maxResults != null)
		{
			query.setMaxResults(maxResults);
		}
		query.addOrder(AuditEntity.revisionNumber().desc());
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	public final static <H extends HibernateObject> H getRevisionEntity(Object auditRow)
	{
		return (H) ((Object[]) auditRow)[0];
	}

	public final static ScreenitRevisionEntity getRevisionInfo(Object auditRow)
	{
		return (ScreenitRevisionEntity) ((Object[]) auditRow)[1];
	}

	public final static RevisionType getRevisionType(Object auditRow)
	{
		return (RevisionType) ((Object[]) auditRow)[2];
	}

	public final static <T extends HibernateObject> String getDiffFieldToLatestVersion(T entity, String fieldName, Session session)
	{
		if (entity.getId() != null)
		{
			T lastEntity = getLastEntity(entity, session);
			if (lastEntity != null)
			{
				return diffEntities(lastEntity, entity, fieldName);
			}
		}
		return diffEntities(null, entity, null);
	}

	public final static <T extends HibernateObject> String getDiffToLatestVersion(T entity, Session session)
	{
		if (entity.getId() != null)
		{
			T lastEntity = getLastEntity(entity, session);
			if (lastEntity != null)
			{
				return diffEntities(lastEntity, entity, null);
			}
		}
		return diffEntities(null, entity, null);
	}

	private final static <T extends HibernateObject> String diffEntities(T oldEntity, T newEntity, String specifiekFieldName)
	{
		String diff = "";
		newEntity = (T) HibernateHelper.deproxy(newEntity);
		SkipFieldsForDiff skipFieldsForDiff = newEntity.getClass().getAnnotation(SkipFieldsForDiff.class);
		for (Field field : ReflectionUtils.getAllFields(newEntity.getClass()))
		{
			String fieldName = field.getName();
			SkipFieldForDiff skipField = field.getAnnotation(SkipFieldForDiff.class);
			if ((specifiekFieldName == null || specifiekFieldName.equals(fieldName)) && !fieldName.equals("id") && !fieldName.equals("serialVersionUID")
				&& !fieldName.startsWith("$j") && !fieldName.equals("PROPERTY_ID") && skipField == null
				&& (skipFieldsForDiff == null || !Arrays.asList(skipFieldsForDiff.value()).contains(fieldName)))
			{
				DiffSpecs diffSpec = field.getAnnotation(DiffSpecs.class);
				Object oldValue = null;
				Object newValue = null;
				try
				{
					if (oldEntity != null)
					{
						oldValue = PropertyUtils.getProperty(oldEntity, fieldName);
					}
					newValue = PropertyUtils.getProperty(newEntity, fieldName);
				}
				catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
				{
					LOG.error(e.getMessage(), e);
				}

				if (field.getType().isPrimitive() || field.getType().equals(String.class)
					|| field.getType().equals(Boolean.class))
				{
					if (!Objects.equals(oldValue, newValue))
					{
						if (oldValue == null)
						{
							oldValue = "(geen waarde)";
						}
						if (newValue == null)
						{
							newValue = "(geen waarde)";
						}

						if (diff.length() > 0)
						{
							diff += ", ";
						}
						diff += fieldName + ": " + oldValue + " -> " + newValue;
					}
				}
				else if (Date.class.isAssignableFrom(field.getType()))
				{
					String dateTimeFormat = getDateTimeFormat(field, (Date) oldValue, (Date) newValue);
					oldValue = formatValue((Date) oldValue, dateTimeFormat);
					newValue = formatValue((Date) newValue, dateTimeFormat);
					if (!Objects.equals(newValue, oldValue))
					{

						if (diff.length() > 0)
						{
							diff += ", ";
						}

						diff += fieldName + ": " + oldValue + " -> " + newValue;
					}
				}
				else if (HibernateObject.class.isAssignableFrom(field.getType()))
				{
					if (!Objects.equals(oldValue, newValue))
					{
						diff = getDiffFromReference(diff, diffSpec, fieldName, oldValue, newValue);
					}
				}
				else if (Comparable.class.isAssignableFrom(field.getType()))
				{
					if (ObjectUtils.compare((Comparable<T>) oldValue, (Comparable<T>) newValue) != 0)
					{
						diff = getDiffFromReference(diff, diffSpec, fieldName, oldValue, newValue);
					}
				}
				else if (List.class.isAssignableFrom(field.getType()) && diffSpec != null && diffSpec.listSupported())
				{
					List oldValueList = oldValue != null ? new ArrayList((List) oldValue) : new ArrayList();
					List newValueList = newValue != null ? new ArrayList((List) newValue) : new ArrayList();
					if (!CollectionUtils.isEqualCollection(oldValueList, newValueList))
					{
						diff = getDiffFromList(diff, diffSpec, fieldName, oldValueList, newValueList);
					}
				}
			}
		}
		return diff;
	}

	private static String formatValue(Date value, String dateTimeFormat)
	{
		String returnValue = "";
		if (value == null)
		{
			returnValue = "(geen waarde)";
		}
		else if (StringUtils.isNotBlank(dateTimeFormat))
		{
			returnValue = new SimpleDateFormat(dateTimeFormat).format(value);
		}
		return returnValue;
	}

	private static String getDateTimeFormat(Field field, Date oldValue, Date newValue)
	{
		String format = null;
		Temporal temporal = field.getAnnotation(Temporal.class);
		if (temporal != null && temporal.value() != null)
		{
			switch (temporal.value())
			{
			case DATE:
				format = "dd-MM-yyyy";
				break;
			case TIME:
				format = "HH:mm";
				break;
			case TIMESTAMP:
				format = "dd-MM-yyyy HH:mm";
				break;
			default:
				break;

			}
		}
		if (StringUtils.isBlank(format))
		{
			if (oldValue instanceof Timestamp || newValue instanceof Timestamp)
			{
				format = "dd-MM-yyyy hh:mm";
			}
			else if (oldValue instanceof Time || newValue instanceof Time)
			{
				format = "hh:mm";
			}
			else if (oldValue instanceof java.sql.Date || newValue instanceof java.sql.Date)
			{
				format = "dd-MM-yyyy";
			}
			else if (oldValue instanceof Date || newValue instanceof Date)
			{
				format = "dd-MM-yyyy hh:mm";
			}
		}
		return format;
	}

	private static String getDiffFromReference(String diff, DiffSpecs diffSpec, String fieldName, Object oldValue, Object newValue)
	{
		if (diff.length() > 0)
		{
			diff += ", ";
		}
		String displayProperty = null;
		if (oldValue == null)
		{
			oldValue = "(geen waarde)";
		}
		if (newValue == null)
		{
			newValue = "(geen waarde)";
		}
		if (diffSpec != null)
		{
			displayProperty = diffSpec.displayProperty();
		}
		if (StringUtils.isNotBlank(displayProperty))
		{
			try
			{
				if (!(oldValue instanceof String))
				{
					oldValue = PropertyUtils.getProperty(oldValue, displayProperty);
				}
				newValue = PropertyUtils.getProperty(newValue, displayProperty);
			}
			catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
			{
				LOG.error(e.getMessage(), e);
			}
		}
		diff += fieldName + ": " + oldValue + " -> " + newValue;
		return diff;
	}

	private static String getDiffFromList(String diff, DiffSpecs diffSpec, String fieldName, List oldValueList, List newValueList)
	{

		if (diff.length() > 0)
		{
			diff += ", ";
		}
		String displayProperty = null;
		if (diffSpec != null)
		{
			displayProperty = diffSpec.displayProperty();
		}
		diff += fieldName + ": " + getListValue(oldValueList, displayProperty) + " -> " + getListValue(newValueList, displayProperty);
		return diff;
	}

	private static String getListValue(List list, String displayProperty)
	{
		String value = "";
		if (CollectionUtils.isEmpty(list))
		{
			value = "(geen waarden)";
		}
		else
		{
			value = "[";
			boolean first = true;
			for (Object element : list)
			{
				if (first)
				{
					first = false;
				}
				else
				{
					value += ", ";
				}
				if (!StringUtils.isBlank(displayProperty) && !(element instanceof String))
				{
					try
					{
						value += PropertyUtils.getProperty(element, displayProperty);
					}
					catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
					{
						LOG.error(e.getMessage(), e);
					}
				}
				else
				{
					value += element;
				}
			}
			value += "]";
		}
		return value;
	}
}
