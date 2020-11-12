
package nl.rivm.screenit.hl7v3.cda.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-ws-common
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBElement;

import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;
import nl.rivm.screenit.hl7v3.cda.EN;
import nl.rivm.screenit.hl7v3.cda.II;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedAuthor;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedEntity;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040DocumentationOf;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040Organization;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040Performer1;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040ServiceEvent;
import nl.rivm.screenit.hl7v3.cda.ST;

import org.apache.commons.beanutils.PropertyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CDAHelper
{
	
	private static final Logger LOG = LoggerFactory.getLogger(CDAHelper.class);

	private CDAHelper()
	{
	}

	public static String getExtension(String oidRoot, List<? extends II> ids)
	{
		String idExtension = null;

		if (ids != null)
		{
			for (II id : ids)
			{
				if (id.getRoot() != null && id.getRoot().equals(oidRoot))
				{
					idExtension = id.getExtension();
					break;
				}
			}
		}
		return idExtension;
	}

	public static String getRootExtension(II id)
	{
		String rootExtension = id.getRoot().trim();
		if (id.getExtension() != null && !id.getExtension().trim().equals(""))
		{
			rootExtension += "." + id.getExtension().trim();
		}
		return rootExtension;
	}

	public static boolean hasRootOidId(String oidRoot, List<? extends II> ids)
	{
		boolean result = false;

		if (ids != null)
		{
			for (II id : ids)
			{
				if (id.getRoot() != null && id.getRoot().equals(oidRoot))
				{
					result = true;
					break;
				}
			}
		}
		return result;
	}

	public static <T extends ST> String getNamePart(List<? extends EN> list, Class<T> declaredClazz, int maxLength, String defaultValue)
	{
		String name = null;
		if (list != null)
		{
			for (EN item : list)
			{
				String namePart = getNamePart(item, declaredClazz);
				if (namePart != null && namePart.trim().length() > 0)
				{
					name = namePart.trim();
				}
			}
		}
		if (name == null && defaultValue != null)
		{
			name = defaultValue;
		}
		if (name != null && maxLength > 0 && name.length() > maxLength)
		{
			name = name.substring(0, maxLength);
		}
		return name;
	}

	public static <T extends ST> String getNamePart(EN item, Class<T> declaredClazz)
	{
		List<Serializable> content = item.getContent();

		String result = null;
		for (Serializable contentItem : content)
		{
			if (contentItem instanceof JAXBElement)
			{
				@SuppressWarnings("unchecked")
				JAXBElement<T> jaxbElement = (JAXBElement<T>) contentItem;
				if (jaxbElement.getDeclaredType().equals(declaredClazz))
				{
					T namePartItem = jaxbElement.getValue();
					List<Serializable> namePartItemContent = namePartItem.getContent();
					if (namePartItemContent != null && !namePartItemContent.isEmpty())
					{
						if (result != null)
						{
							result += " ";
						}
						else
						{
							result = "";
						}
						result += namePartItemContent.get(0).toString();
					}
				}
			}
			else if (declaredClazz == null && contentItem instanceof String)
			{
				if (result != null)
				{
					result += " ";
				}
				else
				{
					result = "";
				}
				result += contentItem.toString();
			}
		}
		return result;
	}

	public static POCDMT000040AssignedAuthor getAssigendAuthor(ClinicalDocument cda)
	{
		if (cda.getAuthors().isEmpty())
		{
			return null;
		}
		else
		{
			return cda.getAuthors().get(0).getAssignedAuthor();
		}
	}

	public static POCDMT000040AssignedEntity getAssigendEntity(ClinicalDocument cda)
	{
		List<POCDMT000040DocumentationOf> documentationOves = cda.getDocumentationOves();
		POCDMT000040AssignedEntity assignedAuthor = null;
		if (documentationOves != null && !documentationOves.isEmpty())
		{
			POCDMT000040ServiceEvent serviceEvent = documentationOves.get(0).getServiceEvent();
			if (serviceEvent != null)
			{
				List<POCDMT000040Performer1> performers = serviceEvent.getPerformers();
				if (performers != null && !performers.isEmpty())
				{
					assignedAuthor = performers.get(0).getAssignedEntity();
				}
			}
		}
		return assignedAuthor;
	}

	public static <T> T getFirstValue(Object root, String path)
	{
		List<T> values = getAllValues(root, path);
		if (values.isEmpty())
		{
			return null;
		}
		else
		{
			return values.get(0);
		}
	}

	public static String getFirstValueNotNull(Object root, String path)
	{
		List<String> values = getAllValues(root, path);
		if (values.isEmpty())
		{
			return "onbekend";
		}
		else
		{
			return values.get(0);
		}
	}

	public static <T> List<T> getAllValues(Object root, String path)
	{
		List<T> values = new ArrayList<>();
		if (root != null && path != null && path.trim().length() > 0)
		{
			getValueFromChild(root, new ArrayList<String>(Arrays.asList(path.split(":"))), values);
		}
		return values;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static <T> void getValueFromChild(Object bean, List<String> pathElements, List<T> values)
	{
		String element = pathElements.get(0);
		try
		{
			String oid = null;
			if (element.contains("["))
			{
				oid = element.substring(element.indexOf('[') + 1, element.indexOf(']'));
				element = element.substring(0, element.indexOf('['));
			}
			Object simpleProperty = PropertyUtils.getSimpleProperty(bean, element);
			if (simpleProperty != null)
			{
				if (pathElements.size() == 1)
				{
					if (simpleProperty instanceof List)
					{
						values.addAll((List<T>) simpleProperty);
					}
					else
					{
						values.add((T) simpleProperty);
					}
				}
				else
				{
					pathElements.remove(0);

					if (simpleProperty instanceof List)
					{
						for (Object simplePropertyElement : (List) simpleProperty)
						{
							if (hasTemplateId(simplePropertyElement, oid))
							{
								getValueFromChild(simplePropertyElement, new ArrayList<>(pathElements), values);
							}
						}
					}
					else if (hasTemplateId(simpleProperty, oid))
					{
						getValueFromChild(simpleProperty, new ArrayList<>(pathElements), values);
					}

				}
			}
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | StringIndexOutOfBoundsException e)
		{
			LOG.error("error bij ophalen van element " + element, e);
		}
	}

	@SuppressWarnings("unchecked")
	private static <T> boolean hasTemplateId(T simpleProperty, String oid)
	{
		boolean hasTemplateId = true;
		if (oid != null && oid.trim().length() > 0)
		{
			try
			{
				Object list = PropertyUtils.getProperty(simpleProperty, "templateIds");
				if (list instanceof List)
				{
					hasTemplateId = false;
					@SuppressWarnings("rawtypes")
					List templateIds = (List) list;
					if (templateIds != null && !templateIds.isEmpty() && templateIds.get(0) instanceof II)
					{
						hasTemplateId = hasRootOidId(oid, templateIds);
					}
				}

			}
			catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
			{
				LOG.error("error bij zoeken naar oid " + oid + " in " + simpleProperty.toString(), e);
			}
		}
		return hasTemplateId;
	}

	public static Date converCdaDateStringToDate(String dateValue) throws ParseException
	{
		Date returnValue = null;
		if (dateValue != null && dateValue.trim().length() > 0)
		{
			String dateFormat = "";
			if (dateValue.length() >= 8)
			{
				dateFormat = "yyyyMMdd";
			}
			if (dateValue.length() >= 10)
			{
				dateFormat += "HH";
			}
			if (dateValue.length() >= 12)
			{
				dateFormat += "mm";
			}
			if (dateValue.length() >= 14)
			{
				dateFormat += "ss";
			}
			if (dateValue.length() >= 16)
			{
				dateFormat += "SSS";
			}
			SimpleDateFormat formatter = new SimpleDateFormat(dateFormat);
			returnValue = formatter.parse(dateValue);
		}
		return returnValue;
	}

	public static String getUitvoerendeOrganisatieInformatie(ClinicalDocument cdaDocument)
	{
		POCDMT000040AssignedEntity assignedAuthor = getAssigendEntity(cdaDocument);

		String uitvoerendeOrganisatie = "";
		if (assignedAuthor != null)
		{
			uitvoerendeOrganisatie = " (uitvoerder '" + getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + "'; "
				+ CDAHelper.getUitvoerendeOrganisatieIds(assignedAuthor) + ")";
		}
		return uitvoerendeOrganisatie;
	}

	public static String getUitvoerendeOrganisatieIds(POCDMT000040AssignedEntity assignedAuthor)
	{
		StringBuilder organisationId = new StringBuilder();

		if (assignedAuthor != null)
		{
			POCDMT000040Organization representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null && representedOrganization.getIds() != null)
			{
				for (II ii : representedOrganization.getIds())
				{
					if (ii.getRoot() != null)
					{
						String root = ii.getRoot();
						String extension = ii.getExtension();
						if (CommonCdaConstants.URA.equals(root))
						{
							if (extension != null && extension.trim().length() > 0)
							{
								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append("URA:");
								organisationId.append(extension);
							}
						}
						else
						{
							String organisationOid = null;
							if (root != null && root.trim().length() > 0)
							{
								organisationOid = root;
								if (extension != null && extension.trim().length() > 0)
								{
									organisationOid += "." + extension.trim();
								}
							}
							if (organisationId != null)
							{
								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append(organisationOid);
							}
						}
					}
				}
			}
		}
		return organisationId.toString();
	}

	public static String getUzinummer(ClinicalDocument cda)
	{
		POCDMT000040AssignedEntity assignedAuthor = CDAHelper.getAssigendEntity(cda);
		String uzinummer = null;
		if (assignedAuthor != null)
		{
			uzinummer = CDAHelper.getExtension(CommonCdaConstants.UZINUMMER, assignedAuthor.getIds());
		}
		return uzinummer;
	}

}
