
package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;
import javax.persistence.Column;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.service.VerwerkCdaBerichtContentService;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.hl7v3.cda.helper.CDAHelper;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;
import nl.rivm.screenit.model.berichten.XPathMapping;
import nl.rivm.screenit.model.berichten.cda.CdaOID;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.Period;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import au.com.bytecode.opencsv.CSVReader;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VerwerkCdaBerichtContentServiceImpl implements VerwerkCdaBerichtContentService
{

	private static final Logger LOG = LoggerFactory.getLogger(VerwerkCdaBerichtContentServiceImpl.class);

	private static final Map<String, String> XPATH_MAPPING = new HashMap<>();

	private static final Map<String, String> CONCEPT_MAPPING = new HashMap<>();

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private HibernateService hibernateService;

	private class ConceptExceptionContext
	{
		private final VerslagDao verslagDao;

		private final XPath xpath;

		private final Node node;

		private final Field field;

		private final String xpathValue;

		private ConceptExceptionContext(VerslagDao verslagDao, Field field, XPath xpath, Node node, String xpathValue)
		{
			this.verslagDao = verslagDao;
			this.field = field;
			this.xpath = xpath;
			this.node = node;
			this.xpathValue = xpathValue;
		}
	}

	private enum MDL_EINDCONCLUSIE_ORDER
	{

		_CRC,

		_AAD,

		_NAAD,

		_449855005,

		_OTH,

		_313170008,

		_255046005,
		_448315008,
		_285611007
	}

	private enum XpathAlternativeMapping
	{

		PROTOCOL_COLON_BIOPT_PER_POLIEP("hl7:act/hl7:specimen/hl7:specimenRole/hl7:id/@extension")
		{
			@Override
			Map<String, String> getAlternatives()
			{
				Map<String, String> map = new HashMap<>();
				MapUtils.putAll(map,
					new String[][] { { 

						"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/" + "hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/"
							+ "hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]",

						"hl7:organizer/hl7:specimen/hl7:specimenRole/hl7:id/@extension", 
					}, {

						"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/" + "hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/"
							+ "hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.315']]]",

						"hl7:organizer/hl7:specimen/hl7:specimenRole/hl7:id/@extension" 
					}, 
					});
				return map;
			}
		};

		private final String xpathReferenceValue;

		XpathAlternativeMapping(String xpathReferenceValue)
		{
			this.xpathReferenceValue = xpathReferenceValue;
		}

		abstract Map<String, String> getAlternatives();

		String getXpathReferenceValue()
		{
			return xpathReferenceValue;
		}
	}

	private enum ConceptExceptionHandler
	{
		MDL_DUURVERRICHTING("125051", VerslagGeneratie.V1)
		{

			@Override
			Object getValue(ConceptExceptionContext context) throws XPathExpressionException
			{
				String xpathValueAanvang = XPATH_MAPPING.get(CdaOID.CONCEPTS_ROOT_OID + ".1.125030").split("\\|")[0];
				String xpathValueEinde = XPATH_MAPPING.get(CdaOID.CONCEPTS_ROOT_OID + ".1.125050").split("\\|")[0];
				Date dateAanvang = getDateValue(context.node, context.xpath, xpathValueAanvang);
				Date dateEinde = getDateValue(context.node, context.xpath, xpathValueEinde);
				if (dateAanvang == null || dateEinde == null)
				{
					return null;
				}
				else
				{
					Quantity duur = new Quantity();
					duur.setValue(Math.abs(new Period(new DateTime(dateEinde), new DateTime(dateAanvang)).getMinutes()) + "");
					duur.setUnit("min");
					return duur;
				}
			}

		},
		MDL_TNUMMER("145162")
		{

			@Override
			Object getValue(ConceptExceptionContext context) throws XPathExpressionException
			{
				return context.xpath.compile("@extension").evaluate(context.node);
			}

		},
		MDL_OPDRACHTNUMMER("145161")
		{

			@Override
			Object getValue(ConceptExceptionContext context) throws XPathExpressionException
			{
				return context.xpath.compile("@extension").evaluate(context.node);
			}

		},
		MDL_EINDCONCLUSIE("140160", VerslagGeneratie.V1)
		{

			@Override
			Object getValue(ConceptExceptionContext context) throws XPathExpressionException
			{
				DSValueSet dsValueSet = context.field.getAnnotation(DSValueSet.class);
				DSValue dsZoekObject = new DSValue();
				Object nodeSet = context.xpath.compile(context.xpathValue).evaluate(context.node, XPathConstants.NODESET);

				String dsValueCode = null;
				Node dsValueNode = null;
				if (nodeSet instanceof NodeList)
				{
					NodeList nodeList = (NodeList) nodeSet;
					for (int i = 0; i < nodeList.getLength(); i++)
					{
						Node dsNode = nodeList.item(i);
						String foundDsValueCode = context.xpath.compile("@code").evaluate(dsNode);
						if (StringUtils.isBlank(foundDsValueCode))
						{
							foundDsValueCode = context.xpath.compile("@nullFlavor").evaluate(dsNode);
						}
						if (dsValueCode == null || StringUtils.isNotBlank(foundDsValueCode)
							&& MDL_EINDCONCLUSIE_ORDER.valueOf("_" + foundDsValueCode).ordinal() < MDL_EINDCONCLUSIE_ORDER.valueOf("_" + dsValueCode).ordinal())
						{
							dsValueNode = dsNode;
							dsValueCode = foundDsValueCode;
						}

					}
				}
				DSValue dsValue = null;
				if (dsValueCode != null)
				{
					dsZoekObject.setCode(dsValueCode);
					dsZoekObject.setCodeSystem(context.xpath.compile("@codeSystem").evaluate(dsValueNode));
					dsZoekObject.setValueSetName(dsValueSet.name());
					dsValue = (DSValue) zoekDsValue(context.xpath, dsValueNode, dsZoekObject, context.verslagDao);
				}
				return dsValue;
			}
		};

		private final String shortConceptId;

		private final VerslagGeneratie[] generaties;

		ConceptExceptionHandler(String shortConceptId, VerslagGeneratie... generaties)
		{
			this.shortConceptId = shortConceptId;
			this.generaties = generaties;
		}

		abstract Object getValue(ConceptExceptionContext context) throws XPathExpressionException;

		static ConceptExceptionHandler convertFromCode(String code, VerslagType verslagType)
		{
			for (ConceptExceptionHandler handler : values())
			{
				if (getFullConceptId(handler.shortConceptId, verslagType).equals(code)
					&& (handler.generaties == null || handler.generaties.length == 0 || Arrays.asList(handler.generaties).contains(verslagType.getHuidigeGeneratie())))
				{
					return handler;
				}
			}
			return null;
		}

	}

	private enum PostConceptValueConverter
	{
		DUMMY("", null)
		{

			@Override
			Object getValue(Object value)
			{
				return value;
			}

		},

		;

		private final String shortConceptId;

		private final VerslagGeneratie generatie;

		PostConceptValueConverter(String shortConceptId, VerslagGeneratie generatie)
		{
			this.shortConceptId = shortConceptId;
			this.generatie = generatie;
		}

		abstract Object getValue(Object value);

		static PostConceptValueConverter convertFromCode(String code, VerslagType verslagType)
		{
			for (PostConceptValueConverter converter : values())
			{
				if (getFullConceptId(converter.shortConceptId, verslagType).equals(code)
					&& (converter.generatie == null || converter.generatie.equals(verslagType.getHuidigeGeneratie())))
				{
					return converter;
				}
			}
			return null;
		}
	}

	@PostConstruct
	public void init()
	{
		try
		{
			loadXpathMapping("/verslagdataset/xpathmapping.txt");
			loadXpathMapping("/verslagdataset/xpathmapping-corr.txt");
			loadConceptMapping("/verslagdataset/conceptmapping-man.txt");
			loadConceptMapping("/verslagdataset/conceptmapping.txt");
		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden van config files ", e);
		}
	}

	private void loadXpathMapping(String resource) throws IOException, FileNotFoundException
	{
		try (CSVReader reader = new CSVReader(new InputStreamReader(VerwerkCdaBerichtContentServiceImpl.class.getResourceAsStream(resource)), ','))
		{
			String[] line = null;
			do
			{
				line = reader.readNext();
				if (line != null)
				{
					XPATH_MAPPING.put(line[0], line[1]);
				}
			}
			while (line != null);
		}
	}

	private void loadConceptMapping(String resource) throws IOException, FileNotFoundException
	{
		try (CSVReader reader = new CSVReader(new InputStreamReader(VerwerkCdaBerichtContentServiceImpl.class.getResourceAsStream(resource)), ';'))
		{
			String[] line = null;
			do
			{
				line = reader.readNext();
				if (line != null && !line[1].equals(Constants.NO_OLD_CONCEPT))
				{
					CONCEPT_MAPPING.put(line[0], line[1]);
				}
			}
			while (line != null);
			reader.close();
		}
	}

	@Override
	public void verwerkVerslagContent(Verslag verslag, Class<? extends VerslagContent> rootClazz)
	{
		try
		{
			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			domFactory.setNamespaceAware(true);
			DocumentBuilder builder = domFactory.newDocumentBuilder();
			Document doc = builder.parse(IOUtils.toInputStream(verslag.getOntvangenBericht().getXmlBericht()));

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			xpath.setNamespaceContext(new UniversalNamespaceCache(doc, "hl7"));

			VerslagGeneratie generatie = VerslagProjectVersionMapping.get().getGeneratie(verslag.getOntvangenBericht().getProjectVersion(), verslag.getType());

			VerslagContent verslagContent = maakEnFullVerlagDeel(doc, xpath, rootClazz, verslag, "", verslag.getType(), generatie);
			VerslagContent oldVerslagContent = verslag.getVerslagContent();
			if (oldVerslagContent != null)
			{
				oldVerslagContent.setVerslag(null);
				hibernateService.delete(oldVerslagContent);
			}
			verslag.setVerslagContent(verslagContent);
			verslagContent.setVersie(generatie);
			verslagContent.setVerslag(verslag);
		}
		catch (NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException | SAXException | IOException | ParserConfigurationException
			| XPathExpressionException e)
		{
			LOG.error("Fout bij vertaling van bericht naar model classes", e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private <T, S> T maakEnFullVerlagDeel(Node node, XPath xpath, Class<T> rootClazz, S parent, String rootXPath, VerslagType verslagType, VerslagGeneratie generatie)
		throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException, XPathExpressionException
	{
		T verslagDeel = rootClazz.newInstance();
		Field[] declaredFields = rootClazz.getDeclaredFields();

		for (Field declaredField : declaredFields)
		{
			Class<?> declaringType = declaredField.getType();
			String fieldName = declaredField.getName();
			VraagElement vraagElement = declaredField.getAnnotation(VraagElement.class);
			if (vraagElement != null && vraagElement.useInCda())
			{
				XPathMapping xpathMapping = getXpathMapping(vraagElement, verslagType, generatie);
				if (declaredField.isAnnotationPresent(OneToOne.class))
				{
					PropertyUtils.setProperty(verslagDeel, fieldName, maakEnFullVerlagDeel(node, xpath, declaringType, verslagDeel, rootXPath, verslagType, generatie));

				}
				else if (declaredField.isAnnotationPresent(OneToMany.class) || declaredField.isAnnotationPresent(ManyToMany.class))
				{
					if (xpathMapping != null)
					{
						String nieuwRootXPath = xpathMapping.value;
						if (isRootPartOfCurrentXpath(rootXPath, nieuwRootXPath))
						{
							nieuwRootXPath = removeRootFromXpath(rootXPath, nieuwRootXPath);
						}
						else if (!(node instanceof Document))
						{
							LOG.warn("root path " + rootXPath + " niet in nieuwXpath " + nieuwRootXPath + " voor " + declaredField);
						}
						Object nodeSet = xpath.compile(nieuwRootXPath).evaluate(node, XPathConstants.NODESET);
						Object property = PropertyUtils.getProperty(verslagDeel, fieldName);
						if (nodeSet instanceof NodeList && property instanceof List)
						{
							List list = (List) property;
							NodeList nodeList = (NodeList) nodeSet;
							Class paramType = getType(declaredField);
							String extension = xpathMapping.extension;

							for (int i = 0; i < nodeList.getLength(); i++)
							{
								LOG.debug("element " + (i + 1) + " van " + nodeList.getLength() + " van veld " + declaredField);
								Node itemNode = nodeList.item(i);
								NamedNodeMap attributes = itemNode.getAttributes();
								Node negationIndNode = attributes.getNamedItem("negationInd");
								if (negationIndNode == null || !"true".equals(negationIndNode.getNodeValue()))
								{
									if (!HibernateObject.class.isAssignableFrom(paramType) || paramType.equals(DSValue.class))
									{
										Object value = getValue(itemNode, xpath, ".", extension, declaredField, verslagType);
										if (value != null)
										{
											list.add(value);
										}
										else
										{
											LOG.warn("geen value voor element " + (i + 1) + " van veld " + declaredField);
										}
									}
									else
									{
										list.add(maakEnFullVerlagDeel(itemNode, xpath, paramType, verslagDeel, nieuwRootXPath, verslagType, generatie));
									}
								}
							}
						}
						else
						{
							LOG.warn("geen nodes voor " + declaredField);
						}
					}
					else
					{
						LOG.warn("geen xpath voor " + declaredField + " code " + vraagElement.code() + " verslagType " + verslagType.name());
					}
				}
				else if (declaredField.isAnnotationPresent(ManyToOne.class))
				{
					if (declaringType.equals(DSValue.class))
					{
						PropertyUtils.setProperty(verslagDeel, fieldName, getValue(node, xpath, declaredField, rootXPath, verslagType, generatie));
					}
				}
				else if (declaredField.getType().equals(Quantity.class) || declaredField.getType().equals(NullFlavourQuantity.class)
					|| declaredField.isAnnotationPresent(Column.class))
				{
					PropertyUtils.setProperty(verslagDeel, fieldName, getValue(node, xpath, declaredField, rootXPath, verslagType, generatie));
				}

			}
			else if (parent.getClass().equals(declaringType))
			{
				PropertyUtils.setProperty(verslagDeel, fieldName, parent);
			}
		}
		return verslagDeel;
	}

	private String removeRootFromXpath(String rootXPath, String nieuwRootXPath)
	{
		if (nieuwRootXPath.substring(rootXPath.length()).startsWith("["))
		{
			nieuwRootXPath = "self::node()" + nieuwRootXPath.substring(rootXPath.length());
		}
		else
		{
			nieuwRootXPath = nieuwRootXPath.substring(rootXPath.length() + 1);
		}
		return nieuwRootXPath;
	}

	private XPathMapping getXpathMapping(VraagElement vraagElement, VerslagType verslagType, VerslagGeneratie generatie)
	{
		XPathMapping mapping = null;
		if (vraagElement != null)
		{
			String conceptIdFromCurrentVersion = vraagElement.code();
			conceptIdFromCurrentVersion = conceptIdFromCurrentVersion.replace("-group", "");
			String xpath = XPATH_MAPPING.get(conceptIdFromCurrentVersion);
			if (generatie != null && !generatie.isHuidigeGeneratie(verslagType))
			{
				String conceptId = conceptIdFromCurrentVersion;
				String conceptIdForGeneratie = verslagType.getConceptRootOid() + "." + generatie.getVersionInConceptId() + conceptId.substring(conceptId.lastIndexOf("."));
				xpath = XPATH_MAPPING.get(conceptIdForGeneratie);
				if (StringUtils.isBlank(xpath))
				{
					do
					{
						conceptId = CONCEPT_MAPPING.get(conceptId);
					}
					while (conceptId != null && !conceptId.equals(Constants.NO_OLD_CONCEPT) && isConceptIdVanNieuwereGeneratie(verslagType, generatie, conceptId));

					if (conceptId == null && CONCEPT_MAPPING.containsKey(conceptIdFromCurrentVersion))
					{
						conceptId = conceptIdFromCurrentVersion;
					}
					xpath = XPATH_MAPPING.get(conceptId);
				}
			}
			if (xpath != null)
			{
				mapping = new XPathMapping();
				if (xpath.contains("|"))
				{
					String[] splittedXpath = xpath.split("\\|");
					xpath = splittedXpath[0];
					mapping.extension = splittedXpath[1];
				}
				mapping.value = xpath;
			}
		}
		return mapping;
	}

	private boolean isConceptIdVanNieuwereGeneratie(VerslagType verslagType, VerslagGeneratie generatie, String conceptId)
	{
		String conceptIdVersion = conceptId.replace(verslagType.getConceptRootOid() + ".", "");
		int indexOfPointAfterVersion = conceptIdVersion.indexOf('.');
		conceptIdVersion = conceptIdVersion.substring(0, indexOfPointAfterVersion);
		for (VerslagGeneratie v : VerslagGeneratie.values())
		{
			if (v.getVersionInConceptId().equals(conceptIdVersion))
			{
				return v.ordinal() > generatie.ordinal();
			}
		}
		return true;
	}

	private Class<?> getType(Field declaredField)
	{
		Type type = declaredField.getGenericType();
		if (type instanceof ParameterizedType)
		{
			ParameterizedType pt = (ParameterizedType) type;
			for (Type t : pt.getActualTypeArguments())
			{
				return (Class<?>) t;
			}
			return null;
		}
		else
		{
			return declaredField.getType();
		}
	}

	private Object getValue(Node node, XPath xpath, Field declaredField, String rootXPath, VerslagType verslagType, VerslagGeneratie generatie)
		throws XPathExpressionException, InstantiationException, IllegalAccessException
	{
		Object returnValue = null;
		VraagElement vraagElement = declaredField.getAnnotation(VraagElement.class);
		XPathMapping xpathMapping = getXpathMapping(vraagElement, verslagType, generatie);
		if (xpathMapping != null && StringUtils.isNotBlank(xpathMapping.value))
		{
			String xpathValue = xpathMapping.value;
			if (isRootPartOfCurrentXpath(rootXPath, xpathValue))
			{
				xpathValue = removeRootFromXpath(rootXPath, xpathValue);
			}
			else if (!(node instanceof Document))
			{
				boolean correctionSuccess = false;

				top: for (XpathAlternativeMapping mapping : XpathAlternativeMapping.values())
				{
					Map<String, String> alternatives = mapping.getAlternatives();
					for (Entry<String, String> altMapping : alternatives.entrySet())
					{
						String alternativeXpath = altMapping.getKey();
						if (xpathValue.startsWith(alternativeXpath))
						{
							String referenceValue = xpath.compile(mapping.getXpathReferenceValue()).evaluate(node);
							String alternativeReferenceValue = altMapping.getValue();
							node = (Node) xpath.compile(alternativeXpath + "[" + alternativeReferenceValue + "='" + referenceValue + "']").evaluate(node.getOwnerDocument(),
								XPathConstants.NODE);
							xpathValue = xpathValue.substring(alternativeXpath.length() + 1);
							correctionSuccess = true;
							break top;
						}
					}
				}

				if (!correctionSuccess)
				{
					LOG.warn("root path " + rootXPath + " niet in xpathValue " + xpathValue + " voor " + declaredField);
				}
			}

			ConceptExceptionHandler handler = ConceptExceptionHandler.convertFromCode(vraagElement.code(), verslagType);
			if (handler != null)
			{
				returnValue = handler.getValue(new ConceptExceptionContext(verslagDao, declaredField, xpath, node, xpathValue));
			}
			else
			{
				returnValue = getValue(node, xpath, xpathValue, xpathMapping.extension, declaredField, verslagType);
			}

			if (vraagElement.isVerplicht() && returnValue == null)
			{
				LOG.info("geen value voor " + declaredField);
			}
		}
		else
		{
			ConceptExceptionHandler handler = ConceptExceptionHandler.convertFromCode(vraagElement.code(), verslagType);
			if (handler != null)
			{
				returnValue = handler.getValue(new ConceptExceptionContext(verslagDao, declaredField, xpath, node, ""));
			}
			else
			{
				LOG.warn("geen xpath voor " + declaredField + " code " + vraagElement.code() + " verslagType " + verslagType.name());
			}
		}

		PostConceptValueConverter converter = PostConceptValueConverter.convertFromCode(vraagElement.code(), verslagType);
		if (converter != null)
		{
			returnValue = converter.getValue(returnValue);
		}
		return returnValue;
	}

	private boolean isRootPartOfCurrentXpath(String rootXPath, String xpathValue)
	{
		return StringUtils.isNotBlank(rootXPath) && !rootXPath.equals(xpathValue) && xpathValue.contains(rootXPath);
	}

	private Object getValue(Node node, XPath xpath, String xpathValue, String extension, Field declaredField, VerslagType verslagType)
		throws XPathExpressionException, InstantiationException, IllegalAccessException
	{
		Object returnValue = null;
		Class<?> type = getType(declaredField);
		if (type.equals(Boolean.class))
		{
			Node booleanNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			if (booleanNode != null)
			{
				NamedNodeMap attributes = booleanNode.getAttributes();
				Node typeNode = attributes.getNamedItem("xsi:type");
				Node valueNode = attributes.getNamedItem("value");
				if (typeNode != null && valueNode != null && typeNode.getNodeValue().equals("BL")) 
				{
					returnValue = "true".equals(valueNode.getNodeValue());
				}
				else
				{
					returnValue = isNoNullFlavour(attributes);
				}

			}
		}
		else if (type.equals(String.class))
		{
			if (StringUtils.isNotBlank(extension))
			{
				if (!xpathValue.endsWith("/"))
				{
					xpathValue += "/";
				}
				xpathValue += extension;
			}
			returnValue = xpath.compile(xpathValue).evaluate(node);
		}
		else if (type.equals(Date.class))
		{
			returnValue = getDateValue(node, xpath, xpathValue);
		}
		else if (type.equals(Quantity.class))
		{
			Quantity quantity = (Quantity) type.newInstance();
			Node quantityNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			quantity.setValue(xpath.compile("@value").evaluate(quantityNode));
			quantity.setUnit(xpath.compile("@unit").evaluate(quantityNode));
			if (StringUtils.isNotBlank(quantity.getValue()))
			{
				returnValue = quantity;
			}
		}
		else if (type.equals(NullFlavourQuantity.class))
		{
			NullFlavourQuantity quantity = (NullFlavourQuantity) type.newInstance();
			Node quantityNode = (Node) xpath.compile(xpathValue.replace("[not(@nullFlavor)]", "")).evaluate(node, XPathConstants.NODE);
			quantity.setValue(xpath.compile("@value").evaluate(quantityNode));
			quantity.setUnit(xpath.compile("@unit").evaluate(quantityNode));
			if (quantityNode != null && StringUtils.isBlank(quantity.getValue()))
			{
				quantity.setUnit(null);
				quantity.setValue(null);
				Boolean noNullFlavour = isNoNullFlavour(quantityNode.getAttributes());
				quantity.setNullFlavour(!Boolean.TRUE.equals(noNullFlavour));
			}
			if (StringUtils.isNotBlank(quantity.getValue()) || quantity.getNullFlavour() != null)
			{
				returnValue = quantity;
			}
		}
		else if (type.equals(DSValue.class))
		{
			DSValueSet dsValueSet = declaredField.getAnnotation(DSValueSet.class);
			DSValue dsZoekObject = new DSValue();
			Node dsNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			dsZoekObject.setCode(xpath.compile("@code").evaluate(dsNode));
			dsZoekObject.setCodeSystem(xpath.compile("@codeSystem").evaluate(dsNode));
			dsZoekObject.setValueSetName(dsValueSet.name());

			dsZoekObjectCorrectie(declaredField, verslagType, dsZoekObject);

			returnValue = zoekDsValue(xpath, dsNode, dsZoekObject, verslagDao);
		}
		else if (type.equals(List.class))
		{
			returnValue = xpath.compile(xpathValue).evaluate(node, XPathConstants.NODESET);
		}
		return returnValue;
	}

	private Boolean isNoNullFlavour(NamedNodeMap attributes)
	{
		Boolean returnValue;
		Node nullFlavorNode = attributes.getNamedItem("nullFlavor");
		if (nullFlavorNode != null && ("UNK".equals(nullFlavorNode.getNodeValue()) || "NA".equals(nullFlavorNode.getNodeValue())))
		{
			returnValue = null;
		}
		else
		{
			Node negationIndNode = attributes.getNamedItem("negationInd");
			returnValue = !(negationIndNode != null && "true".equals(negationIndNode.getNodeValue()));
		}
		return returnValue;
	}

	private static Object zoekDsValue(XPath xpath, Node node, DSValue zoekValue, VerslagDao verslagDao) throws XPathExpressionException
	{
		DSValue returnValue = null;
		returnValue = verslagDao.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
		if (returnValue == null)
		{
			zoekValue.setCode(xpath.compile("@nullFlavor").evaluate(node));
			if ("OTH".equals(zoekValue.getCode()) || "UNK".equals(zoekValue.getCode()) || "ASKU".equals(zoekValue.getCode()))
			{
				zoekValue.setCodeSystem("2.16.840.1.113883.5.1008");
				returnValue = verslagDao.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
				if (returnValue == null)
				{
					zoekValue.setValueSetName(Constants.CDA_NULL_FLAVOR_VALUESET_NAME);
					returnValue = verslagDao.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
				}
			}
		}
		return returnValue;
	}

	private void dsZoekObjectCorrectie(Field declaredField, VerslagType verslagType, DSValue zoekValue)
	{
		if (zoekValue.getCode() != null)
		{
			VraagElement vraagElement = declaredField.getAnnotation(VraagElement.class);
			if (vraagElement.code().equals(getFullConceptId("145070", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "82035006": 
					zoekValue.setCode("309226005"); 
					break;

				case "86273004": 
					zoekValue.setCode("258415003"); 
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("125070", verslagType)))
			{

				switch (zoekValue.getCode())
				{

				case "12":
				case "13":
				case "14":
					zoekValue.setCode("OTH");
					zoekValue.setCodeSystem("2.16.840.1.113883.5.1008");
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("155050", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "13025001": 
					zoekValue.setCode("OTH");
					zoekValue.setCodeSystem("2.16.840.1.113883.5.1008");
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("140150", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "183654001":
					zoekValue.setCode("183851006");
					break;
				case "73761001:408730004=64695001":
					zoekValue.setCode("73761001:260870009=64695001");
					break;
				case "410410006:408730004=428119001":
					zoekValue.setCode("428119001:363589002=73761001");
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("145040", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "277163006":
					zoekValue.setCode("89452002");
					break;
				case "82375006":
					zoekValue.setCode("428054006");
					break;
				case "4":
					zoekValue.setCode("67401000119103:116676008=61647009");
					zoekValue.setCodeSystem("2.16.840.1.113883.6.96");
					break;
				case "128653004":
					zoekValue.setCode("449855005");
					break;
				case "35917007":
					zoekValue.setCode("408645001");
					break;

				}
			}
			else if (vraagElement.code().equals(getFullConceptId("125010", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "444783004:246513007=884001":
					zoekValue.setCode("444783004:246513007=261423007");
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("155030", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "82035006:246090004=395528004":
					zoekValue.setCode("15451000146101");
					break;
				}
			}
			else if (vraagElement.code().equals(getFullConceptId("155053", verslagType)))
			{

				switch (zoekValue.getCode())
				{
				case "269533000:408729009=415684004":
					zoekValue.setCode("162572001:246090004=269533000");
					break;
				}
			}
		}
	}

	private static Date getDateValue(Node node, XPath xpath, String xpathValue) throws XPathExpressionException
	{
		Date returnValue = null;
		String dateValue = xpath.compile(xpathValue + "/@value").evaluate(node);
		try
		{
			returnValue = CDAHelper.converCdaDateStringToDate(dateValue);
		}
		catch (ParseException e)
		{
			LOG.error("Fout bij parsen van value naar datum met xpath " + xpathValue, e);
		}
		return returnValue;
	}

	private static String getFullConceptId(String shortConceptId, VerslagType verslagType)
	{
		return verslagType.getConceptRootOid() + "." + VerslagGeneratie.getHuidigeGeneratie(verslagType).getVersionInConceptId() + "." + shortConceptId;
	}
}
