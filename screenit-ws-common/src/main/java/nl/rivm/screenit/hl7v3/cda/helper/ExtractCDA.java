
package nl.rivm.screenit.hl7v3.cda.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-ws-common
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.ValidationEvent;
import javax.xml.bind.util.ValidationEventCollector;
import javax.xml.namespace.QName;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.sax.SAXSource;

import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;

import org.apache.xalan.processor.TransformerFactoryImpl;
import org.apache.xerces.jaxp.SAXParserFactoryImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

public class ExtractCDA
{

	private static final Logger LOG = LoggerFactory.getLogger(ExtractCDA.class);

	private static final String JAXP_SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";

	private static final String JAXP_SCHEMA_LOCATION = "http://java.sun.com/xml/jaxp/properties/schemaSource";

	private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

	private static JAXBContext jaxbContext;

	private ExtractCDA()
	{
	}

	static
	{
		initJAXBContext();
	}

	private static void initJAXBContext()
	{
		try
		{
			setSystemPropertiesForParserEnTransformer();

			jaxbContext = JAXBContext.newInstance("nl.rivm.screenit.hl7v3.cda");
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
	}

	private static void setSystemPropertiesForParserEnTransformer()
	{
		System.setProperty("javax.xml.parsers.SAXParserFactory", SAXParserFactoryImpl.class.getName());
		System.setProperty("javax.xml.transform.TransformerFactory", TransformerFactoryImpl.class.getName());
	}

	public static ClinicalDocument getCDADocument(String cda)
	{
		ClinicalDocument object = null;
		try
		{
			setSystemPropertiesForParserEnTransformer();

			Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
			ValidationEventCollector vec = new ValidationEventCollector();
			unmarshaller.setEventHandler(vec);

			object = (ClinicalDocument) unmarshaller.unmarshal(getSource(cda));

			if (vec.hasEvents())
			{
				ValidationEvent[] events = vec.getEvents();
				for (ValidationEvent event : events)
				{
					LOG.warn("Schema validation event: ", event.getMessage());
				}
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
		return object;
	}

	public static <T> String getJAXBToXMLString(T jaxbObject, QName qName, Class<T> clazz)
	{
		Writer writer = new StringWriter();
		try
		{
			setSystemPropertiesForParserEnTransformer();

			Marshaller marshaller = jaxbContext.createMarshaller();
			ValidationEventCollector vec = new ValidationEventCollector();
			marshaller.setEventHandler(vec);
			marshaller.setProperty("com.sun.xml.bind.xmlDeclaration", Boolean.FALSE);

			marshaller.marshal(new JAXBElement<T>(qName, clazz, jaxbObject), writer);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
		return writer.toString();
	}

	public static SAXSource getSource(String cda)
	{
		try
		{
			SAXParserFactory spf = SAXParserFactoryImpl.newInstance();
			spf.setNamespaceAware(true);
			spf.setValidating(true);

			spf.setFeature("http://xml.org/sax/features/external-general-entities", false);

			spf.setFeature("http://xml.org/sax/features/external-parameter-entities", false);

			spf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);

			spf.setFeature("http://javax.xml.XMLConstants/feature/secure-processing", true);

			SAXParser saxParser = spf.newSAXParser();
			try
			{
				saxParser.setProperty(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
				saxParser.setProperty(JAXP_SCHEMA_LOCATION, ExtractCDA.class.getResource("/CDA") + "/CDA.XSD");
			}
			catch (SAXNotRecognizedException x)
			{
				LOG.error(x.getMessage(), x);
			}

			XMLReader xmlReader = saxParser.getXMLReader();
			xmlReader.setFeature("http://xml.org/sax/features/external-general-entities", false);

			xmlReader.setFeature("http://xml.org/sax/features/external-parameter-entities", false);

			xmlReader.setErrorHandler(new ErrorHandler()
			{

				@Override
				public void warning(SAXParseException exception) throws SAXException
				{
					LOG.warn("SAX parsing", exception);
				}

				@Override
				public void fatalError(SAXParseException exception) throws SAXException
				{
					LOG.error("SAX parsing", exception);
				}

				@Override
				public void error(SAXParseException exception) throws SAXException
				{
					LOG.error("SAX parsing", exception);
				}
			});
			SAXSource source = new SAXSource(xmlReader, new InputSource(new StringReader(cda)));
			return source;
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
	}

}
