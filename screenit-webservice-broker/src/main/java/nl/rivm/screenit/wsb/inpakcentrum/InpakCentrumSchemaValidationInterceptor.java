package nl.rivm.screenit.wsb.inpakcentrum;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;
import java.io.StringReader;
import java.net.URL;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import nl.rivm.screenit.ws.inpakcentrum.KoppelData;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.binding.xml.XMLFault;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class InpakCentrumSchemaValidationInterceptor extends AbstractSoapInterceptor
{

	private static final Logger LOG = LoggerFactory.getLogger(InpakCentrumSchemaValidationInterceptor.class);

	private String schemaFile;

	public InpakCentrumSchemaValidationInterceptor()
	{
		super(Phase.POST_LOGICAL);
	}

	@Override
	public void handleMessage(SoapMessage message) throws Fault
	{
		URL schemaFileUrl = InpakCentrumSchemaValidationInterceptor.class.getResource(schemaFile);
		SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		Schema schema;
		try
		{
			schema = schemaFactory.newSchema(schemaFileUrl);
		}
		catch (SAXException e)
		{
			LOG.error("Error loading schema", e);
			throw new XMLFault("Schema file for validation not found");
		}
		Validator validator = schema.newValidator();
		String xml = null;
		try
		{
			List<?> messageContentsList = message.getContent(List.class);
			if (messageContentsList != null)
			{
				for (Object messageContent : messageContentsList)
				{
					if (messageContent instanceof KoppelData)
					{
						KoppelData koppelData = (KoppelData) messageContent;
						xml = new String(koppelData.getData());
						break;
					}
				}
			}

			if (xml == null)
			{
				LOG.error("Schema validation failed: kan geen XML document in SOAP vinden.");
				throw new XMLFault("Schema validation failed: kan geen XML document in SOAP vinden.");
			}

			validator.validate(new StreamSource(new StringReader(xml)));
		}
		catch (SAXParseException e)
		{
			LOG.error("Schema validation failed", e);
			throw new XMLFault("Schema validation failed " + e.getMessage());
		}
		catch (SAXException | IOException e)
		{
			LOG.error("Schema validation failed", e);
			throw new XMLFault("Schema validation failed");
		}
	}

	public void setSchemaFile(String schemaFile)
	{
		this.schemaFile = schemaFile;
	}

}
