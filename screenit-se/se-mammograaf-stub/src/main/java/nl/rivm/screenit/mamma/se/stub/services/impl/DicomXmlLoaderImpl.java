package nl.rivm.screenit.mamma.se.stub.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-mammograaf-stub
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import javax.xml.parsers.SAXParserFactory;

import nl.rivm.screenit.mamma.se.stub.services.DicomXmlLoader;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.io.ContentHandlerAdapter;
import org.springframework.stereotype.Service;

@Service
public class DicomXmlLoaderImpl implements DicomXmlLoader
{
	@Override
	public String loadResourceAsString(String xmlFileName) throws IOException
	{
		var xmlFile = Objects.requireNonNull(getClass().getResourceAsStream("/" + xmlFileName));
		return new String(xmlFile.readAllBytes(), StandardCharsets.UTF_8);
	}

	@Override
	public Attributes loadDicomFromResource(String xmlFileName) throws Exception
	{
		var inputStream = getClass().getResourceAsStream("/" + xmlFileName);
		return loadXml(inputStream);
	}

	@Override
	public Attributes loadDicomFromXmlString(String dicomXmlDocument) throws Exception
	{
		return loadXml(new ByteArrayInputStream(dicomXmlDocument.getBytes()));
	}

	private Attributes loadXml(InputStream is) throws Exception
	{
		var dicomObject = new Attributes();
		var factory = SAXParserFactory.newInstance();
		factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
		factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
		var saxParser = factory.newSAXParser();

		var contentHandlerAdapter = new ContentHandlerAdapter(dicomObject);
		saxParser.parse(is, contentHandlerAdapter);
		return dicomObject;
	}
}
