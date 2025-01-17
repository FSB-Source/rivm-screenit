package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.Charset;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;

import org.apache.commons.io.input.ReaderInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CdaTransformerHelper
{

	private static final Logger LOG = LoggerFactory.getLogger(CdaTransformerHelper.class);

	private CdaTransformerHelper()
	{
	}

	public static String cdaToHtml(Source documentSource, String projectVersion, VerslagType verslagType)
	{
		InputStream cdaXsltStream = null;
		try
		{
			VerslagGeneratie verslagVersie = VerslagProjectVersionMapping.get().getGeneratie(projectVersion, verslagType);
			if (verslagVersie != null)
			{
				cdaXsltStream = CdaTransformerHelper.class.getResourceAsStream(verslagVersie.getXsltResource());
			}

			Source cdaXsltSource = new StreamSource(cdaXsltStream);

			TransformerFactory fact = TransformerFactory.newInstance();
			Transformer transformer = fact.newTransformer(cdaXsltSource);

			try (ByteArrayOutputStream resultStream = new ByteArrayOutputStream())
			{
				transformer.transform(documentSource, new StreamResult(resultStream));
				return resultStream.toString();
			}
		}
		catch (TransformerFactoryConfigurationError | TransformerException | NullPointerException | IOException e)
		{
			LOG.error(e.getMessage(), e);
			return "";
		}
		finally
		{
			try
			{
				cdaXsltStream.close();
			}
			catch (Exception e)
			{
			}
		}
	}

	public static String cdaToHtml(InputStream documentStream, String projectVersion, VerslagType verslagType) throws TransformerException
	{
		return cdaToHtml(new StreamSource(documentStream), projectVersion, verslagType);
	}

	public static String cdaToHtml(OntvangenCdaBericht ontvangenCdaBericht)
	{
		String document = ontvangenCdaBericht.getXmlBericht();
		String projectVersion = ontvangenCdaBericht.getProjectVersion();
		VerslagType verslagType = ontvangenCdaBericht.getBerichtType().getVerslagType();
		if (document != null && !document.trim().isEmpty())
		{
			try
			{
				return cdaToHtml(new ReaderInputStream(new StringReader(document), Charset.defaultCharset()), projectVersion, verslagType);
			}
			catch (TransformerException e)
			{
				LOG.error("Fout bij vertalen van CDA XML naar HTML.", e);
				return "";
			}
		}
		else
		{
			return "";
		}
	}
}
