package nl.rivm.screenit.main.web.openid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.StringWriter;

import org.apache.commons.lang.StringUtils;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

public class XrdsDocumentBuilder
{

	private Element baseElement;

	private static final String MASTER_XRDS_NAMESPACE = "xri://$xrd*($v*2.0)";

	public XrdsDocumentBuilder()
	{
		Namespace xrdNS = Namespace.getNamespace(MASTER_XRDS_NAMESPACE);
		baseElement = new Element("XRD", xrdNS);
	}

	public void addServiceElement(String type, String uri, String priority)
	{
		addServiceElement(type, uri, priority, null);
	}

	private void addServiceElement(String type, String uri, String priority, String delegate)
	{
		Namespace xrdNS = Namespace.getNamespace(MASTER_XRDS_NAMESPACE);
		Namespace openidNS = Namespace.getNamespace("openid", "http://openid.net/xmlns/1.0");

		Element serviceElement = new Element("Service", xrdNS);
		Element typeElement = new Element("Type", xrdNS);
		typeElement.addContent(type);
		Element uriElement = new Element("URI", xrdNS);
		uriElement.addContent(uri);
		serviceElement.addContent(typeElement);
		serviceElement.addContent(uriElement);

		if (StringUtils.isNotEmpty(delegate))
		{
			Element delegateElement = new Element("Delegate", openidNS);
			delegateElement.addContent(delegate);
			serviceElement.addContent(delegateElement);
		}

		if (StringUtils.isNotEmpty(priority))
		{
			serviceElement.setAttribute("priority", priority);
		}
		baseElement.addContent(serviceElement);
	}

	public String toXmlString() throws IOException
	{
		Namespace xrdsNS = Namespace.getNamespace("xrds", "xri://$xrds");
		Element rootElement = new Element("XRDS", xrdsNS);
		rootElement.addContent(baseElement);
		Document doc = new Document(rootElement);
		StringWriter w = new StringWriter();
		XMLOutputter o = new XMLOutputter(Format.getPrettyFormat());
		o.output(doc, w);
		w.close();
		return w.toString();
	}

}
