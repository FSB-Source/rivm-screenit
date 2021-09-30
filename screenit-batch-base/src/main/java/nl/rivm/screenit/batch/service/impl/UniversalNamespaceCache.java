package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class UniversalNamespaceCache implements NamespaceContext
{

	private Map<String, String> prefix2Uri = new HashMap<String, String>();

	private Map<String, String> uri2Prefix = new HashMap<String, String>();

	private final String defaultNS;

	public UniversalNamespaceCache(Document document, String defaultNS)
	{
		this.defaultNS = defaultNS;
		examineNode(document);
	}

	private void examineNode(Node node)
	{
		NamedNodeMap attributes = node.getAttributes();
		if (attributes != null)
		{
			for (int i = 0; i < attributes.getLength(); i++)
			{
				Node attribute = attributes.item(i);
				storeAttribute((Attr) attribute);
			}
		}
		NodeList chields = node.getChildNodes();
		for (int i = 0; i < chields.getLength(); i++)
		{
			Node chield = chields.item(i);
			if (chield.getNodeType() == Node.ELEMENT_NODE)
			{
				examineNode(chield);
			}
		}
	}

	private void storeAttribute(Attr attribute)
	{

		if (attribute.getNamespaceURI() != null && attribute.getNamespaceURI().equals(XMLConstants.XMLNS_ATTRIBUTE_NS_URI))
		{

			if (attribute.getNodeName().equals(XMLConstants.XMLNS_ATTRIBUTE))
			{
				putInCache(defaultNS, attribute.getNodeValue());
			}
			else
			{

				putInCache(attribute.getLocalName(), attribute.getNodeValue());
			}
		}

	}

	private void putInCache(String prefix, String uri)
	{
		prefix2Uri.put(prefix, uri);
		uri2Prefix.put(uri, prefix);
	}

	@Override
	public String getNamespaceURI(String prefix)
	{
		if (prefix == null || prefix.equals(XMLConstants.DEFAULT_NS_PREFIX))
		{
			return prefix2Uri.get(defaultNS);
		}
		else
		{
			return prefix2Uri.get(prefix);
		}
	}

	@Override
	public String getPrefix(String namespaceURI)
	{
		return uri2Prefix.get(namespaceURI);
	}

	@Override
	public Iterator<String> getPrefixes(String namespaceURI)
	{

		return null;
	}

}
