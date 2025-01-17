
package nl.rivm.screenit.model.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class KoppelData extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	private String filename;

	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangen;

	@Lob
	@Deprecated
	private byte[] xmlData;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String xmlBericht;

	@Deprecated
	public byte[] getXmlData()
	{
		return xmlData;
	}

	@Deprecated
	public void setXmlData(byte[] xmlData)
	{
		if (xmlData != null)
		{
			this.xmlData = xmlData.clone();
		}
		else
		{
			this.xmlData = null;
		}
	}

	public Date getOntvangen()
	{
		return ontvangen;
	}

	public void setOntvangen(Date ontvangen)
	{
		this.ontvangen = ontvangen;
	}

	public String getFilename()
	{
		return filename;
	}

	public void setFilename(String filename)
	{
		this.filename = filename;
	}

	public String getXmlBericht()
	{
		return xmlBericht;
	}

	public void setXmlBericht(String xmlBericht)
	{
		this.xmlBericht = xmlBericht;
	}

}
