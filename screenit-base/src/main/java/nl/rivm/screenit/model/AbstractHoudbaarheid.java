
package nl.rivm.screenit.model;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public abstract class AbstractHoudbaarheid extends TablePerClassHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private String barcodeStart;

	@Column(nullable = false)
	private String barcodeEnd;

	@Column(nullable = false)
	private Integer lengthBarcode;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date vervalDatum;

	public String getBarcodeStart()
	{
		return barcodeStart;
	}

	public void setBarcodeStart(String barcodeStart)
	{
		this.barcodeStart = barcodeStart;
	}

	public String getBarcodeEnd()
	{
		return barcodeEnd;
	}

	public void setBarcodeEnd(String barcodeEnd)
	{
		this.barcodeEnd = barcodeEnd;
	}

	public Date getVervalDatum()
	{
		return vervalDatum;
	}

	public void setVervalDatum(Date vervalDatum)
	{
		this.vervalDatum = vervalDatum;
	}

	public Integer getLengthBarcode()
	{
		return lengthBarcode;
	}

	public void setLengthBarcode(Integer lengthBarcode)
	{
		this.lengthBarcode = lengthBarcode;
	}

}
