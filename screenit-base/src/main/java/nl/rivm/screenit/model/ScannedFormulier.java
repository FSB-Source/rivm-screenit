
package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class ScannedFormulier extends TablePerClassHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date scanDatum;

	private String barcode;

	@Column(nullable = false)
	private String objid;

	public Date getScanDatum()
	{
		return scanDatum;
	}

	public void setScanDatum(Date scanDatum)
	{
		this.scanDatum = scanDatum;
	}

	public String getBarcode()
	{
		return barcode;
	}

	public void setBarcode(String barcode)
	{
		this.barcode = barcode;
	}

	public String getObjid()
	{
		return objid;
	}

	public void setObjid(String objid)
	{
		this.objid = objid;
	}
}
