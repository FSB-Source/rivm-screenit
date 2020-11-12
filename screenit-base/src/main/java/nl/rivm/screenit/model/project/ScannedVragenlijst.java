
package nl.rivm.screenit.model.project;

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
import java.util.Set;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ScannedVragenlijst extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	public static final String STATUS_AFGEHANDELD = "AFGEHANDELD";

	public static final String STATUS_ONBETROUWBAAR = "ONBETROUWBAAR";

	public static final String STATUS_VERWIJDERD = "VERWIJDERD";

	private String barcode;

	private Date scanDatum;

	private String objid;

	@ManyToOne
	private IFobtLaboratorium labId;

	private String status;

	@ElementCollection
	@Column(length = HibernateMagicNumber.L255)
	@CollectionTable(schema = "algemeen", name = "scanned_vragenlijst_fouten")
	private Set<String> fouten;

	public String getBarcode()
	{
		return barcode;
	}

	public void setBarcode(String barcode)
	{
		this.barcode = barcode;
	}

	public Date getScanDatum()
	{
		return scanDatum;
	}

	public void setScanDatum(Date scanDatum)
	{
		this.scanDatum = scanDatum;
	}

	public String getObjid()
	{
		return objid;
	}

	public void setObjid(String objid)
	{
		this.objid = objid;
	}

	public IFobtLaboratorium getLabId()
	{
		return labId;
	}

	public void setLabId(IFobtLaboratorium labId)
	{
		this.labId = labId;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public void setFouten(Set<String> fouten)
	{
		this.fouten = fouten;
	}
}
