
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import nl.rivm.screenit.model.Instelling;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
public class IFobtLaboratorium extends Instelling
{

	private static final long serialVersionUID = 1L;

	private String labId;

	private String labIdScanner;

	private String qbasenummer;

	@ManyToOne(cascade = CascadeType.ALL)
	@NotAudited
	private AntedateerRange laatsteAntedateerRange;

	public String getLabId()
	{
		return labId;
	}

	public void setLabId(String labId)
	{
		this.labId = labId;
	}

	public String getLabIdScanner()
	{
		return labIdScanner;
	}

	public void setLabIdScanner(String labIdScanner)
	{
		this.labIdScanner = labIdScanner;
	}

	public String getQbasenummer()
	{
		return qbasenummer;
	}

	public void setQbasenummer(String qbasenummer)
	{
		this.qbasenummer = qbasenummer;
	}

	public AntedateerRange getLaatsteAnteDateerRange()
	{
		return laatsteAntedateerRange;
	}

	public void setLaatsteAnteDateerRange(AntedateerRange laatsteAnteDateerRange)
	{
		this.laatsteAntedateerRange = laatsteAnteDateerRange;
	}
}
