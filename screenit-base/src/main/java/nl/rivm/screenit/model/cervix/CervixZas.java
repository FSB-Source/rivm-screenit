package nl.rivm.screenit.model.cervix;

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

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class CervixZas extends CervixMonster
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private CervixZasStatus zasStatus;

	@Temporal(TemporalType.DATE)
	private Date verstuurd;

	public CervixZasStatus getZasStatus()
	{
		return zasStatus;
	}

	public void setZasStatus(CervixZasStatus zasStatus)
	{
		this.zasStatus = zasStatus;
	}

	public Date getVerstuurd()
	{
		return verstuurd;
	}

	public void setVerstuurd(Date verstuurd)
	{
		this.verstuurd = verstuurd;
	}
}
