
package nl.rivm.screenit.model.colon;

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

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;

import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity(name = "mdlverslag")
@Proxy(lazy = true)
@Audited
public class MdlVerslag extends ColonVerslag<MdlVerslagContent>
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private MdlVervolgbeleid vervolgbeleid;

	@Column(length = HibernateMagicNumber.L255)
	private String patientnummer;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, optional = false)
	@ForeignKey(name = "none")
	@NotAudited
	private MdlVerslagContent verslagContent;

	public MdlVervolgbeleid getVervolgbeleid()
	{
		return vervolgbeleid;
	}

	public void setVervolgbeleid(MdlVervolgbeleid vervolgbeleid)
	{
		this.vervolgbeleid = vervolgbeleid;
	}

	public String getPatientnummer()
	{
		return patientnummer;
	}

	public void setPatientnummer(String patientnummer)
	{
		this.patientnummer = patientnummer;
	}

	@Override
	public MdlVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	@Override
	public void setVerslagContent(MdlVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}
}
