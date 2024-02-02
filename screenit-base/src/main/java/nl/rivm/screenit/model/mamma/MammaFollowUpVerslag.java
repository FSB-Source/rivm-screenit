package nl.rivm.screenit.model.mamma;

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
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;

import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Proxy(lazy = true)
@Table(
	uniqueConstraints = { @UniqueConstraint(columnNames = "verslagContent") })
@Audited
public class MammaFollowUpVerslag extends MammaVerslag<MammaFollowUpVerslagContent>
{
	private static final long serialVersionUID = 1L;

	@OneToOne(optional = false, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@NotAudited
	private MammaFollowUpVerslagContent verslagContent;

	@Override
	public MammaFollowUpVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	@Override
	public void setVerslagContent(MammaFollowUpVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

}
