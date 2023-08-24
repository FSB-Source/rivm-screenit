package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "brief", indexes = { @Index(name = "idx_mamma_brief_gegenereerd", columnList = "gegenereerd"),
	@Index(name = "idx_mamma_brief_vervangendeprojectbrief", columnList = "vervangendeprojectbrief") })
@Audited
public class MammaBrief extends ClientBrief<MammaScreeningRonde, MammaAfmelding, MammaBrief>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private MammaScreeningRonde screeningRonde;

	@OneToOne(mappedBy = "brief", fetch = FetchType.LAZY, optional = true)
	private MammaUitnodiging uitnodiging;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfmelding afmelding;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private MammaBrief herdruk;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private MammaMergedBrieven mergedBrieven;

	@Override
	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public MammaUitnodiging getUitnodiging()
	{
		return uitnodiging;
	}

	public void setUitnodiging(MammaUitnodiging uitnodiging)
	{
		this.uitnodiging = uitnodiging;
	}

	@Override
	public MammaAfmelding getAfmelding()
	{
		return afmelding;
	}

	@Override
	public void setAfmelding(MammaAfmelding afmelding)
	{
		this.afmelding = afmelding;
	}

	@Override
	public MammaBrief getHerdruk()
	{
		return herdruk;
	}

	@Override
	public void setHerdruk(MammaBrief herdruk)
	{
		this.herdruk = herdruk;
	}

	@Override
	public MammaMergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (MammaMergedBrieven) mergedBrieven;
	}

	public void setMergedBrieven(MammaMergedBrieven mergedBrieven)
	{
		this.mergedBrieven = mergedBrieven;
	}
}
