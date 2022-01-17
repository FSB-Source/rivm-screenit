package nl.rivm.screenit.model.algemeen;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRonde;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Audited
public class BezwaarBrief extends ClientBrief<ScreeningRonde, Afmelding, BezwaarBrief>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private BezwaarMoment bezwaarMoment;

	@ManyToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private BezwaarMergedBrieven mergedBrieven;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private BezwaarBrief herdruk;

	public BezwaarMoment getBezwaarMoment()
	{
		return bezwaarMoment;
	}

	public void setBezwaarMoment(BezwaarMoment bezwaarMoment)
	{
		this.bezwaarMoment = bezwaarMoment;
	}

	@Override
	public Afmelding getAfmelding()
	{
		return null;
	}

	@Override
	public void setAfmelding(Afmelding afmelding)
	{

	}

	@Override
	public ScreeningRonde getScreeningRonde()
	{
		return null;
	}

	@Override
	public void setScreeningRonde(ScreeningRonde screeningRonde)
	{

	}

	@Override
	public MergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (BezwaarMergedBrieven) mergedBrieven;
	}

	@Override
	public BezwaarBrief getHerdruk()
	{
		return herdruk;
	}

	@Override
	public void setHerdruk(BezwaarBrief herdruk)
	{
		this.herdruk = herdruk;
	}
}
