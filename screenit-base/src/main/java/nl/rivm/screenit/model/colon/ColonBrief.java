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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_colon_brief_gegenereerd", columnList = "gegenereerd"),
	@Index(name = "idx_colon_brief_vervangendeprojectbrief", columnList = "vervangendeprojectbrief") })
@Audited
@Getter
@Setter
public class ColonBrief extends ClientBrief<ColonScreeningRonde, ColonAfmelding, ColonBrief>
{
	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private ColonIntakeAfspraak intakeAfspraak;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private ColonIntakeAfspraak vorigeIntakeAfspraak;

	@ManyToOne(optional = true, fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private IFOBTTest ifobtTest;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ColonMergedBrieven mergedBrieven;

	@ManyToOne(optional = true, fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ColonScreeningRonde screeningRonde;

	@ManyToOne(optional = true, fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ColonAfmelding afmelding;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private ColonBrief herdruk;

	@Override
	public MergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (ColonMergedBrieven) mergedBrieven;
	}
}
