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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "merged_brieven")
@Audited
public class MammaMergedBrieven extends MergedBrieven<MammaBrief>
{

	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "mergedBrieven", fetch = FetchType.LAZY)
	private List<MammaBrief> brieven = new ArrayList<>();

	@Override
	public List<MammaBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<MammaBrief> brieven)
	{
		this.brieven = brieven;
	}

}
