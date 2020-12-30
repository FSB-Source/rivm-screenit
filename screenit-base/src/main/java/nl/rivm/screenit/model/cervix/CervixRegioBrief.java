package nl.rivm.screenit.model.cervix;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningOrganisatie;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "regio_brief", indexes = { @Index(name = "idx_cervix_regio_brief_gegenereerd", columnList = "gegenereerd") })
@Audited
public class CervixRegioBrief extends Brief
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	private ScreeningOrganisatie regio;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixHuisarts huisarts;

	@ManyToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private CervixRegioMergedBrieven mergedBrieven;

	@Override
	public CervixRegioMergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	public void setMergedBrieven(CervixRegioMergedBrieven mergedBrieven)
	{
		this.mergedBrieven = mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (CervixRegioMergedBrieven) mergedBrieven;
	}

	public ScreeningOrganisatie getRegio()
	{
		return regio;
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = regio;
	}

	public CervixHuisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(CervixHuisarts cervixHuisarts)
	{
		this.huisarts = cervixHuisarts;
	}

}
