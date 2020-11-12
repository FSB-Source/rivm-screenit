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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "brief", indexes = { @Index(name = "idx_cervix_brief_gegenereerd", columnList = "gegenereerd"),
	@Index(name = "idx_cervix_brief_vervangendeprojectbrief", columnList = "vervangendeprojectbrief") })
@Audited
public class CervixBrief extends ClientBrief<CervixScreeningRonde, CervixAfmelding, CervixBrief>
{
	
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixScreeningRonde screeningRonde;

	@OneToOne(mappedBy = "brief", fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodiging;

	@OneToOne(mappedBy = "brief", fetch = FetchType.LAZY)
	private CervixMonster monster;

	@OneToOne(mappedBy = "huisartsOnbekendBrief", fetch = FetchType.LAZY)
	private CervixLabformulier labformulier;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixAfmelding afmelding;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief herdruk;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixMergedBrieven mergedBrieven;

	@Enumerated(EnumType.STRING)
	private PreferenceKey heraanmeldenTekstKey;

	@Enumerated(EnumType.STRING)
	private CervixOmissieType omissieType;

	@Transient
	private transient boolean aangevraagdeHerdruk = false;

	@Override
	public CervixScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(CervixScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public CervixUitnodiging getUitnodiging()
	{
		return uitnodiging;
	}

	public void setUitnodiging(CervixUitnodiging uitnodiging)
	{
		this.uitnodiging = uitnodiging;
	}

	public CervixMonster getMonster()
	{
		return monster;
	}

	public void setMonster(CervixMonster monster)
	{
		this.monster = monster;
	}

	@Override
	public CervixAfmelding getAfmelding()
	{
		return afmelding;
	}

	@Override
	public void setAfmelding(CervixAfmelding afmelding)
	{
		this.afmelding = afmelding;
	}

	@Override
	public CervixMergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	public void setMergedBrieven(CervixMergedBrieven mergedBrieven)
	{
		this.mergedBrieven = mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (CervixMergedBrieven) mergedBrieven;
	}

	public PreferenceKey getHeraanmeldenTekstKey()
	{
		return heraanmeldenTekstKey;
	}

	public void setHeraanmeldenTekstKey(PreferenceKey heraanmeldenTekstKey)
	{
		this.heraanmeldenTekstKey = heraanmeldenTekstKey;
	}

	public CervixLabformulier getLabformulier()
	{
		return labformulier;
	}

	public void setLabformulier(CervixLabformulier labformulier)
	{
		this.labformulier = labformulier;
	}

	@Override
	public CervixBrief getHerdruk()
	{
		return herdruk;
	}

	@Override
	public void setHerdruk(CervixBrief herdruk)
	{
		this.herdruk = herdruk;
	}

	public boolean isAangevraagdeHerdruk()
	{
		return aangevraagdeHerdruk;
	}

	public void setAangevraagdeHerdruk(boolean aangevraagdeHerdruk)
	{
		this.aangevraagdeHerdruk = aangevraagdeHerdruk;
	}

	public CervixOmissieType getOmissieType()
	{
		return omissieType;
	}

	public void setOmissieType(CervixOmissieType omissieType)
	{
		this.omissieType = omissieType;
	}
}
