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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "afmelding")
@Audited
public class CervixAfmelding extends Afmelding<CervixScreeningRonde, CervixDossier, CervixBrief>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixScreeningRonde screeningRonde;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixDossier dossier;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief afmeldingAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief afmeldingBevestiging;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief heraanmeldAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief heraanmeldBevestiging;

	@OneToMany(mappedBy = "afmelding", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixBrief> brieven = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	private CervixAfmeldingReden reden;

	@Override
	public CervixBrief getAfmeldingAanvraag()
	{
		return afmeldingAanvraag;
	}

	@Override
	public void setAfmeldingAanvraag(CervixBrief afmeldingAanvraag)
	{
		this.afmeldingAanvraag = afmeldingAanvraag;
	}

	@Override
	public CervixBrief getAfmeldingBevestiging()
	{
		return afmeldingBevestiging;
	}

	@Override
	public void setAfmeldingBevestiging(CervixBrief afmeldingBevestiging)
	{
		this.afmeldingBevestiging = afmeldingBevestiging;
	}

	@Override
	public CervixBrief getHeraanmeldAanvraag()
	{
		return heraanmeldAanvraag;
	}

	@Override
	public void setHeraanmeldAanvraag(CervixBrief heraanmeldAanvraag)
	{
		this.heraanmeldAanvraag = heraanmeldAanvraag;
	}

	@Override
	public CervixBrief getHeraanmeldBevestiging()
	{
		return heraanmeldBevestiging;
	}

	@Override
	public void setHeraanmeldBevestiging(CervixBrief heraanmeldBevestiging)
	{
		this.heraanmeldBevestiging = heraanmeldBevestiging;
	}

	@Override
	public List<CervixBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<CervixBrief> brieven)
	{
		this.brieven = brieven;
	}

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

	@Override
	public CervixDossier getDossier()
	{
		return dossier;
	}

	@Override
	public void setDossier(CervixDossier dossier)
	{
		this.dossier = dossier;
	}

	public CervixAfmeldingReden getReden()
	{
		return reden;
	}

	public void setReden(CervixAfmeldingReden reden)
	{
		this.reden = reden;
	}
}
