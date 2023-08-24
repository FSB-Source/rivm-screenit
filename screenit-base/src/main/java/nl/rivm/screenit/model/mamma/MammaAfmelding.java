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
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "afmelding")
@Audited
public class MammaAfmelding extends Afmelding<MammaScreeningRonde, MammaDossier, MammaBrief>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private MammaScreeningRonde screeningRonde;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private MammaDossier dossier;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.DELETE)
	private MammaBrief afmeldingAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.DELETE)
	private MammaBrief afmeldingBevestiging;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.DELETE)
	private MammaBrief heraanmeldAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.DELETE)
	private MammaBrief heraanmeldBevestiging;

	@OneToMany(mappedBy = "afmelding", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	private List<MammaBrief> brieven = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	private MammaAfmeldingReden reden;

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

	@Override
	public MammaDossier getDossier()
	{
		return dossier;
	}

	@Override
	public void setDossier(MammaDossier dossier)
	{
		this.dossier = dossier;
	}

	@Override
	public MammaBrief getAfmeldingAanvraag()
	{
		return afmeldingAanvraag;
	}

	@Override
	public void setAfmeldingAanvraag(MammaBrief afmeldingAanvraag)
	{
		this.afmeldingAanvraag = afmeldingAanvraag;
	}

	@Override
	public MammaBrief getAfmeldingBevestiging()
	{
		return afmeldingBevestiging;
	}

	@Override
	public void setAfmeldingBevestiging(MammaBrief afmeldingBevestiging)
	{
		this.afmeldingBevestiging = afmeldingBevestiging;
	}

	@Override
	public MammaBrief getHeraanmeldAanvraag()
	{
		return heraanmeldAanvraag;
	}

	@Override
	public void setHeraanmeldAanvraag(MammaBrief heraanmeldAanvraag)
	{
		this.heraanmeldAanvraag = heraanmeldAanvraag;
	}

	@Override
	public MammaBrief getHeraanmeldBevestiging()
	{
		return heraanmeldBevestiging;
	}

	@Override
	public void setHeraanmeldBevestiging(MammaBrief heraanmeldBevestiging)
	{
		this.heraanmeldBevestiging = heraanmeldBevestiging;
	}

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

	public MammaAfmeldingReden getReden()
	{
		return reden;
	}

	public void setReden(MammaAfmeldingReden reden)
	{
		this.reden = reden;
	}
}
