package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "kansberekening_afspraak_event")
@Audited
public class MammaKansberekeningAfspraakEvent extends MammaKansberekeningEvent
{
	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "afspraakEvent", optional = false)
	private MammaAfspraak afspraak;

	@Column(nullable = true)
	private Boolean opkomst;

	@Column(nullable = false)
	private Integer maand;

	@Column(nullable = false)
	private Integer uur;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaVerzettenReden verzettenReden;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BriefType briefTypeUitnodiging;

	@Column(nullable = false)
	private Boolean naHerinnering;

	@Column(nullable = false)
	private Boolean naHeraanmelding;

	@Column(nullable = false)
	private Boolean rondeGeforceerd;

	public void setAfspraak(MammaAfspraak afspraak)
	{
		this.afspraak = afspraak;
	}

	public void setMaand(Integer maand)
	{
		this.maand = maand;
	}

	public void setUur(Integer uur)
	{
		this.uur = uur;
	}

	public void setVerzettenReden(MammaVerzettenReden verzettenReden)
	{
		this.verzettenReden = verzettenReden;
	}

	public void setBriefTypeUitnodiging(BriefType briefTypeUitnodiging)
	{
		this.briefTypeUitnodiging = briefTypeUitnodiging;
	}

	public void setNaHerinnering(Boolean naHerinnering)
	{
		this.naHerinnering = naHerinnering;
	}

	public void setNaHeraanmelding(Boolean naHeraanmelding)
	{
		this.naHeraanmelding = naHeraanmelding;
	}

	public MammaAfspraak getAfspraak()
	{
		return afspraak;
	}

	public Integer getMaand()
	{
		return maand;
	}

	public Integer getUur()
	{
		return uur;
	}

	public MammaVerzettenReden getVerzettenReden()
	{
		return verzettenReden;
	}

	public BriefType getBriefTypeUitnodiging()
	{
		return briefTypeUitnodiging;
	}

	public Boolean getNaHerinnering()
	{
		return naHerinnering;
	}

	public Boolean getNaHeraanmelding()
	{
		return naHeraanmelding;
	}

	public Boolean getOpkomst()
	{
		return opkomst;
	}

	public void setOpkomst(Boolean opkomst)
	{
		this.opkomst = opkomst;
	}

	public Boolean getRondeGeforceerd()
	{
		return rondeGeforceerd;
	}

	public void setRondeGeforceerd(Boolean rondeGeforceerd)
	{
		this.rondeGeforceerd = rondeGeforceerd;
	}
}
