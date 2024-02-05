package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class PlanningStandplaatsPeriode extends PlanningConceptEntiteit
{

	private final Lock lock = new ReentrantLock();

	private final Integer standplaatsRondeVolgNr;

	private final Set<PlanningBlokkade> blokkadeNavigableSet = new HashSet<>();

	private PlanningStandplaatsRonde standplaatsRonde;

	private PlanningScreeningsEenheid screeningsEenheid;

	private Integer screeningsEenheidVolgNr;

	private LocalDate vanaf;

	private LocalDate totEnMet;

	private Boolean prognose;

	private BigDecimal somGewogenDatum = BigDecimal.ZERO;

	private BigDecimal beschikbaarTotaal = BigDecimal.ZERO;

	private BigDecimal beschikbaarTehuis = BigDecimal.ZERO;

	private BigDecimal beschikbaarVoorJaarovergangTotaal = BigDecimal.ZERO;

	private BigDecimal beschikbaarVoorJaarovergangTehuis = BigDecimal.ZERO;

	public PlanningStandplaatsPeriode(Long id, Integer screeningsEenheidVolgNr, Integer standplaatsRondeVolgNr, LocalDate vanaf, Boolean prognose, LocalDate totEnMet)
	{
		super(id);
		this.screeningsEenheidVolgNr = screeningsEenheidVolgNr;
		this.standplaatsRondeVolgNr = standplaatsRondeVolgNr;
		this.vanaf = vanaf;
		this.prognose = prognose;
		this.totEnMet = totEnMet;
	}

	public void lock()
	{
		LOG.trace("Lock SP: " + getId());
		lock.lock();
	}

	public void unlock()
	{
		LOG.trace("Unlock SP: " + getId());
		lock.unlock();
	}

	public void await()
	{
		LOG.trace("Await SP: " + getId());
		lock.lock();
		lock.unlock();
	}

	public PlanningStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(PlanningStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}

	public PlanningScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(PlanningScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public LocalDate getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(LocalDate vanaf)
	{
		this.vanaf = vanaf;
	}

	public LocalDate getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(LocalDate totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public Integer getScreeningsEenheidVolgNr()
	{
		return screeningsEenheidVolgNr;
	}

	public void setScreeningsEenheidVolgNr(Integer screeningsEenheidVolgNr)
	{
		this.screeningsEenheidVolgNr = screeningsEenheidVolgNr;
	}

	public Integer getStandplaatsRondeVolgNr()
	{
		return standplaatsRondeVolgNr;
	}

	public Set<PlanningBlokkade> getBlokkadeNavigableSet()
	{
		return blokkadeNavigableSet;
	}

	public Boolean getPrognose()
	{
		return prognose;
	}

	public void setPrognose(Boolean prognose)
	{
		this.prognose = prognose;
	}

	public BigDecimal getSomGewogenDatum()
	{
		return somGewogenDatum;
	}

	public BigDecimal getBeschikbaarTotaal()
	{
		return beschikbaarTotaal;
	}

	public BigDecimal getBeschikbaarTehuis()
	{
		return beschikbaarTehuis;
	}

	public void setBeschikbaarTehuis(BigDecimal beschikbaarTehuis)
	{
		this.beschikbaarTehuis = beschikbaarTehuis;
	}

	public BigDecimal getBeschikbaarVoorJaarovergangTotaal()
	{
		return beschikbaarVoorJaarovergangTotaal;
	}

	public BigDecimal getBeschikbaarVoorJaarovergangTehuis()
	{
		return beschikbaarVoorJaarovergangTehuis;
	}

	public boolean gesplitst()
	{
		return getStandplaatsRonde().getStandplaatsPeriodeNavigableSet().higher(this) != null;
	}

	public void clear()
	{
		this.somGewogenDatum = BigDecimal.ZERO;
		this.beschikbaarTotaal = BigDecimal.ZERO;
		this.beschikbaarTehuis = BigDecimal.ZERO;
		this.beschikbaarVoorJaarovergangTotaal = BigDecimal.ZERO;
		this.beschikbaarVoorJaarovergangTehuis = BigDecimal.ZERO;
	}

	public void add(long epochDay, BigDecimal beschikbaarTotaal, BigDecimal beschikbaarTehuis, boolean voorJaarovergang)
	{
		this.somGewogenDatum = this.somGewogenDatum.add(new BigDecimal(epochDay).multiply(beschikbaarTotaal));
		this.beschikbaarTotaal = this.beschikbaarTotaal.add(beschikbaarTotaal);
		this.beschikbaarTehuis = this.beschikbaarTehuis.add(beschikbaarTehuis);

		if (voorJaarovergang)
		{
			this.beschikbaarVoorJaarovergangTotaal = this.beschikbaarVoorJaarovergangTotaal.add(beschikbaarTotaal);
			this.beschikbaarVoorJaarovergangTehuis = this.beschikbaarVoorJaarovergangTehuis.add(beschikbaarTehuis);
		}
	}
}
