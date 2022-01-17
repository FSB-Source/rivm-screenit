package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalTime;
import java.util.Date;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.DateUtil;

public class PlanningBlok extends PlanningConceptEntiteit
{
	private LocalTime vanaf;

	private LocalTime tot;

	private int aantalOnderzoeken;

	private final PlanningBeschikbaar beschikbaar = new PlanningBeschikbaar();

	private MammaCapaciteitBlokType capaciteitBlokType;

	private PlanningScreeningsEenheid screeningsEenheid;

	private PlanningDag dag;

	private String opmerkingen;

	private boolean minderValideAfspraakMogelijk;

	public PlanningBlok(Long id, LocalTime vanaf, LocalTime tot, int aantalOnderzoeken, MammaCapaciteitBlokType capaciteitBlokType, String opmerkingen,
		boolean minderValideAfspraakMogelijk)
	{
		super(id);
		this.vanaf = vanaf;
		this.tot = tot;
		this.aantalOnderzoeken = aantalOnderzoeken;
		this.capaciteitBlokType = capaciteitBlokType;
		this.opmerkingen = opmerkingen;
		this.minderValideAfspraakMogelijk = minderValideAfspraakMogelijk;
	}

	public void setScreeningsEenheid(PlanningScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public PlanningScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public LocalTime getVanaf()
	{
		return vanaf;
	}

	public Date getDateVanaf()
	{
		return DateUtil.toUtilDate(vanaf, dag.getDatum());
	}

	public void setVanaf(LocalTime vanaf)
	{
		this.vanaf = vanaf;
	}

	public LocalTime getTot()
	{
		return tot;
	}

	public Date getDateTot()
	{
		return DateUtil.toUtilDate(tot, dag.getDatum());
	}

	public void setTot(LocalTime tot)
	{
		this.tot = tot;
	}

	public PlanningBeschikbaar getBeschikbaar()
	{
		return beschikbaar;
	}

	public MammaCapaciteitBlokType getCapaciteitBlokType()
	{
		return capaciteitBlokType;
	}

	public void setCapaciteitBlokType(MammaCapaciteitBlokType capaciteitBlokType)
	{
		this.capaciteitBlokType = capaciteitBlokType;
	}

	public void setAantalOnderzoeken(int aantalOnderzoeken)
	{
		this.aantalOnderzoeken = aantalOnderzoeken;
	}

	public PlanningDag getDag()
	{
		return dag;
	}

	public void setDag(PlanningDag dag)
	{
		this.dag = dag;
	}

	public String getOpmerkingen()
	{
		return opmerkingen;
	}

	public void setOpmerkingen(String opmerkingen)
	{
		this.opmerkingen = opmerkingen;
	}

	public int getAantalOnderzoeken()
	{
		return aantalOnderzoeken;
	}

	public boolean isMinderValideAfspraakMogelijk()
	{
		return minderValideAfspraakMogelijk;
	}

	public void setMinderValideAfspraakMogelijk(boolean minderValideAfspraakMogelijk)
	{
		this.minderValideAfspraakMogelijk = minderValideAfspraakMogelijk;
	}
}
