package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.model.mamma.enums.MammaFactorType;

public class PlanningScreeningsOrganisatie extends PlanningEntiteit
{
	private final Set<PlanningScreeningsEenheid> screeningsEenheidSet = new HashSet<>();

	private final Set<PlanningStandplaats> standplaatsSet = new HashSet<>();

	private final List<PlanningClient> clientList = new ArrayList<>();

	private final Set<PlanningTehuis> tehuisSet = new HashSet<>();

	private final Set<Long> conceptGewijzigdDoor = new HashSet<>();

	private Integer afspraakDrempel;

	private BigDecimal factorEersteOnderzoek;

	private BigDecimal factorDubbeleTijd;

	private BigDecimal factorMinderValide;

	private int wekenVanTevorenUitnodigen;

	public PlanningScreeningsOrganisatie(Long id, Integer afspraakDrempel, BigDecimal factorEersteOnderzoek, BigDecimal factorDubbeleTijd, BigDecimal factorMinderValide,
		int wekenVanTevorenUitnodigen)
	{
		super(id);
		this.afspraakDrempel = afspraakDrempel;
		this.factorEersteOnderzoek = factorEersteOnderzoek;
		this.factorDubbeleTijd = factorDubbeleTijd;
		this.factorMinderValide = factorMinderValide;
		this.wekenVanTevorenUitnodigen = wekenVanTevorenUitnodigen;
	}

	public List<PlanningClient> getClientList()
	{
		return clientList;
	}

	public Set<PlanningScreeningsEenheid> getScreeningsEenheidSet()
	{
		return screeningsEenheidSet;
	}

	public Set<PlanningStandplaats> getStandplaatsSet()
	{
		return standplaatsSet;
	}

	public Integer getAfspraakDrempel()
	{
		return afspraakDrempel;
	}

	public void setAfspraakDrempel(Integer afspraakDrempel)
	{
		this.afspraakDrempel = afspraakDrempel;
	}

	public BigDecimal getFactorEersteOnderzoek()
	{
		return factorEersteOnderzoek;
	}

	public void setFactorEersteOnderzoek(BigDecimal factorEersteOnderzoek)
	{
		this.factorEersteOnderzoek = factorEersteOnderzoek;
	}

	public BigDecimal getFactorDubbeleTijd()
	{
		return factorDubbeleTijd;
	}

	public void setFactorDubbeleTijd(BigDecimal factorDubbeleTijd)
	{
		this.factorDubbeleTijd = factorDubbeleTijd;
	}

	public BigDecimal getFactorMinderValide()
	{
		return factorMinderValide;
	}

	public void setFactorMinderValide(BigDecimal factorMinderValide)
	{
		this.factorMinderValide = factorMinderValide;
	}

	public BigDecimal getFactor(MammaFactorType factorType)
	{
		switch (factorType)
		{
		case GEEN:
			return BigDecimal.ONE;
		case EERSTE_ONDERZOEK:
			return getFactorEersteOnderzoek();
		case DUBBELE_TIJD:
			return getFactorDubbeleTijd();
		case MINDER_VALIDE:
			return getFactorMinderValide();
		default:
			throw new IllegalArgumentException();
		}
	}

	public int getWekenVanTevorenUitnodigen()
	{
		return wekenVanTevorenUitnodigen;
	}

	public void setWekenVanTevorenUitnodigen(int wekenVanTevorenUitnodigen)
	{
		this.wekenVanTevorenUitnodigen = wekenVanTevorenUitnodigen;
	}

	public void conceptGewijzigdDoor(Long instellingGebruikerId)
	{
		conceptGewijzigdDoor.add(instellingGebruikerId);
	}

	public void restConceptGewijzigdDoor()
	{
		conceptGewijzigdDoor.clear();
	}

	public Set<Long> getConceptGewijzigdDoor()
	{
		return conceptGewijzigdDoor;
	}

	public Set<PlanningTehuis> getTehuisSet()
	{
		return tehuisSet;
	}
}
