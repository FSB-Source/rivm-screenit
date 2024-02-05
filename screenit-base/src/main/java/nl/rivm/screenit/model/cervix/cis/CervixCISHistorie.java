package nl.rivm.screenit.model.cervix.cis;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "cervix", name = "cis_historie")
public class CervixCISHistorie extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(mappedBy = "cisHistorie", optional = false, fetch = FetchType.LAZY)
	private CervixDossier dossier;

	@Column(nullable = false)
	private boolean heeftPap0 = false;

	@Column(nullable = false)
	private boolean deelnameImprove = false;

	@Column(nullable = false)
	private boolean heeftUitslagInRonde0;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitstel uitstel;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixScreeningRonde screeningRonde;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixAfmelding afmelding;

	@OneToMany(mappedBy = "cisHistorie", fetch = FetchType.LAZY)
	private List<CervixCISHistorieOngestructureerdRegel> cisHistorieRegels = new ArrayList<>();

	public CervixDossier getDossier()
	{
		return dossier;
	}

	public void setDossier(CervixDossier dossier)
	{
		this.dossier = dossier;
	}

	public CervixScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(CervixScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public CervixAfmelding getAfmelding()
	{
		return afmelding;
	}

	public void setAfmelding(CervixAfmelding afmelding)
	{
		this.afmelding = afmelding;
	}

	public List<CervixCISHistorieOngestructureerdRegel> getCisHistorieRegels()
	{
		return cisHistorieRegels;
	}

	public void setCisHistorieRegels(List<CervixCISHistorieOngestructureerdRegel> cisHistorieRegels)
	{
		this.cisHistorieRegels = cisHistorieRegels;
	}

	public boolean isHeeftPap0()
	{
		return heeftPap0;
	}

	public void setHeeftPap0(boolean heeftPap0)
	{
		this.heeftPap0 = heeftPap0;
	}

	public CervixUitstel getUitstel()
	{
		return uitstel;
	}

	public void setUitstel(CervixUitstel uitstel)
	{
		this.uitstel = uitstel;
	}

	public boolean isHeeftUitslagInRonde0()
	{
		return heeftUitslagInRonde0;
	}

	public void setHeeftUitslagInRonde0(boolean heeftUitslagInRonde0)
	{
		this.heeftUitslagInRonde0 = heeftUitslagInRonde0;
	}

	public boolean isDeelnameImprove()
	{
		return deelnameImprove;
	}

	public void setDeelnameImprove(boolean deelnameImprove)
	{
		this.deelnameImprove = deelnameImprove;
	}
}
