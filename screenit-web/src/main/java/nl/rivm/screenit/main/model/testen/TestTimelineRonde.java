package nl.rivm.screenit.main.model.testen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;

public class TestTimelineRonde implements Serializable, IDetachable
{

	private static final long serialVersionUID = 1L;

	private Integer rondeNummer;

	private ScreeningRondeGebeurtenissen colonScreeningRondeDossier;

	private ScreeningRondeGebeurtenissen cervixScreeningRondeDossier;

	private ScreeningRondeGebeurtenissen mammaScreeningRondeDossier;

	public Integer getRondeNummer()
	{
		return rondeNummer;
	}

	public void setRondeNummer(Integer rondeNummer)
	{
		this.rondeNummer = rondeNummer;
	}

	public ScreeningRondeGebeurtenissen getColonScreeningRondeDossier()
	{
		return colonScreeningRondeDossier;
	}

	public void setColonScreeningRondeDossier(ScreeningRondeGebeurtenissen colonScreeningRondeDossier)
	{
		this.colonScreeningRondeDossier = colonScreeningRondeDossier;
	}

	public ScreeningRondeGebeurtenissen getCervixScreeningRondeDossier()
	{
		return cervixScreeningRondeDossier;
	}

	public void setCervixScreeningRondeDossier(ScreeningRondeGebeurtenissen cervixScreeningRondeDossier)
	{
		this.cervixScreeningRondeDossier = cervixScreeningRondeDossier;
	}

	public ScreeningRondeGebeurtenissen getMammaScreeningRondeDossier()
	{
		return mammaScreeningRondeDossier;
	}

	public void setMammaScreeningRondeDossier(ScreeningRondeGebeurtenissen mammaScreeningRondeDossier)
	{
		this.mammaScreeningRondeDossier = mammaScreeningRondeDossier;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(colonScreeningRondeDossier);
		ModelUtil.nullSafeDetach(cervixScreeningRondeDossier);
		ModelUtil.nullSafeDetach(mammaScreeningRondeDossier);
	}
}
