
package nl.rivm.screenit.batch.jobs.generalis.gba.wrappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;

public class Vo105BerichtWrapper
{

	private Vo105Bericht vo105Bericht;

	private ScreeningOrganisatie screeningOrganisatie;

	public Vo105Bericht getVo105Bericht()
	{
		return vo105Bericht;
	}

	public void setVo105Bericht(Vo105Bericht vo105Bericht)
	{
		this.vo105Bericht = vo105Bericht;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}
}
