package nl.rivm.screenit.dto.cervix.facturatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;

@Setter
@Getter
public class CervixVerrichtingenZoekObject implements Serializable, Cloneable
{
	private String monsterId;

	private Date geboorteDatum;

	private String bsn;

	private Date verrichtingsDatumVanaf;

	private Date verrichtingsDatumTotenmet;

	private Date datumUitstrijkje;

	private CervixTariefType verrichtingsType;

	private String betalingskenmerk;

	private boolean alleenZonderBetalingskenmerk = false;

	private Boolean debet = null;

	private boolean alleenVerrichtingen = false;

	private BMHKLaboratorium bmhkLaboratorium;

	private ScreeningOrganisatie screeningOrganisatie;

	private CervixHuisarts huisarts;

	private CervixHuisartsLocatie huisartsLocatie;

	@Override
	public CervixVerrichtingenZoekObject clone() throws CloneNotSupportedException
	{
		return (CervixVerrichtingenZoekObject) super.clone();
	}
}
