package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Recht;

public class CervixLabformulierVoorCytologiePanel extends CervixLabformulierBasePanel
{

	private static final long serialVersionUID = 1L;

	public CervixLabformulierVoorCytologiePanel(String id, List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		super(id, labformulierenIds, labformulier);
	}

	@Override
	protected void setResponse(List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		setResponsePage(new CervixLabformulierVoorCytologiePage(labformulierenIds, labformulier));
	}

	@Override
	protected Recht getRecht()
	{
		return Recht.GEBRUIKER_CERVIX_LABFORMULIEREN_ZOEKEN_VOOR_CYTOLOGIE;
	}

	@Override
	protected List<CervixLabformulierStatus> permissiesVoorStatussen()
	{
		return Arrays.asList(new CervixLabformulierStatus[] { CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE });
	}

	@Override
	protected boolean getMonsterIdEnabled()
	{
		return false;
	}

	@Override
	protected boolean getSignaleringenEnabled()
	{
		return false;
	}

	@Override
	protected boolean getDatumUitstrijkjeVisible()
	{
		return true;
	}

	@Override
	protected boolean getMedischeGegevensVisible()
	{
		return true;
	}

	@Override
	protected boolean magHuisartsWijzigen()
	{
		return false;
	}

	@Override
	protected CervixLabformulierStatus getVanStatus()
	{
		return null;
	}

	@Override
	protected CervixLabformulierStatus getNaarStatus()
	{
		return null;
	}

	@Override
	protected boolean getLabformulierStatusVisible()
	{
		return false;
	}
}
