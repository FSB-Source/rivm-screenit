package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Recht;

public class CervixLabformulierHuisartsOnbekendPanel extends CervixLabformulierBasePanel
{

	public CervixLabformulierHuisartsOnbekendPanel(String id, List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		super(id, labformulierenIds, labformulier);
	}

	@Override
	protected void setResponse(List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		setResponsePage(new CervixLabformulierHuisartsOnbekendPage(labformulierenIds, labformulier));
	}

	@Override
	protected Recht getRecht()
	{
		return Recht.GEBRUIKER_CERVIX_LABFORMULIEREN_HUISARTS_ONBEKEND;
	}

	@Override
	protected List<CervixLabformulierStatus> permissiesVoorStatussen()
	{
		List<CervixLabformulierStatus> labformulierStatussen = new ArrayList<>();
		labformulierStatussen.add(CervixLabformulierStatus.HUISARTS_ONBEKEND);
		labformulierStatussen.add(CervixLabformulierStatus.GECONTROLEERD);
		return labformulierStatussen;
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
		return false;
	}

	@Override
	protected boolean getMedischeGegevensVisible()
	{
		return false;
	}

	@Override
	protected boolean magHuisartsWijzigen()
	{
		return !getModelObject()
				.getDigitaal();
	}

	@Override
	protected boolean magHuisartslocatieZoekenOpAdres()
	{
		return true;
	}

	@Override
	protected CervixLabformulierStatus getVanStatus()
	{
		return CervixLabformulierStatus.HUISARTS_ONBEKEND;
	}

	@Override
	protected CervixLabformulierStatus getNaarStatus()
	{
		return CervixLabformulierStatus.GECONTROLEERD;
	}

	@Override
	protected boolean getLabformulierStatusVisible()
	{
		return true;
	}

}
