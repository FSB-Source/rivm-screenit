package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

public class CervixLabformulierControlerenPanel extends CervixLabformulierBasePanel
{
	public CervixLabformulierControlerenPanel(String id, List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		super(id, labformulierenIds, labformulier);
	}

	@Override
	protected void setResponse(List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		setResponsePage(new CervixLabformulierControlerenPage(labformulierenIds, labformulier));
	}

	@Override
	protected List<CervixLabformulierStatus> permissiesVoorStatussen()
	{
		List<CervixLabformulierStatus> labformulierStatussen = new ArrayList<>();
		if (!getModelObject().getKunstmatig())
		{
			labformulierStatussen.add(CervixLabformulierStatus.GESCAND);
		}
		labformulierStatussen.add(CervixLabformulierStatus.HUISARTS_ONBEKEND);
		labformulierStatussen.add(CervixLabformulierStatus.GECONTROLEERD);
		if (ScreenitSession.get().checkPermission(getRecht(), Actie.VERWIJDEREN))
		{
			labformulierStatussen.add(CervixLabformulierStatus.AFGEKEURD);
		}
		return labformulierStatussen;
	}

	@Override
	protected boolean highlightMonsterstatus(CervixLabformulier labformulier)
	{
		if (labformulier == null)
		{
			return false;
		}
		CervixUitstrijkje uitstrijkje = labformulier.getUitstrijkje();
		return uitstrijkje == null || uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ONTVANGEN;
	}

	@Override
	protected Recht getRecht()
	{
		return Recht.GEBRUIKER_CERVIX_LABFORMULIEREN_CONTROLEREN;
	}

	@Override
	protected boolean getDatumUitstrijkjeVisible()
	{
		return true;
	}

	@Override
	protected boolean getMonsterIdEnabled()
	{
		return true;
	}

	@Override
	protected boolean getSignaleringenEnabled()
	{
		return true;
	}

	@Override
	protected boolean getMedischeGegevensVisible()
	{
		return false;
	}

	@Override
	protected boolean magHuisartsWijzigen()
	{
		return !getModelObject().getDigitaal();
	}

	@Override
	protected CervixLabformulierStatus getVanStatus()
	{
		return CervixLabformulierStatus.GESCAND;
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
