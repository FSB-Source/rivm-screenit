package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.service.cervix.CervixAanvraagService;

import org.springframework.stereotype.Service;

@Service
public class CervixAanvraagServiceImpl implements CervixAanvraagService
{

	@Override
	public boolean magDigitaalLabformulierAanvragen(CervixUitstrijkje uitstrijkje)
	{
		CervixScreeningRonde laatsteScreeningRonde = uitstrijkje.getUitnodiging().getScreeningRonde().getDossier().getLaatsteScreeningRonde();
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		return laatsteScreeningRonde.getStatus() != ScreeningRondeStatus.AFGEROND
			&& uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ONTVANGEN
			&& (labformulier == null || (labformulier.getStatus() != CervixLabformulierStatus.GECONTROLEERD
				|| labformulier.getStatus() != CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
				|| labformulier.getStatus() != CervixLabformulierStatus.HUISARTS_ONBEKEND));
	}

}
