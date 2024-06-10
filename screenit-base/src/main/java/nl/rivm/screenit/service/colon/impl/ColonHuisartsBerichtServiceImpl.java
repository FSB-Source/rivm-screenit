package nl.rivm.screenit.service.colon.impl;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.colon.ColonEdiService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.BezwaarUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ColonHuisartsBerichtServiceImpl implements ColonHuisartsBerichtService
{
	@Autowired
	private ColonEdiService ediService;

	@Override
	public void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType,
		MailMergeContext context)
	{
		verstuurColonHuisartsBericht(client, colonScreeningRonde, colonScreeningRonde.getColonHuisarts(), berichtType, context, false);
	}

	@Override
	public ColonHuisartsBericht verstuurHuisartsBericht(ColonHuisartsBericht huidigBericht, EnovationHuisarts huisarts)
	{
		MailMergeContext context = new MailMergeContext();
		context.setClient(huidigBericht.getClient());
		return verstuurColonHuisartsBericht(huidigBericht.getClient(), huidigBericht.getScreeningsRonde(), huisarts, huidigBericht.getBerichtType(), context, true);
	}

	@Override
	public ColonHuisartsBericht verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, EnovationHuisarts huisarts, HuisartsBerichtType berichtType,
		MailMergeContext context, boolean opnieuwVerzonden)
	{
		if (colonScreeningRonde == null
			|| BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, Bevolkingsonderzoek.COLON))
		{
			return null;
		}

		if (huisarts != null)
		{
			ColonHuisartsBericht huisartsBericht = ediService.maakHuisartsBericht(berichtType, ColonHuisartsBerichtStatus.CONTROLE_NIET_NODIG, client,
				huisarts, context, opnieuwVerzonden);
			ediService.verstuurMedVry(huisartsBericht);
			return huisartsBericht;
		}
		return null;
	}
}
