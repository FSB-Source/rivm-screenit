package nl.rivm.screenit.service.colon;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;

public interface ColonHuisartsBerichtService
{
	ColonHuisartsBericht verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, EnovationHuisarts huisarts, HuisartsBerichtType berichtType,
		MailMergeContext context, boolean opnieuwVerzonden);

	void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType, MailMergeContext context);

	ColonHuisartsBericht verstuurHuisartsBericht(ColonHuisartsBericht huidigBericht, EnovationHuisarts object);

}
