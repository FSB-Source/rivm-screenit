package nl.rivm.screenit.service;

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

import java.util.Date;

import nl.rivm.screenit.dto.mamma.se.MammaKwaliteitsopnameDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;

public interface BerichtToBatchService
{
	void queueCDABericht(Bevolkingsonderzoek bvo);

	void queueHPVBericht(Long labId);

	void queueIFobtBericht(String messageId);

	void queueMammaUploadBeeldenVerzoekBericht();

	void queueMammaHl7v24RetryDeleteBerichtUitgaand(Long uitnodigingsNr, Long clientId);

	void queueMammaHL7v24BerichtUitgaand(MammaScreeningRonde ronde, MammaHL7v24ORMBerichtStatus status);

	void queueMammaHL7v24BerichtUitgaand(Client client, MammaHL7v24ORMBerichtStatus status);

	void queueMammaUploadBeeldenHL7v24BerichtUitgaand(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus status, Date onderzoeksDatum);

	void queueMammaUploadBeeldenHL7v24BerichtUitgaand(Long accessionNumber, Long clientId, MammaHL7v24ORMBerichtStatus status,
		Date onderzoeksDatum);

	void queueMammaKwaliteitsopnameHL7v24BerichtUitgaand(MammaKwaliteitsopnameDto kwaliteitsopname, MammaHL7v24ORMBerichtStatus status);

	void queueMammaInkomendIMSBericht(String messageId);

	void queueMammaPersoonsGegevensGewijzigdImsBericht(Client client);

	void queueMammaBsnWijzigingenImsBericht(Client client, GbaMutatie mutatie);

	void queueMammaVerzamelOnderzoeksDataBericht();
}
