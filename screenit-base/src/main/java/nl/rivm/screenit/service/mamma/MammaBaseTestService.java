package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

public interface MammaBaseTestService
{
	MammaDossier geefDossier(GbaPersoon gbaPersoon);

	MammaScreeningRonde geefScreeningRonde(GbaPersoon gbaPersoon);

	MammaUitnodiging maakUitnodiging(GbaPersoon gbaPersoon, BriefType briefType);

	String clientenResetten(String bsns);

	void clientReset(Client client);

	int clientenDefinitiefAfmelden(List<Client> clienten, MammaAfmeldingReden afmeldingReden);

	void maakOfVindClient(String bsn, MammaDoelgroep doelgroep, String postcode, BigDecimal deelnamekans, BigDecimal opkomstkans, Date geboortedatum);

	Client maakOfVindClient(GbaPersoon persoon, MammaDoelgroep doelgroep, BigDecimal deelnamekans, BigDecimal opkomstkans, boolean alleenMaken);
}
