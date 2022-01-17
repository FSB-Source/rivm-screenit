package nl.rivm.screenit.clientportaal.services.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.model.colon.ColonFitStatusDto;
import nl.rivm.screenit.clientportaal.services.colon.ColonFitService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonFitServiceImpl implements ColonFitService
{

    @Autowired
    private ClientContactService clientContactService;

    @Autowired
    private ColonScreeningsrondeService screeningsrondeService;

    @Override
    public ColonFitStatusDto getFitStatus(Client client)
    {
        ColonDossier colonDossier = client.getColonDossier();
        ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();

        if (laatsteScreeningRonde == null)
        {
            return null;
        }

        boolean maxAantalFitAanvragenBereikt = screeningsrondeService.heeftMaxAantalFitAanvragenBereikt(laatsteScreeningRonde);

        return new ColonFitStatusDto(maxAantalFitAanvragenBereikt);
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED)
    public void vraagFitAan(Client client)
    {
        clientContactService.vraagNieuweIfobtAan(client, client);
    }
}
