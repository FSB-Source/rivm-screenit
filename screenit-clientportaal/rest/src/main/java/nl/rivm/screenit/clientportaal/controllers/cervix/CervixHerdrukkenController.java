package nl.rivm.screenit.clientportaal.controllers.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("cervix/herdrukken")
@RestController
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class CervixHerdrukkenController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final BriefHerdrukkenService herdrukkenService;

	private final ClientService clientService;

	@PutMapping("aanvragen")
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> vraagHerdrukAan(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.CERVIX_HERDRUK))
		{
			herdrukkenService.opnieuwAanmaken(getCervixBriefLaatstVerstuurdeUitnodiging(client), client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	private CervixBrief getCervixBriefLaatstVerstuurdeUitnodiging(Client client)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();
		CervixUitnodiging laatsteAfgedrukteUitstrijkjeUitnodiging = clientService.getLaatstVerstuurdeUitnodiging(laatsteScreeningRonde, false);

		CervixBrief cervixBrief = laatsteAfgedrukteUitstrijkjeUitnodiging.getBrief();
		cervixBrief.setAangevraagdeHerdruk(true);
		return cervixBrief;
	}
}
