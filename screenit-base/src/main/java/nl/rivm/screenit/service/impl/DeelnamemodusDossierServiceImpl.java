package nl.rivm.screenit.service.impl;

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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.DeelnamemodusDossierService;
import nl.rivm.screenit.service.DossierFactory;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class DeelnamemodusDossierServiceImpl implements DeelnamemodusDossierService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private DossierFactory dossierFactory;

	@Autowired
	private LogService logService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Set<Bevolkingsonderzoek> pasDeelnamewensToe(Client client, DeelnamewensDto deelnamewensDto, Account account)
	{
		Set<Bevolkingsonderzoek> bevolkingsonderzoeken = new HashSet<>();
		if (deelnamewensDto.isDeelnamewensBmhk() || deelnamewensDto.isDeelnamewensBk())
		{
			bevolkingsonderzoeken = dossierFactory.maakBmhkEnBkDossiers(client);

			var cervixDossier = client.getCervixDossier();
			if (deelnamewensDto.isDeelnamewensBmhk() && cervixDossier.getDeelnamemodus() != Deelnamemodus.EXPLICIETE_DEELNAMEWENS)
			{
				setDossierExplicieteDeelnamewens(cervixDossier);
				bevolkingsonderzoeken.add(Bevolkingsonderzoek.CERVIX);
			}

			var mammaDossier = client.getMammaDossier();
			if (deelnamewensDto.isDeelnamewensBk() && mammaDossier.getDeelnamemodus() != Deelnamemodus.EXPLICIETE_DEELNAMEWENS)
			{
				setDossierExplicieteDeelnamewens(mammaDossier);
				bevolkingsonderzoeken.add(Bevolkingsonderzoek.MAMMA);
			}
		}
		logService.logGebeurtenis(LogGebeurtenis.DEELNAMEWENS_REGISTREREN, account, client, getDeelnamewensLogMelding(deelnamewensDto, bevolkingsonderzoeken));
		return bevolkingsonderzoeken;
	}

	protected String getDeelnamewensLogMelding(DeelnamewensDto deelnamewensDto, Set<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		List<Bevolkingsonderzoek> deelnameBvos = new ArrayList<>();
		List<Bevolkingsonderzoek> selectieblokkadeBvos = new ArrayList<>();
		bepaalDeelnamewensBvoLijsten(bevolkingsonderzoeken, Bevolkingsonderzoek.CERVIX, deelnamewensDto.isDeelnamewensBmhk(), deelnameBvos, selectieblokkadeBvos);
		bepaalDeelnamewensBvoLijsten(bevolkingsonderzoeken, Bevolkingsonderzoek.MAMMA, deelnamewensDto.isDeelnamewensBk(), deelnameBvos, selectieblokkadeBvos);
		String melding = "Expliciete deelnamewens vastgelegd voor bevolkingsonderzoek(en): " + Bevolkingsonderzoek.getAfkortingen(deelnameBvos);
		melding += !selectieblokkadeBvos.isEmpty() ?
			", dossiers met selectieblokkade aangemaakt voor bevolkingsonderzoek(en): " + Bevolkingsonderzoek.getAfkortingen(selectieblokkadeBvos) : "";
		return melding;
	}

	private void setDossierExplicieteDeelnamewens(DeelnamemodusDossier dossier)
	{
		dossier.setDeelnamemodus(Deelnamemodus.EXPLICIETE_DEELNAMEWENS);
		hibernateService.saveOrUpdate(dossier);
	}

	private void bepaalDeelnamewensBvoLijsten(Set<Bevolkingsonderzoek> bevolkingsonderzoeken, Bevolkingsonderzoek bevolkingsonderzoek, boolean deelnamewens,
		List<Bevolkingsonderzoek> deelnameBvos, List<Bevolkingsonderzoek> selectieblokkadeBvos)
	{
		if (bevolkingsonderzoeken.contains(bevolkingsonderzoek))
		{
			if (deelnamewens)
			{
				deelnameBvos.add(bevolkingsonderzoek);
			}
			else
			{
				selectieblokkadeBvos.add(bevolkingsonderzoek);
			}
		}
	}
}
