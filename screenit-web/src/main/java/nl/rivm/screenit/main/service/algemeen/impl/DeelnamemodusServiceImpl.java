package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;

import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.main.service.algemeen.DeelnamemodusService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Deelnamemodus;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class DeelnamemodusServiceImpl implements DeelnamemodusService
{
	@Override
	public String selectieblokkadeTekst(Client client)
	{
		var bvos = new ArrayList<Bevolkingsonderzoek>();
		if (heeftSelectieblokkade(client.getCervixDossier()))
		{
			bvos.add(Bevolkingsonderzoek.CERVIX);
		}

		if (heeftSelectieblokkade(client.getMammaDossier()))
		{
			bvos.add(Bevolkingsonderzoek.MAMMA);
		}

		return Bevolkingsonderzoek.getAfkortingen(bvos);
	}

	@Override
	public DeelnamewensDto getDeelnamewensDto(Client client)
	{
		var deelnamewensDto = new DeelnamewensDto();
		deelnamewensDto.setDeelnamewensBmhk(heeftExplicieteDeelnamewens(client.getCervixDossier()));
		deelnamewensDto.setDeelnamewensBk(heeftExplicieteDeelnamewens(client.getMammaDossier()));
		return deelnamewensDto;
	}

	@Override
	public boolean heeftNieuweDeelnamewensGeselecteerd(Client client, DeelnamewensDto deelnamewensDto)
	{
		var bmhkNieuwGeselecteerd = deelnamewensNieuwGeselecteerd(client.getCervixDossier(), deelnamewensDto.isDeelnamewensBmhk());
		var bkNieuwGeselecteerd = deelnamewensNieuwGeselecteerd(client.getMammaDossier(), deelnamewensDto.isDeelnamewensBk());
		return bmhkNieuwGeselecteerd || bkNieuwGeselecteerd;
	}

	private boolean heeftSelectieblokkade(DeelnamemodusDossier dossier)
	{
		return dossier != null && dossier.getDeelnamemodus() == Deelnamemodus.SELECTIEBLOKKADE;
	}

	private static boolean heeftExplicieteDeelnamewens(DeelnamemodusDossier dossier)
	{
		return dossier != null && dossier.getDeelnamemodus() == Deelnamemodus.EXPLICIETE_DEELNAMEWENS;
	}

	private boolean deelnamewensNieuwGeselecteerd(DeelnamemodusDossier dossier, boolean deelnamewensInvoer)
	{
		if (!deelnamewensInvoer)
		{
			return false;
		}
		return dossier == null || dossier.getDeelnamemodus() != Deelnamemodus.EXPLICIETE_DEELNAMEWENS;
	}
}
