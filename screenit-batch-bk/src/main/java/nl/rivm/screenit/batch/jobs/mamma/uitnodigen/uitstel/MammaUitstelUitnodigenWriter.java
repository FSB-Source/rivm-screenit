package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.uitstel;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.MammaUitnodigenListener;
import nl.rivm.screenit.batch.model.UitstelUitnodigingRaportageEntry;
import nl.rivm.screenit.batch.service.MammaBatchUitnodigenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsPeriodeUitnodigenRapportage;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class MammaUitstelUitnodigenWriter extends BaseWriter<Client>
{
	private final MammaBatchUitnodigenService uitnodigenService;

	@Override
	protected void write(Client client) throws Exception
	{
		LOG.info("Maak uitnodiging voor uitstel voor client (id: '{}')", client.getId());
		var raportage = uitnodigenService.maakUitnodigingVoorClientContactUitstel(client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitstel());
		if (raportage != null)
		{
			bijwerkenRapportage(raportage);
		}
	}

	private void bijwerkenRapportage(UitstelUitnodigingRaportageEntry entry)
	{
		var rapportage = getRapportageUitJobContext(entry);

		rapportage.setUitgenodigdNaUitstel(rapportage.getUitgenodigdNaUitstel() + 1);

		switch (entry.getBriefType())
		{
		case MAMMA_AFSPRAAK_UITNODIGING:
			rapportage.setUitgenodigdAfspraak(rapportage.getUitgenodigdAfspraak() + 1);
			break;
		case MAMMA_OPEN_UITNODIGING:
			rapportage.setUitgenodigdOpen(rapportage.getUitgenodigdOpen() + 1);
			break;
		case MAMMA_UITNODIGING_MINDER_VALIDE:
			rapportage.setUitgenodigdMinderValide(rapportage.getUitgenodigdMinderValide() + 1);
			break;
		case MAMMA_UITNODIGING_SUSPECT:
			rapportage.setUitgenodigdSuspect(rapportage.getUitgenodigdSuspect() + 1);
			break;
		default:
			throw new IllegalStateException("Onverwacht uitnodigingsbrieftype: " + entry.getBriefType());
		}
	}

	private MammaStandplaatsPeriodeUitnodigenRapportage getRapportageUitJobContext(UitstelUitnodigingRaportageEntry entry)
	{
		Map<Long, MammaStandplaatsPeriodeUitnodigenRapportage> rapportages = getTypedValueFromExecutionContext(MammaUitnodigenListener.UITSTEL_PER_STANDPLAATSPERIODE_KEY);
		return rapportages.computeIfAbsent(entry.getStandplaatsPeriodeId(), k -> new MammaStandplaatsPeriodeUitnodigenRapportage());
	}
}
