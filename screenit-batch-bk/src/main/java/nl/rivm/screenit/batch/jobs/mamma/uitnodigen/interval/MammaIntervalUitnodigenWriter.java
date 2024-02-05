package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.interval;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.MammaUitnodigenListener;
import nl.rivm.screenit.batch.service.MammaBatchUitnodigenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIntervalUitnodigenRapportage;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class MammaIntervalUitnodigenWriter extends BaseWriter<Client>
{
	private final MammaBatchUitnodigenService uitnodigenService;

	@Override
	protected void write(Client client) throws Exception
	{
		LOG.info("Client {} uitnodigen", client.getId());
		var uitnodiging = uitnodigenService.maakNieuweRondeEnIntervalUitnodiging(client);
		if (uitnodiging != null)
		{
			bijwerkenRapportage(uitnodiging);
		}
	}

	private void bijwerkenRapportage(MammaUitnodiging uitnodiging)
	{
		var screeningsorganisatie = uitnodiging.getStandplaatsRonde().getStandplaats().getRegio();
		var intervalUitnodigenRapportage = getRapportageUitJobContext(screeningsorganisatie);

		var uitnodigingsBriefType = uitnodiging.getBrief().getBriefType();
		switch (uitnodigingsBriefType)
		{
		case MAMMA_OPEN_UITNODIGING:
			intervalUitnodigenRapportage.setUitgenodigdOpen(intervalUitnodigenRapportage.getUitgenodigdOpen() + 1);
			break;
		case MAMMA_UITNODIGING_MINDER_VALIDE:
			intervalUitnodigenRapportage.setUitgenodigdMinderValide(intervalUitnodigenRapportage.getUitgenodigdMinderValide() + 1);
			break;
		case MAMMA_UITNODIGING_SUSPECT:
			intervalUitnodigenRapportage.setUitgenodigdSuspect(intervalUitnodigenRapportage.getUitgenodigdSuspect() + 1);
			break;
		default:
			throw new IllegalStateException("Onverwacht uitnodigingsbrieftype: " + uitnodigingsBriefType);
		}
	}

	private MammaIntervalUitnodigenRapportage getRapportageUitJobContext(ScreeningOrganisatie screeningsorganisatie)
	{
		Map<Long, MammaIntervalUitnodigenRapportage> rapportages = getTypedValueFromExecutionContext(MammaUitnodigenListener.INTERVAL_RAPPORTAGE_KEY);
		return rapportages.computeIfAbsent(screeningsorganisatie.getId(), k -> new MammaIntervalUitnodigenRapportage());
	}
}
