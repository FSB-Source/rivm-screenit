package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.rondes;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class MammaScreeningRondesVerwijderenWriter extends BaseWriter<MammaScreeningRonde>
{

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaScreeningRonde ronde)
	{
		var heeftLaatstePositieveUitslagBinnenBewaartermijn = heeftLaatstePositieveUitslagBinnenBewaartermijn(ronde);
		aantalContextOphogen(MammaIlmJobListener.KEY_RONDES_VERWERKT_AANTAL);

		if (!heeftLaatstePositieveUitslagBinnenBewaartermijn)
		{
			var isVerwijderd = screeningrondeService.verwijderScreeningRonde(ronde, false);
			if (isVerwijderd)
			{
				LOG.info("ILM ronde verwijderd, rondeId: {}", ronde.getId());
				aantalContextOphogen(MammaIlmJobListener.KEY_RONDES_VERWIJDERD_AANTAL);
			}
			else
			{
				LOG.info("ILM ronde niet verwijderd, rondeId: {}, heeftIlmBeelden: {}, heeftLaatstePositieveUitslagBinnenBewaartermijn: {}", ronde.getId(), true,
					heeftLaatstePositieveUitslagBinnenBewaartermijn);
			}
		}
		getExecutionContext().putLong(MammaIlmJobListener.KEY_LAATSTE_RONDE_ID, ronde.getId());
	}

	private boolean heeftLaatstePositieveUitslagBinnenBewaartermijn(MammaScreeningRonde ronde)
	{
		var client = ronde.getDossier().getClient();
		var laatsteScreeningRondeMetPositieveUitslag = screeningrondeService.getLaatsteScreeningRondeMetOngunstigeUitslag(client);
		if (laatsteScreeningRondeMetPositieveUitslag != null)
		{
			var bewaartermijnInDagen = preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN.name());
			var verwijderGrensDatum = currentDateSupplier.getLocalDate().minusDays(bewaartermijnInDagen);
			return DateUtil.toLocalDate(laatsteScreeningRondeMetPositieveUitslag.getStatusDatum()).isAfter(verwijderGrensDatum);
		}
		return false;
	}
}
