package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaScreeningRondesVerwijderenWriter extends BaseWriter<MammaScreeningRonde>
{
	private Logger LOG = LoggerFactory.getLogger(MammaScreeningRondesVerwijderenWriter.class);

	@Autowired
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(MammaScreeningRonde ronde)
	{
		boolean heeftLaatstePositieveUitslagBinnenBewaartermijn = heeftLaatstePositieveUitslagBinnenBewaartermijn(ronde);
		boolean isVerwijderd = false;
		if (!heeftLaatstePositieveUitslagBinnenBewaartermijn)
		{
			isVerwijderd = screeningrondeService.verwijderScreeningRonde(ronde, false);
			if (isVerwijderd)
			{
				LOG.info("Verwijderd, rondeId: {}", ronde.getId());
				aantalContextOphogen(MammaIlmJobListener.KEY_RONDES_VERWIJDERD_AANTAL);
			}
			else
			{
				LOG.info("Niet verwijderd, rondeId: {}, heeftIlmBeelden: {}, heeftLaatstePositieveUitslagBinnenBewaartermijn: {}", ronde.getId(), !isVerwijderd,
					heeftLaatstePositieveUitslagBinnenBewaartermijn);
			}
		}
	}

	private boolean heeftLaatstePositieveUitslagBinnenBewaartermijn(MammaScreeningRonde ronde)
	{
		Client client = ronde.getDossier().getClient();
		MammaScreeningRonde laatsteScreeningRondeMetPositieveUitslag = screeningrondeService.getLaatsteScreeningRondeMetPositieveUitslag(client);
		if (laatsteScreeningRondeMetPositieveUitslag != null)
		{
			int bewaartermijnInDagen = preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN.name());
			LocalDate verwijderGrensDatum = currentDateSupplier.getLocalDate().minusDays(bewaartermijnInDagen);
			return DateUtil.toLocalDate(laatsteScreeningRondeMetPositieveUitslag.getStatusDatum()).isAfter(verwijderGrensDatum);
		}
		return false;
	}
}
