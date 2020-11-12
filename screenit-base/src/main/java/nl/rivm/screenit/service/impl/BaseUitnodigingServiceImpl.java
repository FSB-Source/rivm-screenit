
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.Date;

import nl.rivm.screenit.dao.BaseUitnodigingDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.service.BaseUitnodigingService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BaseUitnodigingServiceImpl implements BaseUitnodigingService
{

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseUitnodigingDao uitnodigingDao;

	@Override
	public <U extends Uitnodiging<?>> boolean heeftAlEenNieuwereUitnodiging(U huidigeUitnodiging)
	{
		boolean heeftAlEenNieuwereUitnodiging = false;
		for (Uitnodiging<?> uitnodiging : huidigeUitnodiging.getScreeningRonde().getUitnodigingen())
		{
			if (DateUtil.compareAfter(uitnodiging.getCreatieDatum(), huidigeUitnodiging.getCreatieDatum()))
			{
				heeftAlEenNieuwereUitnodiging = true;
				break;
			}
		}
		return heeftAlEenNieuwereUitnodiging;
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public <U extends InpakbareUitnodiging<?>> boolean isVerstuurdMetTijdelijkAdres(U uitnodiging)
	{
		Date verstuurdDatum = uitnodiging.getVerstuurdDatum();
		return AdresUtil.isTijdelijkAdres(uitnodiging.getScreeningRonde().getDossier().getClient().getPersoon(), new DateTime(verstuurdDatum));
	}

	@Override
	public <U extends InpakbareUitnodiging<?>> boolean isAdresGewijzigdNaUitnodigingsdatum(U uitnodiging)
	{
		boolean isAdresGewijzigd = false;
		Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
		boolean kanVersturenMetTijdelijkAdres = clientService.isTijdelijkeAdresNuActueel(client.getPersoon());
		boolean verstuurdMetTijdelijkAdres = isVerstuurdMetTijdelijkAdres(uitnodiging);

		isAdresGewijzigd |= verstuurdMetTijdelijkAdres && !kanVersturenMetTijdelijkAdres;

		if (!isAdresGewijzigd)
		{

			isAdresGewijzigd |= !verstuurdMetTijdelijkAdres && kanVersturenMetTijdelijkAdres;
		}

		if (!isAdresGewijzigd && verstuurdMetTijdelijkAdres && kanVersturenMetTijdelijkAdres)
		{

			LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();

			loggingZoekCriteria.setBsnClient(client.getPersoon().getBsn());
			loggingZoekCriteria.setGebeurtenis(Arrays.asList(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES));
			loggingZoekCriteria.setVanaf(uitnodiging.getVerstuurdDatum());
			isAdresGewijzigd |= logService.countLogRegels(loggingZoekCriteria) > 0;
		}

		if (!isAdresGewijzigd)
		{
			for (GbaMutatie mutatie : client.getGbaMutaties())
			{
				if (DateUtil.compareAfter(mutatie.getMutatieDatum(), uitnodiging.getVerstuurdDatum()))
				{
					isAdresGewijzigd = true;
					break;
				}
			}
		}
		return isAdresGewijzigd;
	}

	@Override
	public <U extends Uitnodiging<?>> U getUitnodiging(Class<U> clazz, String trackId, String postcode, Integer huisnummer)
	{
		return uitnodigingDao.getUitnodiging(clazz, trackId, postcode, huisnummer);
	}
}
