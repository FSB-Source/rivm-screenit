package nl.rivm.screenit.clientportaal.services.cervix.impl;

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

import java.util.Date;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.exception.NotValidException;
import nl.rivm.screenit.clientportaal.model.cervix.CervixZasStatusDto;
import nl.rivm.screenit.clientportaal.services.cervix.CervixZasService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixZasServiceImpl implements CervixZasService
{

	private final ICurrentDateSupplier currentDateSupplier;

	private final CervixBaseScreeningrondeService screeningrondeService;

	private final CervixFactory factory;

	@Override
	public CervixZasStatusDto getZasStatus(Client client)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();

		if (laatsteScreeningRonde == null)
		{
			return null;
		}

		boolean heeftMaxAantalZasBereikt = screeningrondeService.heeftMaxAantalZASsenBereikt(laatsteScreeningRonde, true);
		CervixUitstel uitstel = laatsteScreeningRonde.getUitstel();
		Date uitstellenTotDatum = null;
		if (uitstel != null && uitstel.getGeannuleerdDatum() == null)
		{
			uitstellenTotDatum = uitstel.getUitstellenTotDatum();
		}
		return new CervixZasStatusDto(heeftMaxAantalZasBereikt, currentDateSupplier.getLocalDate(), DateUtil.toLocalDate(uitstellenTotDatum));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void vraagZasAan(Client client, boolean ontvangenNaUitstel)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();

		valideerAanvraag(laatsteScreeningRonde);

		factory.maakZasUitnodiging(client, client, ontvangenNaUitstel, true);
	}

	private void valideerAanvraag(CervixScreeningRonde laatsteScreeningRonde)
	{
		CervixUitnodiging laatsteZasUitnodiging = laatsteScreeningRonde.getLaatsteZasUitnodiging();
		if (screeningrondeService.heeftMaxAantalZASsenBereikt(laatsteScreeningRonde, true))
		{
			throw new NotValidException("MAX_ZAS_AANVRAGEN_BEREIKT");
		}
		if (laatsteZasUitnodiging != null && laatsteZasUitnodiging.getMonster() == null && laatsteZasUitnodiging.getGeannuleerdDatum() == null)
		{
			throw new NotValidException("ZAS_AANVRAAG_IN_BEHANDELING");
		}
	}

	@Override
	public boolean rondeHeeftCervixUitstel(CervixScreeningRonde ronde)
	{
		CervixUitstel uitstel = ronde.getUitstel();
		return uitstel != null && uitstel.getGeannuleerdDatum() == null;
	}
}
