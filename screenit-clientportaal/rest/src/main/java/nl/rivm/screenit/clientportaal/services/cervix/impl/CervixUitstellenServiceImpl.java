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

import java.time.LocalDate;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.exception.NotValidException;
import nl.rivm.screenit.clientportaal.mappers.cervix.CervixUitstelMapper;
import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstelDto;
import nl.rivm.screenit.clientportaal.model.cervix.CervixUitstellenStatusDto;
import nl.rivm.screenit.clientportaal.services.DatumValidatieService;
import nl.rivm.screenit.clientportaal.services.cervix.CervixUitstellenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixUitstellenServiceImpl implements CervixUitstellenService
{

	@Autowired
	private CervixBaseScreeningrondeService screeningrondeService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private CervixUitstelMapper cervixUitstelMapper;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private DatumValidatieService datumValidatieService;

	private final int DUUR_ZWANGERSCHAP = 9;

	private final int MAX_TERMIJN_UITSTELLEN = 60;

	@Override
	public CervixUitstellenStatusDto getUitstelStatus(Client client)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();
		CervixUitnodiging laatsteZasUitnodiging = laatsteScreeningRonde.getLaatsteZasUitnodiging();

		boolean zasAanvraagNogInBehandeling = laatsteZasUitnodiging != null && laatsteZasUitnodiging.getMonster() == null;

		Integer uitstelBijZwangerschap = preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name());

		LocalDate datumVolgendeRonde = DateUtil.toLocalDate(cervixDossier.getVolgendeRondeVanaf());

		return new CervixUitstellenStatusDto(zasAanvraagNogInBehandeling, uitstelBijZwangerschap, datumVolgendeRonde);
	}

	@Override
	public CervixUitstelDto getHuidigeCervixUitstel(Client client)
	{
		CervixDossier cervixDossier = client.getCervixDossier();
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();

		if (laatsteScreeningRonde != null)
		{
			CervixUitstel cervixUitstel = laatsteScreeningRonde.getUitstel();
			if (cervixUitstel != null)
			{
				return cervixUitstelMapper.cervixUitstelToDto(cervixUitstel);
			}
		}
		return null;
	}

	@Override
	public void valideerDatumBijZwangerschap(LocalDate uitstellenTotDatum)
	{
		Integer uitstelBijZwangerschap = preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name());

		LocalDate minDatum = currentDateSupplier.getLocalDate().minusDays(uitstelBijZwangerschap);
		if (uitstellenTotDatum.isBefore(minDatum))
		{
			throw new NotValidException("Datum ligt te ver in het verleden");
		}

		LocalDate maxDatum = currentDateSupplier.getLocalDate().plusMonths(DUUR_ZWANGERSCHAP).plusDays(uitstelBijZwangerschap);
		if (uitstellenTotDatum.isAfter(maxDatum))
		{
			throw new NotValidException("Datum ligt te ver in de toekomst");
		}
	}

	@Override
	public void valideerDatumBijAnders(LocalDate uitstellenTotDatum)
	{
		if (datumValidatieService.datumIsInHetVerleden(uitstellenTotDatum))
		{
			throw new NotValidException("Datum ligt in het verleden");
		}

		LocalDate maxDatum = currentDateSupplier.getLocalDate().plusMonths(MAX_TERMIJN_UITSTELLEN);
		if (uitstellenTotDatum.isAfter(maxDatum))
		{
			throw new NotValidException("Datum ligt te ver in de toekomst");
		}
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@Override
	public void vraagUitstelAan(Client client, CervixUitstelDto uitstellenDto)
	{
		screeningrondeService.uitstelAanvragen(client, cervixUitstelMapper.dtoToCervixUitstel(uitstellenDto), client);
	}

}
