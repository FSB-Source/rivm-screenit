package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class Cervix2023StartBepalingServiceImpl implements Cervix2023StartBepalingService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Override
	public boolean isBmhk2023Actief()
	{
		return !getStartdatumBmhk2023().isAfter(dateSupplier.getLocalDate());
	}

	@Override
	public boolean rondeValtBinnenBmhk2023(CervixScreeningRonde ronde)
	{
		var eersteUitnodiging = ronde.getEersteUitnodiging();
		if (eersteUitnodiging == null)
		{
			return isBmhk2023Actief();
		}

		var datumEersteUitnodigingAangemaakt = DateUtil.toLocalDate(eersteUitnodiging.getCreatieDatum());
		return !getStartdatumBmhk2023().isAfter(datumEersteUitnodigingAangemaakt);
	}

	@Override
	public boolean datumValtBinnenBmhk2023(LocalDate datum)
	{
		return !datum.isBefore(getStartdatumBmhk2023());
	}

	@Override
	public boolean isBmhk2023Laboratorium(BMHKLaboratorium laboratorium)
	{
		return organisatieParameterService.getOrganisatieParameter(laboratorium, OrganisatieParameterKey.CERVIX_HPV_ORDER_NIEUW, false);
	}

	@Override
	public boolean isBmhk2023Tarief(CervixTarief tarief)
	{
		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			return false;
		}

		return isBmhk2023Laboratorium(CervixTariefType.getLabTarief(tarief).getBmhkLaboratorium());
	}

	private LocalDate getStartdatumBmhk2023()
	{
		var startDatumString = preferenceService.getString(PreferenceKey.CERVIX_START_BMHK2023.name(), "20230605");
		return LocalDate.parse(startDatumString, DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDD));
	}
}
