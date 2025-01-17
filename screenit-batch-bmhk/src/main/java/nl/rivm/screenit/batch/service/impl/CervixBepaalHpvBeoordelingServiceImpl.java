package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.CervixBepaalHpvBeoordelingService;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvAnalyseresultaat;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvMonsterWrapper;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvOrderCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixBepaalHpvBeoordelingServiceImpl implements CervixBepaalHpvBeoordelingService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	private Boolean isGenotyperingAnalyseGestart;

	private LocalDateTime refreshTime;

	@Override
	public CervixHpvBeoordelingWaarde getHpvBeoordelingWaarde(List<CervixHpvAnalyseresultaat> analyseresultaten)
	{
		CervixHpvBeoordelingWaarde beoordeling;

		List<CervixHpvAnalyseresultaat> completeAnalyseresultaten = CervixHpvMonsterWrapper.getCompleteAnalyseresultaten(analyseresultaten);

		Set<CervixHpvResultValue> resultValues = completeAnalyseresultaten.stream().map(CervixHpvAnalyseresultaat::getResultValue).collect(Collectors.toSet());
		Set<CervixHpvResultCode> resultCodes = completeAnalyseresultaten.stream().map(CervixHpvAnalyseresultaat::getResultCode).collect(Collectors.toSet());
		Set<CervixHpvOrderCode> orderCodes = resultCodes.stream().map(CervixHpvResultCode::getOrderCode).collect(Collectors.toSet());
		if (orderCodes.size() == 1)
		{
			CervixHpvOrderCode orderCode = orderCodes.iterator().next();
			switch (orderCode)
			{
			case GEN:
				beoordeling = bepaalGenBeoordeling(resultValues, resultCodes);
				break;
			case PAN:
				beoordeling = bepaalPanBeoordeling(resultValues, resultCodes);
				break;
			default:
				throw new IllegalStateException(orderCode + " not supported");
			}
		}
		else
		{
			throw new IllegalStateException();
		}
		return beoordeling;
	}

	private CervixHpvBeoordelingWaarde bepaalGenBeoordeling(Set<CervixHpvResultValue> resultValues, Set<CervixHpvResultCode> resultCodes)
	{
		CervixHpvBeoordelingWaarde beoordeling = null;
		if (resultCodes.size() == 3)
		{
			if (isGenotyperingAnalyseGestart())
			{
				var positieven = geefAantalResultaten(resultValues, CervixHpvBeoordelingWaarde.POSITIEF);
				var negatieven = geefAantalResultaten(resultValues, CervixHpvBeoordelingWaarde.NEGATIEF);
				var ongeldige = geefAantalResultaten(resultValues, CervixHpvBeoordelingWaarde.ONGELDIG);
				var alleenHpvOtherPositiefRestOngeldig = alleenHpvOtherPositiefRestOngeldig(resultValues);
				var hpv16EnOf18Positief = hpv16EnOf18Positief(resultValues);

				if ((positieven + negatieven + ongeldige) == 3)
				{
					if (negatieven == 3)
					{
						beoordeling = CervixHpvBeoordelingWaarde.NEGATIEF;
					}
					else if (hpv16EnOf18Positief || positieven == 1 && negatieven == 2 || positieven == 2 && (negatieven == 1 || ongeldige == 1) || positieven == 3)
					{
						beoordeling = CervixHpvBeoordelingWaarde.POSITIEF;
					}
					else if (ongeldige == 3 || alleenHpvOtherPositiefRestOngeldig)
					{
						beoordeling = CervixHpvBeoordelingWaarde.ONGELDIG;
					}
				}
			}
		}
		else
		{
			throw new IllegalStateException();
		}
		return beoordeling;
	}

	private boolean hpv16EnOf18Positief(Set<CervixHpvResultValue> resultValues)
	{
		return resultValues.stream().anyMatch(waarde -> waarde.getResultaatType() == CervixHpvBeoordelingWaarde.POSITIEF &&
			List.of(CervixHpvResultCode.HPV18, CervixHpvResultCode.HPV16).contains(waarde.getResultCode()));
	}

	private boolean alleenHpvOtherPositiefRestOngeldig(Set<CervixHpvResultValue> resultValues)
	{
		return resultValues.stream()
			.filter(waarde -> waarde.getResultaatType() == CervixHpvBeoordelingWaarde.POSITIEF && waarde.getResultCode() == CervixHpvResultCode.OHR)
			.count() == 1 && geefAantalResultaten(resultValues, CervixHpvBeoordelingWaarde.ONGELDIG) == 2;
	}

	private int geefAantalResultaten(Set<CervixHpvResultValue> resultValues, CervixHpvBeoordelingWaarde beoordelingWaarde)
	{
		return (int) resultValues.stream().filter(resultaat -> resultaat.getResultaatType() == beoordelingWaarde).count();
	}

	private CervixHpvBeoordelingWaarde bepaalPanBeoordeling(Set<CervixHpvResultValue> resultValues, Set<CervixHpvResultCode> resultCodes)
	{
		CervixHpvBeoordelingWaarde beoordeling = null;
		if (resultCodes.size() == 1)
		{
			CervixHpvResultValue analyseresultaat = resultValues.iterator().next();
			switch (analyseresultaat)
			{
			case INVALID_HR_HPV:
				beoordeling = CervixHpvBeoordelingWaarde.ONGELDIG;
				break;
			case NEG_HR_HPV:
				beoordeling = CervixHpvBeoordelingWaarde.NEGATIEF;
				break;
			case POS_HR_HPV:
				if (!isGenotyperingAnalyseGestart())
				{
					beoordeling = CervixHpvBeoordelingWaarde.POSITIEF;
				}
				break;
			default:
				break;
			}
		}
		return beoordeling;
	}

	private boolean isGenotyperingAnalyseGestart()
	{
		if (isGenotyperingAnalyseGestart == null || refreshTime.plusMinutes(1).isBefore(LocalDateTime.now()))
		{
			String startdatumAanleveringGenotyperingString = preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());
			LocalDate startdatumAanleveringGenotypering = LocalDate.parse(startdatumAanleveringGenotyperingString, DateTimeFormatter.ofPattern("yyyyMMdd"));

			isGenotyperingAnalyseGestart = !currentDateSupplier.getLocalDate().isBefore(startdatumAanleveringGenotypering);
			refreshTime = LocalDateTime.now();
		}
		return isGenotyperingAnalyseGestart;
	}
}
