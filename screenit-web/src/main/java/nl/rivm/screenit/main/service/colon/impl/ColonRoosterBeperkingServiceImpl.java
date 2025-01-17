package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.DayOfWeek;
import java.time.LocalTime;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonIntakeAfspraakService;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ColonRoosterBeperkingServiceImpl implements ColonRoosterBeperkingService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ColonIntakeAfspraakService intakeAfspraakService;

	@Override
	public ColonRoosterBeperkingenDto getRoosterBeperkingen()
	{
		var colonRoosterBeperkingDto = new ColonRoosterBeperkingenDto();
		var nachtBeperkingBegin = preferenceService.getString(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_BEGIN.name());
		if (nachtBeperkingBegin != null)
		{
			var nachtBeperkingBeginTime = LocalTime.parse(nachtBeperkingBegin);
			colonRoosterBeperkingDto.setNachtBeperkingBegin(nachtBeperkingBeginTime);
		}

		var nachtBeperkingEind = preferenceService.getString(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_EIND.name());
		if (nachtBeperkingEind != null)
		{
			var nachtBeperkingEindTime = LocalTime.parse(nachtBeperkingEind);
			colonRoosterBeperkingDto.setNachtBeperkingEind(nachtBeperkingEindTime);
		}
		var nachtBeperkingType = preferenceService.getString(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_TYPE.name());
		if (nachtBeperkingType != null)
		{
			colonRoosterBeperkingDto.setNachtBeperkingType(ColonRoosterBeperking.valueOf(nachtBeperkingType));
		}
		var zondagBeperkingType = preferenceService.getString(PreferenceKey.COLON_ROOSTER_ZONDAG_BEPERKING_TYPE.name());
		if (zondagBeperkingType != null)
		{
			colonRoosterBeperkingDto.setZondagBeperkingType(ColonRoosterBeperking.valueOf(zondagBeperkingType));
		}
		var zaterdagBeperkingType = preferenceService.getString(PreferenceKey.COLON_ROOSTER_ZATERDAG_BEPERKING_TYPE.name());
		if (zaterdagBeperkingType != null)
		{
			colonRoosterBeperkingDto.setZaterdagBeperkingType(ColonRoosterBeperking.valueOf(zaterdagBeperkingType));
		}
		return colonRoosterBeperkingDto;
	}

	private void validateRoosterBeperkingen(ColonRoosterBeperkingenDto roosterBeperkingDto) throws ValidatieException
	{

		if (roosterBeperkingDto.getNachtBeperkingBegin().isBefore(roosterBeperkingDto.getNachtBeperkingEind()))
		{
			throw new ValidatieException("error.weekend.beperking.tijden.incorrect");
		}

		if (controleerAfsprakenOpZaterdag(roosterBeperkingDto))
		{
			throw new ValidatieException("error.weekend.beperking.afspraakslots.op.zaterdag");
		}

		if (controleerAfsprakenOpZondag(roosterBeperkingDto))
		{
			throw new ValidatieException("error.weekend.beperking.afspraakslots.op.zondag");
		}

		if (controleerAfsprakenInNacht(roosterBeperkingDto))
		{
			throw new ValidatieException("error.weekend.beperking.afspraken.tijdens.beperking");
		}

	}

	private boolean controleerAfsprakenOpZaterdag(ColonRoosterBeperkingenDto roosterBeperkingDto)
	{
		if (roosterBeperkingDto.getZaterdagBeperkingType() == ColonRoosterBeperking.ZACHT)
		{
			return false;
		}

		return intakeAfspraakService.countAfsprakenOpDagVanDeWeek(DayOfWeek.SATURDAY) > 0;
	}

	private boolean controleerAfsprakenOpZondag(ColonRoosterBeperkingenDto roosterBeperkingDto)
	{
		if (roosterBeperkingDto.getZondagBeperkingType() == ColonRoosterBeperking.ZACHT)
		{
			return false;
		}
		return intakeAfspraakService.countAfsprakenOpDagVanDeWeek(DayOfWeek.SUNDAY) > 0;
	}

	private boolean controleerAfsprakenInNacht(ColonRoosterBeperkingenDto roosterBeperkingDto)
	{
		if (roosterBeperkingDto.getNachtBeperkingType() == ColonRoosterBeperking.ZACHT)
		{
			return false;
		}
		return intakeAfspraakService.countAfsprakenInNacht(roosterBeperkingDto.getNachtBeperkingBegin(),
			roosterBeperkingDto.getNachtBeperkingEind()) > 0;
	}

	@Override
	@Transactional
	public void updateRoosterBeperkingen(ColonRoosterBeperkingenDto roosterBeperkingenDto) throws ValidatieException
	{
		validateRoosterBeperkingen(roosterBeperkingenDto);

		preferenceService.putString(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_BEGIN.name(), roosterBeperkingenDto.getNachtBeperkingBegin().toString());
		preferenceService.putString(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_EIND.name(), roosterBeperkingenDto.getNachtBeperkingEind().toString());
		preferenceService.putEnum(PreferenceKey.COLON_ROOSTER_NACHT_BEPERKING_TYPE.name(), roosterBeperkingenDto.getNachtBeperkingType());
		preferenceService.putEnum(PreferenceKey.COLON_ROOSTER_ZATERDAG_BEPERKING_TYPE.name(), roosterBeperkingenDto.getZaterdagBeperkingType());
		preferenceService.putEnum(PreferenceKey.COLON_ROOSTER_ZONDAG_BEPERKING_TYPE.name(), roosterBeperkingenDto.getZondagBeperkingType());
	}
}
