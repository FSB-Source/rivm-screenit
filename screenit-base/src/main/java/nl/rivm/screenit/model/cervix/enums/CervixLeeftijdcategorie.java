package nl.rivm.screenit.model.cervix.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.enums.BriefType;

import com.google.common.primitives.Ints;

@Getter
@AllArgsConstructor
public enum CervixLeeftijdcategorie
{
	_30(30, BriefType.CERVIX_UITNODIGING_30),
	_35(35, BriefType.CERVIX_UITNODIGING_35),
	_40(40, BriefType.CERVIX_UITNODIGING_40),
	_45(45, BriefType.CERVIX_UITNODIGING_45),
	_50(50, BriefType.CERVIX_UITNODIGING_50),
	_55(55, BriefType.CERVIX_UITNODIGING_55),
	_60(60, BriefType.CERVIX_UITNODIGING_60),
	_65(65, BriefType.CERVIX_UITNODIGING_65),
	_70(70, null);

	private final int leeftijd;

	private final BriefType uitnodigingsBrief;

	public static CervixLeeftijdcategorie getLeeftijdcategorie(LocalDate geboortedatum, LocalDateTime creatiedatum)
	{
		int leeftijd = Ints.checkedCast(ChronoUnit.YEARS.between(geboortedatum, creatiedatum));
		int remainder = leeftijd % 5;
		int leeftijdCategorieInteger = leeftijd - remainder;
		switch (leeftijdCategorieInteger)
		{
		case 30:
			return _30;
		case 35:
			return _35;
		case 40:
			return _40;
		case 45:
			return _45;
		case 50:
			return _50;
		case 55:
			return _55;
		case 60:
			return _60;
		case 65:
			return _65;
		default:
			throw new IllegalStateException("Leeftijd voor CervixLeeftijdcategorie is niet geldig.");
		}
	}

	public CervixLeeftijdcategorie volgende()
	{
		switch (this)
		{
		case _30:
			return _35;
		case _35:
			return _40;
		case _40:
			return _45;
		case _45:
			return _50;
		case _50:
			return _55;
		case _55:
			return _60;
		case _60:
			return _65;
		case _65:
			return _70;
		}
		return null;
	}
}
