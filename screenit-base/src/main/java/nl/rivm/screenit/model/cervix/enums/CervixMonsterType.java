package nl.rivm.screenit.model.cervix.enums;

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
import java.util.List;

import nl.rivm.screenit.model.enums.BriefType;

public enum CervixMonsterType
{

	UITSTRIJKJE(Arrays.asList(
		BriefType.CERVIX_UITNODIGING,
		BriefType.CERVIX_UITNODIGING_CONTROLEUITSTRIJKJE,
		BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_HPV_ONBEOORDEELBAAR,
		BriefType.CERVIX_UITSTRIJKJE_NIET_ANALYSEERBAAR_OF_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_UITSTRIJKJE_TWEEDE_KEER_ONBEOORDEELBAAR,
		BriefType.CERVIX_CYTOLOGIE_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_HPV_POSITIEF,
		BriefType.CERVIX_ZAS_TWEEDE_KEER_ONBEOORDEELBAAR,
		BriefType.CERVIX_ZAS_NA_HPV_POSITIEF,
		BriefType.CERVIX_ZAS_NA_CYTOLOGIE_ONBEOORDEELBAAR)),

	ZAS((Arrays.asList(
		BriefType.CERVIX_ZAS_UITNODIGING,
		BriefType.CERVIX_ZAS_NIET_ANALYSEERBAAR_OF_ONBEOORDEELBAAR)));

	List<BriefType> briefTypen;

	CervixMonsterType(List<BriefType> briefTypen)
	{
		this.briefTypen = briefTypen;
	}

	public static CervixMonsterType getMonsterType(BriefType briefType)
	{
		if (UITSTRIJKJE.briefTypen.contains(briefType))
		{
			return UITSTRIJKJE;
		}
		else if (ZAS.briefTypen.contains(briefType))
		{
			return ZAS;
		}
		return null;
	}

	public List<BriefType> getBrieftypen()
	{
		return briefTypen;
	}
}
