package nl.rivm.screenit.model;

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

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public enum OrganisatieParameterKey
{
	CERVIX_MAX_AANTAL_CLIENTEN_SELECTIE(OrganisatieType.BMHK_LABORATORIUM, Integer.class, 3000, Bevolkingsonderzoek.CERVIX),

	CERVIX_MAX_AANTAL_HERINNERINGEN_ZAS(OrganisatieType.RIVM, Integer.class, 1000, Bevolkingsonderzoek.CERVIX),

	CERVIX_MAX_AANTAL_HERINNERINGEN_UITSTRIJKJE(OrganisatieType.RIVM, Integer.class, 15000, Bevolkingsonderzoek.CERVIX),

	CERVIX_MAX_AANTAL_ZAS_NAAR_INPAKCENTRUM(OrganisatieType.INPAKCENTRUM, Integer.class, 20000, Bevolkingsonderzoek.CERVIX),

	MAX_MERGED_BRIEVEN_PDF_SIZE_MB(OrganisatieType.SCREENINGSORGANISATIE, Integer.class, 999, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA),;

	private final OrganisatieType organisatieType;

	private final Class<?> valueType;

	private final Integer maxValue;

	private final Bevolkingsonderzoek[] bevolkingsonderzoeken;

	OrganisatieParameterKey(OrganisatieType organisatieType, Class<?> valueType, Integer maxValue, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		this.organisatieType = organisatieType;
		this.valueType = valueType;
		this.maxValue = maxValue;
		this.bevolkingsonderzoeken = bevolkingsonderzoeken.clone();
	}

	public OrganisatieType getOrganisatieType()
	{
		return organisatieType;
	}

	public Class<?> getValueType()
	{
		return valueType;
	}

	public Integer getMaxValue()
	{
		return maxValue;
	}

	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

}
