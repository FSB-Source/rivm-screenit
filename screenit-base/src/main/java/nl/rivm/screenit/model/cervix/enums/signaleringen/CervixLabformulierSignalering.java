package nl.rivm.screenit.model.cervix.enums.signaleringen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.stream.Collectors;

public enum CervixLabformulierSignalering
{

	BARCODE_STICKER_OVER_AGB_HEENGEPLAKT("(Barcode)sticker over AGB code heengeplakt"),

	HUISARTS_ID_ONTBREEKT("Huisarts ID ontbreekt"),

	DATUM_AFNAME_IS_ONJUIST_OF_ONTBREEKT_OP_FORMULIER("Datum afname is onjuist of ontbreekt op formulier"),

	AFNAMEDATUM_NIET_OF_VERKEERD_OVERGENOMEN_IN_SCREENIT("Afnamedatum niet of verkeerd overgenomen in ScreenIT"),

	@Deprecated
	KOPIE_AANVRAAGFORMULIER_GEBRUIKT("Kopie aanvraagformulier gebruikt");

	private String beschrijving;

	CervixLabformulierSignalering(String beschrijving)
	{
		this.beschrijving = beschrijving;
	}

	public String getBeschrijving()
	{
		return beschrijving;
	}

	public static List<CervixLabformulierSignalering> getMogelijkeSignaleringen()
	{
		List<CervixLabformulierSignalering> values = Arrays.asList(values());
		return values.stream().filter(value -> !value.equals(CervixLabformulierSignalering.KOPIE_AANVRAAGFORMULIER_GEBRUIKT)).collect(Collectors.toList());
	}
}
