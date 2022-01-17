package nl.rivm.screenit.model.cervix.enums.signaleringen;

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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;

public enum CervixMonsterSignalering
{
	BARCODES_POT_FORMULIER_NIET_OVEREEN(CervixMonsterType.UITSTRIJKJE, true, "Barcodes pot en formulier komen niet overeen"),

	BARCODE_POT_FORMULIER_ONTBREEKT(CervixMonsterType.UITSTRIJKJE, true, "Barcode pot of formulier ontbreekt"),

	GEEN_AANVRAAG_FORMULIER_WEL_MONSTER(CervixMonsterType.UITSTRIJKJE, false, "Geen aanvraagformulier/wel monster"),

	VERKEERDE_FORMULIER(CervixMonsterType.UITSTRIJKJE, true, "Verkeerde formulier"),

	BARCODE_ONTBREEKT_BSN_AANWEZIG(CervixMonsterType.UITSTRIJKJE, true, "Barcode ontbreekt op pot én formulier, wel BSN aanwezig én BSN in ScreenIT"),

	POTJE_NIET_GOED_AFGESLOTEN_HEEFT_GELEKT(CervixMonsterType.UITSTRIJKJE, true, "Pot niet goed afgesloten, heeft gelekt. (wel analyseerbaar)"),

	BARCODE_VERTICAAL_GEPLAKT(CervixMonsterType.UITSTRIJKJE, true, "Barcode verticaal geplakt (i.p.v. horizontaal)"),

	ZAKJE_DICHTGENIET_OF_FORMULIER_AAN_ZAKJE_GENIET(CervixMonsterType.UITSTRIJKJE, true, "Zakje dichtgeniet of formulier aan zakje geniet"),

	KOPIE_AANVRAAGFORMULIER_GEBRUIKT(CervixMonsterType.UITSTRIJKJE, false, "Kopie aanvraagformulier gebruikt"),

	ZAS_AANGELEVERD_ZONDER_DOP_BLISTER(CervixMonsterType.ZAS, true, "Onjuiste aanlevering ZAS (zonder blister en/of safetybag)"),

	@Deprecated
	ZAS_FYSIEKE_SCHADE(CervixMonsterType.ZAS, false, "ZAS en/of borstel met fysieke schade"),

	@Deprecated
	ZAS_ZONDER_BORSTEL(CervixMonsterType.ZAS, false, "ZAS zonder borstel"),

	@Deprecated
	ZAS_VERONTREINIGD(CervixMonsterType.ZAS, false, "ZAS verontreinigd"),

	ZAS_DICHTE_BLISTER(CervixMonsterType.ZAS, false, "Dichte blister"),

	ZAS_BARCODE_ZAS_NIET_SCANBAAR(CervixMonsterType.ZAS, false, "Barcode ZAS niet scanbaar"),

	ZAS_FYSIEKE_SCHADE_WEL_ANALYSEERBAAR(CervixMonsterType.ZAS, false, "Fysieke schade (ZAS wel analyseerbaar)"),

	ZAS_VERVORMDE_BRUSH(CervixMonsterType.ZAS, false, "Vervormde brush");

	private CervixMonsterType monsterType;

	private String beschrijving;

	private boolean bijDigitaalLabformulier;

	CervixMonsterSignalering(CervixMonsterType monsterType, boolean bijDigitaalLabformulier, String beschrijving)
	{
		this.monsterType = monsterType;
		this.beschrijving = beschrijving;
		this.bijDigitaalLabformulier = bijDigitaalLabformulier;
	}

	public String getBeschrijving()
	{
		return beschrijving;
	}

	public boolean isBijDigitaalLabformulier()
	{
		return bijDigitaalLabformulier;
	}

	public CervixMonsterType getMonsterType()
	{
		return monsterType;
	}

	public static List<CervixMonsterSignalering> getMogelijkeSignaleringen(CervixMonsterType monsterType, boolean isDigitaal)
	{
		return Arrays.stream(values())
			.filter(cervixMonsterSignalering -> cervixMonsterSignalering.getMonsterType().equals(monsterType))
			.filter(cervixMonsterSignalering -> !isDigitaal || cervixMonsterSignalering.isBijDigitaalLabformulier())
			.filter(cervixMonsterSignalering -> !cervixMonsterSignalering.equals(CervixMonsterSignalering.ZAS_FYSIEKE_SCHADE)
				&& !cervixMonsterSignalering.equals(CervixMonsterSignalering.ZAS_ZONDER_BORSTEL)
				&& !cervixMonsterSignalering.equals(CervixMonsterSignalering.ZAS_VERONTREINIGD))
			.collect(Collectors.toList());
	}
}
