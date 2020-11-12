package nl.rivm.screenit.util.cervix;

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

import java.util.List;

import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public class CervixMonsterUtil
{

	private static final String ALPHANUMERIC_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";

	private static final String ALPHANUMERIC_CHARACTERS_REVERSED = new StringBuilder().append(ALPHANUMERIC_CHARACTERS).reverse().toString();

	public static CervixUitstrijkje getUitstrijkje(CervixMonster monster)
	{
		return monster != null
			? (CervixUitstrijkje) HibernateHelper.deproxy(monster)
			: null;
	}

	public static CervixZas getZAS(CervixMonster monster)
	{
		return monster != null
			? (CervixZas) HibernateHelper.deproxy(monster)
			: null;
	}

	public static boolean isUitstrijkje(CervixMonster monster)
	{
		return HibernateHelper.deproxy(monster) instanceof CervixUitstrijkje;
	}

	public static boolean isZAS(CervixMonster monster)
	{
		return HibernateHelper.deproxy(monster) instanceof CervixZas;
	}

	public static String getMonsterControleLetters(CervixDossier cervixDossier)
	{
		CervixScreeningRonde laatsteScreeningRonde = cervixDossier.getLaatsteScreeningRonde();
		CervixUitnodiging laatsteUitnodiging = laatsteScreeningRonde.getLaatsteUitnodiging();
		String bsn = cervixDossier.getClient().getPersoon().getBsn();
		int bsnModulo = Integer.valueOf(bsn.charAt(bsn.length() - 1)) % 4;
		return getAlphaNumericCharacterVanHetAantalScreeningRondes(cervixDossier.getScreeningRondes(), bsnModulo)
			+ getAlphaNumericCharacterVanHetAantalUitnodigingen(laatsteScreeningRonde.getUitnodigingen(), bsnModulo)
			+ getAlphaNumericCharactorVanLaatste3MonsterIdDigits(laatsteUitnodiging.getMonster().getMonsterId());
	}

	private static String getAlphaNumericCharacterVanHetAantalScreeningRondes(List<CervixScreeningRonde> screeningRondes,
		int bsnModulo)
	{
		return bsnModulo == 0 || bsnModulo == 2
			? String.valueOf(ALPHANUMERIC_CHARACTERS.charAt(getAlphanumericModuloValueOf(screeningRondes.size() - 1)))
			: String.valueOf(ALPHANUMERIC_CHARACTERS_REVERSED.charAt(getAlphanumericModuloValueOf(screeningRondes.size() - 1)));
	}

	private static String getAlphaNumericCharacterVanHetAantalUitnodigingen(List<CervixUitnodiging> uitnodigingen,
		int bsnModulo)
	{
		return bsnModulo == 0 || bsnModulo == 1
			? String.valueOf(ALPHANUMERIC_CHARACTERS.charAt(getAlphanumericModuloValueOf(uitnodigingen.size() - 1)))
			: String.valueOf(ALPHANUMERIC_CHARACTERS_REVERSED.charAt(getAlphanumericModuloValueOf(uitnodigingen.size() - 1)));
	}

	private static String getAlphaNumericCharactorVanLaatste3MonsterIdDigits(String monsterId)
	{
		return String.valueOf(ALPHANUMERIC_CHARACTERS.charAt(getAlphanumericModuloValueOf(Integer.parseInt(monsterId.substring(monsterId.length() - 3)))));
	}

	private static int getAlphanumericModuloValueOf(int value)
	{
		return value % ALPHANUMERIC_CHARACTERS.length();
	}

}
