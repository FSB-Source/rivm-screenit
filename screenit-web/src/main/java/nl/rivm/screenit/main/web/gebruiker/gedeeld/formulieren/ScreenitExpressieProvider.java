
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.colon.verslag.mdl.MdlPoliep;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.ExpressieType;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.api.resultaat.MeervoudigAntwoord;
import nl.topicuszorg.formulieren2.expressie.definitie.Expressie;
import nl.topicuszorg.formulieren2.expressie.definitie.impl.DefaultExpressieProvider;

import org.apache.commons.lang3.StringUtils;

public class ScreenitExpressieProvider extends DefaultExpressieProvider
{

	Map<String, Expressie> expressieCache = new HashMap<>();

	private static String DISPLAY_NAME_MANIER_VAN_VERWIJDEREN = null;

	@Override
	protected <T> Expressie<T> getExpressie(Class<T> resultType, FormulierResultaat formulierResultaat, String expressieString, ExpressieType expressieType, Integer herhaling)
	{
		String expressieKey = expressieString;
		if (herhaling != null)
		{
			expressieKey += "_" + herhaling;
		}
		Expressie<T> expressie = expressieCache.get(expressieKey);
		if (expressie == null)
		{
			expressie = super.getExpressie(resultType, formulierResultaat, expressieString, expressieType, herhaling);
			expressieCache.put(expressieKey, expressie);
		}
		return expressie;

	}

	@Override
	public Object getVariabeleWaarde(FormulierResultaat formulierResultaat, String var, Integer herhaling)
	{
		if ("manier_van_verwijderen_is_niet_niet_verwijderd".equals(var))
		{
			Boolean value = Boolean.TRUE;
			String displeyNameManierVanVerwijderen = getDisplayNameManierVanVerwijderen();
			for (Antwoord<?> antwoord : formulierResultaat.getAntwoorden())
			{
				VraagDefinitie<?> vraagDefinitie = antwoord.getVraagInstantie().getVraagDefinitie();
				String vraagDefinitieVraagString = vraagDefinitie.getVraag();

				if (StringUtils.isNotBlank(vraagDefinitieVraagString) && vraagDefinitieVraagString.equals(displeyNameManierVanVerwijderen)
					&& (herhaling == null || antwoord.getHerhaling().equals(herhaling)))
				{
					List<?> waarde = ((MeervoudigAntwoord<?>) antwoord).getValues();
					for (Object selectedValue : waarde)
					{
						if (selectedValue instanceof DSValue && ((DSValue) selectedValue).getCode().equals("9"))
						{
							value = Boolean.FALSE;
							break;
						}
					}
					break;
				}
			}
			return value;
		}
		else
		{
			return super.getVariabeleWaarde(formulierResultaat, var, herhaling);
		}
	}

	private static String getDisplayNameManierVanVerwijderen()
	{
		if (DISPLAY_NAME_MANIER_VAN_VERWIJDEREN == null)
		{
			for (Field field : MdlPoliep.class.getDeclaredFields())
			{
				VraagElement vraagElement = field.getAnnotation(VraagElement.class);
				if (vraagElement != null && vraagElement.code() != null && vraagElement.code().endsWith("145080"))
				{
					DISPLAY_NAME_MANIER_VAN_VERWIJDEREN = vraagElement.displayName();
					break;
				}
			}
		}
		return DISPLAY_NAME_MANIER_VAN_VERWIJDEREN;
	}

	public void resetCache()
	{
		expressieCache.clear();
	}
}
