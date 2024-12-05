package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Bezwaar_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportGewensteUitslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.StringUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaPalgaSpecification
{

	public static Specification<MammaDossier> heeftGeenClientGbaStatusAfgevoerdOfBezwaar()
	{
		return (r, q, cb) ->
		{
			var clientJoin = join(r, MammaDossier_.client);
			return cb.and(cb.notEqual(clientJoin.get(Client_.gbaStatus), GbaStatus.AFGEVOERD), cb.notEqual(clientJoin.get(Client_.gbaStatus), GbaStatus.BEZWAAR));
		};
	}

	public static Specification<MammaDossier> heeftGeenActieveBezwaarVoorPalga()
	{
		return (r, q, cb) ->
		{
			var hoofdClientJoin = join(r, MammaDossier_.client);

			var subquery = q.subquery(BezwaarMoment.class);
			var subroot = subquery.from(Bezwaar.class);

			subquery.select(subroot.join(Bezwaar_.bezwaarMoment)).where(
				cb.and(subroot.get(Bezwaar_.type).in(Arrays.asList(BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, BezwaarType.GEEN_KWALITEITSWAARBORGING)),
					cb.equal(subroot.get(Bezwaar_.bevolkingsonderzoek), Bevolkingsonderzoek.MAMMA)));
			return cb.or(cb.not(cb.in(hoofdClientJoin.get(Client_.laatstVoltooideBezwaarMoment)).value(subquery)),
				cb.isNull(hoofdClientJoin.get(Client_.laatstVoltooideBezwaarMoment)));
		};
	}

	public static Specification<MammaDossier> voldoetAanPalgaExportConfig(MammaPalgaExportConfig palgaExportConfig, LocalDate vandaag)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaDossier_.screeningRondes);
			var laatsteOnderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var laatsteBeoordelingJoin = join(laatsteOnderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
			var creatieDatumTruncate = DateSpecification.truncate("day", laatsteOnderzoekJoin.get(MammaOnderzoek_.creatieDatum), cb);
			switch (palgaExportConfig.getPeriodeType())
			{
			case ONDERZOEKS_DATUM_PERIODE:
				return cb.and(laatsteBeoordelingJoin.get(MammaBeoordeling_.status).in(gekozenUitslagen(palgaExportConfig.getGewensteUitslag())),
					cb.lessThanOrEqualTo(creatieDatumTruncate, palgaExportConfig.getTotEnMetOnderzoeksDatum()),
					cb.greaterThanOrEqualTo(creatieDatumTruncate, palgaExportConfig.getVanafOnderzoeksDatum()));
			case ONDERZOEKS_DATUM_AANTAL_MAANDEN_TERUG:
				var maximaleDatumOnderzoekInVerleden = DateUtil.toUtilDate(vandaag.minusMonths(palgaExportConfig.getOnderzoekAantalMaandenTerug()));
				return cb.and(laatsteBeoordelingJoin.get(MammaBeoordeling_.status).in(gekozenUitslagen(palgaExportConfig.getGewensteUitslag())),
					cb.greaterThanOrEqualTo(creatieDatumTruncate, maximaleDatumOnderzoekInVerleden));
			default:
				return null;
			}
		};
	}

	private static List<MammaBeoordelingStatus> gekozenUitslagen(MammaPalgaExportGewensteUitslag uitslagkeuze)
	{
		switch (uitslagkeuze)
		{
		case ALLEEN_ONGUNSTIG:
			return List.of(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
		case ALLEEN_GUNSTIG:
			return List.of(MammaBeoordelingStatus.UITSLAG_GUNSTIG);
		case GUNSTIG_EN_ONGUNSTIG:
			return List.of(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG, MammaBeoordelingStatus.UITSLAG_GUNSTIG);
		default:
			throw new IllegalStateException("Unexpected value: " + uitslagkeuze);
		}
	}

	public static Specification<Client> heeftPalgaPatid3Voorwaarden(GbaPersoon persoon)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			join(r, Client_.mammaDossier);
			return cb.and(
				cb.equal(persoonJoin.get(GbaPersoon_.geboortedatum), persoon.getGeboortedatum()),
				cb.like(persoonJoin.get(GbaPersoon_.achternaam), getWildcardAchternaam(persoon.getAchternaam())));
		};
	}

	private static String getWildcardAchternaam(String achternaam)
	{
		StringBuilder wildcardAchternaam = new StringBuilder();
		char[] chars = achternaam.toCharArray();
		for (int i = 0; i < chars.length; i++)
		{
			if (!isGenormaliseerdKarakter(chars[i], i != chars.length - 1 ? chars[i + 1] : ' '))
			{
				wildcardAchternaam.append(chars[i]);
			}
			else
			{
				wildcardAchternaam.append("_");
			}
		}
		return wildcardAchternaam.toString();
	}

	private static boolean isGenormaliseerdKarakter(char karakter, char opvolgendKarakter)
	{
		karakter = Character.toLowerCase(karakter);
		opvolgendKarakter = Character.toLowerCase(opvolgendKarakter);
		return !StringUtil.isAlfabetKarakter(karakter) || karakter == 'y' || (karakter == 'i' && opvolgendKarakter == 'j');
	}

}
