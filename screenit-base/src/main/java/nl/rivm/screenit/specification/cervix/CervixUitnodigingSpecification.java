package nl.rivm.screenit.specification.cervix;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixBrief_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.CervixZas_;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixUitnodigingSpecification
{
	public static Specification<CervixUitnodiging> heeftGeenVertrokkenPersoonUitNederlandDatum()
	{
		return PersoonSpecification.heeftGeenVertrokkenUitNederlandDatumPredicate()
			.toSpecification(r ->
			{
				var ronde = SpecificationUtil.join(r, CervixUitnodiging_.screeningRonde);
				var dossier = SpecificationUtil.join(ronde, CervixScreeningRonde_.dossier);
				var client = SpecificationUtil.join(dossier, CervixDossier_.client);
				return SpecificationUtil.join(client, Client_.persoon);
			});
	}

	public static Specification<CervixUitnodiging> heeftGeenPersoonMetOverledenDatum()
	{
		return PersoonSpecification.heeftGeenOverledenDatumPredicate()
			.toSpecification(r ->
			{
				var ronde = SpecificationUtil.join(r, CervixUitnodiging_.screeningRonde);
				var dossier = SpecificationUtil.join(ronde, CervixScreeningRonde_.dossier);
				var client = SpecificationUtil.join(dossier, CervixDossier_.client);
				return SpecificationUtil.join(client, Client_.persoon);
			});
	}

	public static Specification<CervixUitnodiging> heeftMergedBrieven()
	{
		return (r, q, cb) ->
		{
			var brief = SpecificationUtil.join(r, CervixUitnodiging_.brief);
			return cb.isNotNull(brief.get(CervixBrief_.mergedBrieven));
		};
	}

	public static Specification<CervixUitnodiging> heeftClientMetIndicatieAanwezig()
	{
		return ClientSpecification.heeftIndicatie()
			.toSpecification(r ->
			{
				var ronde = SpecificationUtil.join(r, CervixUitnodiging_.screeningRonde);
				var dossier = SpecificationUtil.join(ronde, CervixScreeningRonde_.dossier);
				return SpecificationUtil.join(dossier, CervixDossier_.client);
			});
	}

	public static Specification<CervixUitnodiging> heeftLopendeRonde()
	{
		return (r, q, cb) ->
		{
			var ronde = SpecificationUtil.join(r, CervixUitnodiging_.screeningRonde);
			return cb.equal(ronde.get(ScreeningRonde_.status), ScreeningRondeStatus.LOPEND);
		};
	}

	public static Specification<CervixUitnodiging> heeftHerinneren(boolean heeftHerinneren)
	{
		return (r, q, cb) ->
			cb.equal(r.get(CervixUitnodiging_.herinneren), heeftHerinneren);
	}

	public static Specification<CervixUitnodiging> heeftGeenGeanulleerdeHerinneringDatum()
	{
		return (r, q, cb) -> cb.isNull(r.get(CervixUitnodiging_.herinnerenGeannuleerdDatum));
	}

	public static Specification<CervixUitnodiging> heeftMonsterType(CervixMonsterType monsterType)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixUitnodiging_.monsterType), monsterType);
	}

	public static Specification<CervixUitnodiging> heeftMergedBrievenVoorDatum(LocalDate datum)
	{
		return CervixMergedBrievenSpecification.heeftPrintDatumVoorDatum(datum)
			.toSpecification(r ->
			{
				var brief = SpecificationUtil.join(r, CervixUitnodiging_.brief);
				return SpecificationUtil.join(brief, CervixBrief_.mergedBrieven);
			});
	}

	public static Specification<CervixUitnodiging> heeftZasDieVerstuurdIsVoorDatum(LocalDate datum)
	{
		return (r, q, cb) ->
		{
			var monster = SpecificationUtil.join(r, CervixUitnodiging_.monster);
			return cb.lessThan(cb.treat(monster, CervixZas.class).get(CervixZas_.verstuurd), DateUtil.toUtilDate(datum));
		};
	}

	public static Specification<CervixUitnodiging> heeftBriefMetBrieftype(BriefType... briefTypes)
	{
		return CervixBriefSpecification.heeftBriefInBrieftype(briefTypes).toSpecification(r -> SpecificationUtil.join(r, CervixUitnodiging_.brief));
	}

	public static Specification<CervixUitnodiging> heeftScreeningRonde(CervixScreeningRonde screeningRonde)
	{
		return (r, q, cb) ->
		{
			var uitnodiging = SpecificationUtil.join(r, CervixUitnodiging_.screeningRonde);
			return cb.equal(uitnodiging, screeningRonde);
		};
	}

}
