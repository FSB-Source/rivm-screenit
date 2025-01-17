package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftBeeldenMetGunstigeUitslag;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaDossierSpecification
{
	public static Specification<MammaDossier> heeftAlleenGunstigeUitslagenMetBeelden()
	{
		return heeftMeerDanDrieGunstigeUitslagen().and(heeftAlleenGunstigeUitslagen());
	}

	private static Specification<MammaDossier> heeftAlleenGunstigeUitslagen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaDossier.class);
			var rondeJoin = join(subqueryRoot, MammaDossier_.screeningRondes);
			var onderzoekJoinDossier = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var beoordelingJoinDossier = join(onderzoekJoinDossier, MammaOnderzoek_.laatsteBeoordeling);

			subquery.select(subqueryRoot.get(TablePerClassHibernateObject_.id))
				.where(
					cb.or(
						beoordelingJoinDossier.get(MammaBeoordeling_.status).in(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG, MammaBeoordelingStatus.ONBEOORDEELBAAR),
						cb.and(
							cb.equal(beoordelingJoinDossier.get(MammaBeoordeling_.status), MammaBeoordelingStatus.UITSLAG_GUNSTIG),
							cb.equal(rondeJoin.get(MammaScreeningRonde_.followUpConclusieStatus), MammaFollowUpConclusieStatus.FALSE_NEGATIVE)
						)
					)
				);

			return cb.not(r.get(TablePerClassHibernateObject_.id).in(subquery));
		};
	}

	private static Specification<MammaDossier> heeftMeerDanDrieGunstigeUitslagen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaScreeningRonde.class);

			subquery.select(cb.count(subqueryRoot.get(TablePerClassHibernateObject_.id)))
				.where(
					heeftBeeldenMetGunstigeUitslag(r).toPredicate(subqueryRoot, q, cb)
				);
			return cb.and(cb.lt(cb.literal(3L), subquery));
		};
	}

}
