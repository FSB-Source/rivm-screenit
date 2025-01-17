package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag_;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag_;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.composePredicatesOr;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OntvangenCdaBerichtSpecification
{
	public static Specification<OntvangenCdaBericht> heeftBerichtId(String berichtId)
	{
		return (r, q, cb) -> cb.equal(r.get(OntvangenCdaBericht_.berichtId), berichtId);
	}

	public static Specification<OntvangenCdaBericht> heeftBerichtStatusVerwerkingOfVerwerkt()
	{
		return (r, q, cb) -> r.get(OntvangenCdaBericht_.status).in(List.of(BerichtStatus.VERWERKING, BerichtStatus.VERWERKT));
	}

	public static Specification<OntvangenCdaBericht> heeftSetId(String setId)
	{
		return (r, q, cb) -> cb.equal(r.get(OntvangenCdaBericht_.setId), setId);
	}

	public static Specification<OntvangenCdaBericht> heeftVersieDieGroterIsDan(Long versie)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(OntvangenCdaBericht_.versie), versie);
	}

	public static Specification<OntvangenCdaBericht> heeftBerichtGeenStatusFoutOfWaarschuwing()
	{
		return (r, q, cb) -> cb.not(r.get(OntvangenCdaBericht_.status).in(List.of(BerichtStatus.FOUT, BerichtStatus.WAARSCHUWING)));
	}

	public static Specification<OntvangenCdaBericht> heeftBerichtStatus(BerichtStatus status)
	{
		return (r, q, cb) ->
			cb.equal(r.get(OntvangenCdaBericht_.status), status);
	}

	public static Specification<OntvangenCdaBericht> heeftBerichtTypeVoorBvo(Bevolkingsonderzoek bvo)
	{
		return (r, q, cb) ->
		{
			var berichtTypes = Arrays.stream(BerichtType.values())
				.filter(berichtType -> berichtType.getBevolkingsonderzoek() == bvo)
				.collect(Collectors.toList());
			return r.get(OntvangenCdaBericht_.berichtType).in(berichtTypes);
		};
	}

	public static Specification<OntvangenCdaBericht> maakZoekSpecification(BerichtZoekFilter filter)
	{
		return (r, q, cb) ->
		{
			boolean mdl = Boolean.TRUE.equals(filter.getMdlBerichten());
			boolean pa = Boolean.TRUE.equals(filter.getPaLabBerichten());
			boolean cyto = Boolean.TRUE.equals(filter.getCytologieBerichten());
			boolean followUp = Boolean.TRUE.equals(filter.getFollowUpBerichten());
			boolean colon = mdl || pa;
			boolean geenBerichtType = !(mdl || pa || cyto || followUp);

			var predicates = new ArrayList<Predicate>();
			if (geenBerichtType)
			{
				predicates.add(cb.isNull(r.get(OntvangenCdaBericht_.berichtType)));
			}
			else
			{

				var berichtTypePredicates = new ArrayList<Predicate>();
				if (colon)
				{
					var subquery = q.subquery(Long.class);
					var rootColon = subquery.from(ColonVerslag.class);
					subquery.select(join(rootColon, ColonVerslag_.ontvangenCdaBericht).get(OntvangenCdaBericht_.id));
					berichtTypePredicates.add(r.get(AbstractHibernateObject_.id).in(subquery));
				}
				if (followUp)
				{
					var subquery = q.subquery(Long.class);
					var rootMamma = subquery.from(MammaVerslag.class);
					subquery.select(join(rootMamma, MammaVerslag_.ontvangenCdaBericht).get(OntvangenCdaBericht_.id));
					berichtTypePredicates.add(r.get(AbstractHibernateObject_.id).in(subquery));
				}
				if (cyto)
				{
					var subquery = q.subquery(Long.class);
					var rootCervix = subquery.from(CervixVerslag.class);
					subquery.select(join(rootCervix, CervixVerslag_.ontvangenCdaBericht).get(OntvangenCdaBericht_.id));
					berichtTypePredicates.add(r.get(AbstractHibernateObject_.id).in(subquery));
				}
				predicates.add(composePredicatesOr(cb, berichtTypePredicates));

				var berichtTypes = new ArrayList<BerichtType>();
				if (mdl)
				{
					berichtTypes.add(BerichtType.MDL_VERSLAG);
				}
				if (pa)
				{
					berichtTypes.add(BerichtType.PA_LAB_VERSLAG);
				}
				if (cyto)
				{
					berichtTypes.add(BerichtType.CERVIX_CYTOLOGIE_VERSLAG);
				}
				if (followUp)
				{
					berichtTypes.add(BerichtType.MAMMA_PA_FOLLOW_UP_VERSLAG);
				}
				predicates.add(r.get(OntvangenCdaBericht_.berichtType).in(berichtTypes));
			}

			if (StringUtils.isNotBlank(filter.getText()))
			{
				var text = filter.getText();
				predicates.add(cb.or(
					containsCaseInsensitive(cb, r.get(OntvangenCdaBericht_.berichtId), text),
					containsCaseInsensitive(cb, r.get(OntvangenCdaBericht_.setId), text),
					containsCaseInsensitive(cb, r.get(OntvangenCdaBericht_.projectVersion), text)
				));
			}
			return composePredicates(cb, predicates);
		};
	}
}
