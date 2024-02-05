package nl.rivm.screenit.mamma.planning.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dao.mamma.MammaSelectieRestrictions;
import nl.rivm.screenit.mamma.planning.dao.PlanningClientDao;
import nl.rivm.screenit.mamma.planning.dao.dto.ClientDaoDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;

import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenTotEnMetGeboortedatum;
import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenVanafGeboortedatum;

@Repository
@RequiredArgsConstructor
public class PlanningClientDaoImpl extends AbstractAutowiredDao implements PlanningClientDao
{

	private final MammaSelectieRestrictions selectieRestricties;

	@Override
	public List<ClientDaoDto> clientenVoorConceptmodel()
	{
		Criteria criteria = getSession().createCriteria(Client.class, "client");
		criteria.createAlias("client.mammaDossier", "dossier");
		criteria.createAlias("dossier.deelnamekans", "deelnamekans");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("adres.gbaGemeente", "gemeente");
		criteria.createAlias("gemeente.screeningOrganisatie", "so");
		criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteScreeningRonde.laatsteUitstel", "laatsteUitstel", JoinType.LEFT_OUTER_JOIN, Restrictions.isNull("laatsteUitstel.geannuleerdOp"));
		criteria.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteUitnodiging.laatsteAfspraak", "laatsteAfspraak", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteAfspraak.standplaatsPeriode", "laatsteAfspraakStandplaatsPeriode", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("dossier.screeningRondeEvent", "screeningRondeEvent", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("volgendeUitnodiging.interval", "uitnodigingsInterval", JoinType.LEFT_OUTER_JOIN);

		selectieRestricties.addStandaardSelectieRestricties(criteria);

		var actiefUitstelRestriction = Restrictions.and(
			Restrictions.isNotNull("laatsteUitstel.id"),
			Restrictions.isNull("laatsteUitstel.uitnodiging"),
			Restrictions.eq("laatsteScreeningRonde.status", ScreeningRondeStatus.LOPEND));

		criteria.add(Restrictions.or(
			Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(plannenVanafGeboortedatum), DateUtil.toUtilDate(plannenTotEnMetGeboortedatum)),
			actiefUitstelRestriction));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("client.id"), "id")
			.add(Projections.property("persoon.geboortedatum"), "geboortedatum")
			.add(Projections.property("adres.postcode"), "postcode")
			.add(Projections.property("tijdelijkGbaAdres.postcode"), "tijdelijkGbaPostcode")
			.add(Projections.property("so.id"), "screeningOrgansatieId")
			.add(Projections.property("dossier.id"), "dossierId")
			.add(Projections.property("dossier.doelgroep"), "doelgroep")
			.add(Projections.property("dossier.tehuis.id"), "tehuisId")
			.add(Projections.property("dossier.eersteOnderzoek"), "eersteOnderzoek")
			.add(Projections.property("dossier.laatsteMammografieAfgerond"), "laatsteMammografieAfgerondOp")
			.add(Projections.property("deelnamekans.deelnamekans"), "deelnamekans")
			.add(Projections.property("laatsteScreeningRonde.creatieDatum"), "screeningRondeCreatieDatum")
			.add(Projections.property("laatsteScreeningRonde.standplaatsRonde.id"), "oorspronkelijkeStandplaatsRondeId")
			.add(Projections.property("laatsteScreeningRonde.isGeforceerd"), "screeningRondeIsGeforceerd")
			.add(Projections.property("laatsteUitstel.standplaats.id"), "uitstelStandplaatsId")
			.add(Projections.property("laatsteUitstel.streefDatum"), "uitstelStreefDatum")
			.add(Projections.property("laatsteUitstel.uitstelReden"), "uitstelReden")
			.add(Projections.property("laatsteUitstel.uitnodiging.id"), "uitstelUitnodigingId")
			.add(Projections.property("laatsteUitnodiging.standplaatsRonde.id"), "uitnodigingStandplaatsRondeId")
			.add(Projections.property("laatsteAfspraakStandplaatsPeriode.standplaatsRonde.id"), "afspraakStandplaatsRondeId")
			.add(Projections.property("laatsteAfspraak.afgezegdOp"), "afspraakAfgezegdOp")
			.add(Projections.property("screeningRondeEvent.voorgaandeScreeningRondes"), "voorgaandeScreeningRondes")
			.add(Projections.property("laatsteUitnodiging.creatieDatum"), "laatsteUitnodigingDatum")
			.add(Projections.property("laatsteAfspraak.status"), "afspraakStatus")
			.add(Projections.property("laatsteAfspraak.vanaf"), "afspraakMoment")
			.add(Projections.property("uitnodigingsInterval.type"), "uitnodigingsIntervalType")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(ClientDaoDto.class));

		return criteria.list();
	}
}
