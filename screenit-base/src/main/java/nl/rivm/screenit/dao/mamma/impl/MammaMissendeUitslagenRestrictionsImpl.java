package nl.rivm.screenit.dao.mamma.impl;

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

import java.time.temporal.ChronoUnit;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaMissendeUitslagenRestrictions;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class MammaMissendeUitslagenRestrictionsImpl implements MammaMissendeUitslagenRestrictions
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	public void addBeeldenZonderUitslagRestrictions(Criteria criteria)
	{
		var signaleringsTermijn = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);

		var vandaag = currentDateSupplier.getLocalDate();

		var signalerenVanaf = vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		var minimaleSignaleringsDatum = vandaag.minusDays(signaleringsTermijn);
		var minimaleSignaleringsDatumOnderbrokenOnderzoek = vandaag.minusDays(
			Math.min(signaleringsTermijn + ChronoUnit.DAYS.between(vandaag.minusMonths(Constants.MAMMA_MAX_AANTAL_MAANDEN_GEEN_UITSLAG_ONDERBROKEN_ONDERZOEK), vandaag),
				OrganisatieParameterKey.MAMMA_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN.getMaxValue()));

		criteria.createAlias("onderzoek.mammografie", "mammografie");
		criteria.createAlias("onderzoek.afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");

		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.gt("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
					DateRestrictions.gtProperty("onderzoek.creatieDatum", "dossier.datumLaatstGecontroleerdeSignalering")
				),
				Restrictions.and(
					Restrictions.or(
						DateRestrictions.le("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
						Restrictions.isNull("dossier.datumLaatstGecontroleerdeSignalering")
					),
					Restrictions.gt("onderzoek.creatieDatum", DateUtil.toUtilDate(signalerenVanaf))
				)
			)
		);

		criteria.add(Restrictions.or(
			Restrictions.and(
				Restrictions.ne("onderzoek.status", MammaOnderzoekStatus.ONDERBROKEN),
				DateRestrictions.le("onderzoek.creatieDatum", DateUtil.toUtilDate(minimaleSignaleringsDatum))
			),
			Restrictions.and(
				Restrictions.eq("onderzoek.status", MammaOnderzoekStatus.ONDERBROKEN),
				DateRestrictions.le("onderzoek.creatieDatum", DateUtil.toUtilDate(minimaleSignaleringsDatumOnderbrokenOnderzoek))
			)
		));

		criteria.add(Restrictions.in("mammografie.ilmStatus", MammaMammografieIlmStatus.BEELDEN_BESCHIKBAAR_OF_BESCHIKBAAR_GEWEEST));

		criteria.add(
			Restrictions.or(
				Subqueries.notExists(maakUitslagBriefSubquery().setProjection(Projections.id())),
				Subqueries.propertyGt("onderzoek.creatieDatum", maakUitslagBriefSubquery().setProjection(Projections.max("creatieDatum")))
			)
		);
		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
	}

	private DetachedCriteria maakUitslagBriefSubquery()
	{
		var criteria = DetachedCriteria.forClass(MammaBrief.class, "brief");
		criteria.createAlias("brief.projectBrief", "projectBrief", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.eqProperty("brief.screeningRonde", "ronde.id"));
		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", false),
					Restrictions.in("brief.briefType", BriefType.getMammaUitslagBriefTypen()),
					Restrictions.eq("brief.gegenereerd", true)
				),
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", true),
					Restrictions.in("projectBrief.briefType", BriefType.getMammaUitslagBriefTypen()),
					Restrictions.eq("projectBrief.gegenereerd", true)
				)
			)
		);
		return criteria;
	}
}
