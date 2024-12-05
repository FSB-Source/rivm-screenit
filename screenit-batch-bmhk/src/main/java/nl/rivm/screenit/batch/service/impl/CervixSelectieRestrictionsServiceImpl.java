package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@RequiredArgsConstructor
public class CervixSelectieRestrictionsServiceImpl implements CervixSelectieRestrictionsService
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	public void addClientSelectieRestrictions(Criteria criteria)
	{
		addClientSelectieRestrictions(criteria, 0);
	}

	@Override
	public void addClientSelectieRestrictions(Criteria criteria, int daysToAddToToday)
	{
		addLeftOuterJoins(criteria);

		LocalDate vandaag = dateSupplier.getLocalDate();
		if (daysToAddToToday > 0)
		{
			vandaag = vandaag.minusDays(daysToAddToToday);
		}

		LocalDate geboortedatumMinimaal = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
		LocalDate geboortedatumMinimaalExtraRonde = vandaag.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());

		LocalDate exactDertigJaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		LocalDate exact35JaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._35.getLeeftijd());

		criteria.add(
			Restrictions.and( 
				Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(exactDertigJaarGeleden)),
				Restrictions.or(
					Restrictions.isNull("ronde.uitstel"),
					Restrictions.isNotNull("uitstel.geannuleerdDatum"),
					Restrictions.le("dossier.volgendeRondeVanaf", DateUtil.toUtilDate(vandaag))
				),
				Restrictions.or(
					Restrictions.and(
						Restrictions.isNull("dossier.volgendeRondeVanaf"),
						Restrictions.or(
							Restrictions.isNull("dossier.laatsteScreeningRonde"),
							Restrictions.eq("ronde.aangemeld", true),
							Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(exact35JaarGeleden)))),
					Restrictions.le("dossier.volgendeRondeVanaf", DateUtil.toUtilDate(vandaag)))));

		criteria.add(
			Restrictions.or(
				Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal)),
				Restrictions.and(
					Restrictions.eq("ronde.leeftijdcategorie", CervixLeeftijdcategorie._60),
					Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaalExtraRonde)),
					Restrictions.or(

						Restrictions.and(
							Restrictions.eq("laatsteHpvBeoordeling.hpvUitslag", CervixHpvBeoordelingWaarde.POSITIEF),
							Restrictions.or(
								Restrictions.isNull("cytologieVerslag.cytologieUitslag"),
								Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1)),
							Restrictions.or(
								Restrictions.isNull("vervolgonderzoekVerslag.cytologieUitslag"),
								Restrictions.eq("vervolgonderzoekVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1))),

						Restrictions.and(
							Restrictions.eq("analyseresultaten.hpvohr", CervixHpvResultValue.POS_OTHER_HR_HPV),
							Restrictions.or(
								Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP2),
								Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP3A1)
							),
							Restrictions.isNull("vervolgonderzoekVerslag.id")
						)))));
	}

	private void addLeftOuterJoins(Criteria criteria)
	{
		criteria.createAlias("dossier.laatsteScreeningRonde", "ronde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.monsterHpvUitslag", "monsterHpvUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("monsterHpvUitslag.laatsteHpvBeoordeling", "laatsteHpvBeoordeling", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeCytologieUitslag", "uitstrijkjeCytologieUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeCytologieUitslag.cytologieVerslag", "cytologieVerslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeVervolgonderzoekUitslag", "uitstrijkjeVervolgonderzoekUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeVervolgonderzoekUitslag.cytologieVerslag", "vervolgonderzoekVerslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("laatsteHpvBeoordeling.analyseresultaten", "analyseresultaten", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstel", "uitstel", JoinType.LEFT_OUTER_JOIN);
	}

}
