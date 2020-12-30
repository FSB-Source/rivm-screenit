package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixSelectieRestrictionsServiceImpl implements CervixSelectieRestrictionsService
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public void addClientSelectieRestrictions(Criteria criteria)
	{
		criteria.createAlias("dossier.laatsteScreeningRonde", "ronde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.monsterHpvUitslag", "monsterHpvUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("monsterHpvUitslag.laatsteHpvBeoordeling", "laatsteHpvBeoordeling", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeCytologieUitslag", "uitstrijkjeCytologieUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeCytologieUitslag.cytologieVerslag", "cytologieVerslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeVervolgonderzoekUitslag", "uitstrijkjeVervolgonderzoekUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeVervolgonderzoekUitslag.cytologieVerslag", "vervolgonderzoekVerslag", JoinType.LEFT_OUTER_JOIN);

		LocalDate vandaag = dateSupplier.getLocalDate();

		LocalDate geboortedatumMinimaal = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
		LocalDate geboortedatumMinimaalVervolgonderzoekNegatief = vandaag.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());

		String startdatumBMHKString = preferenceService.getString(PreferenceKey.STARTDATUM_BMHK.name());
		LocalDate startdatumBMHK = LocalDate.parse(startdatumBMHKString, DateTimeFormatter.ofPattern("yyyyMMdd"));

		long daysBetween = ChronoUnit.DAYS.between(startdatumBMHK, vandaag);
		LocalDate geboortedatumMinimaal30 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		LocalDate geboortedatumMinimaal35 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._35.getLeeftijd());
		LocalDate geboortedatumMinimaal40 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._40.getLeeftijd());
		LocalDate geboortedatumMinimaal45 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._45.getLeeftijd());
		LocalDate geboortedatumMinimaal50 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._50.getLeeftijd());
		LocalDate geboortedatumMinimaal55 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._55.getLeeftijd());
		LocalDate geboortedatumMinimaal60 = startdatumBMHK.minusYears(CervixLeeftijdcategorie._60.getLeeftijd());

		LocalDate geboortedatumMaximaal30 = geboortedatumMinimaal30.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal35 = geboortedatumMinimaal35.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal40 = geboortedatumMinimaal40.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal45 = geboortedatumMinimaal45.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal50 = geboortedatumMinimaal50.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal55 = geboortedatumMinimaal55.plusDays(daysBetween);
		LocalDate geboortedatumMaximaal60 = geboortedatumMinimaal60.plusDays(daysBetween);

		Disjunction disjunction = Restrictions.disjunction();
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal30), DateUtil.toUtilDate(geboortedatumMaximaal30)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal35), DateUtil.toUtilDate(geboortedatumMaximaal35)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal40), DateUtil.toUtilDate(geboortedatumMaximaal40)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal45), DateUtil.toUtilDate(geboortedatumMaximaal45)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal50), DateUtil.toUtilDate(geboortedatumMaximaal50)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal55), DateUtil.toUtilDate(geboortedatumMaximaal55)));
		disjunction.add(Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal60), DateUtil.toUtilDate(geboortedatumMaximaal60)));

		LocalDate geboortedatumMaximaal = vandaag.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		criteria.add(Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMaximaal)));
		criteria.add(Restrictions.or(Restrictions.and(Restrictions.isNull("dossier.volgendeRondeVanaf"), disjunction),
			Restrictions.le("dossier.volgendeRondeVanaf", DateUtil.toUtilDate(vandaag))));

		criteria.add(Restrictions.or(
			Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal)),
			Restrictions.and(
				Restrictions.eq("ronde.leeftijdcategorie", CervixLeeftijdcategorie._60),
				Restrictions.eq("laatsteHpvBeoordeling.hpvUitslag", CervixHpvUitslag.POSITIEF),
				Restrictions.or(
					Restrictions.isNull("cytologieVerslag.cytologieUitslag"),
					Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1)),
				Restrictions.or(
					Restrictions.isNull("vervolgonderzoekVerslag.cytologieUitslag"),
					Restrictions.eq("vervolgonderzoekVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1)),
				Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaalVervolgonderzoekNegatief)))));
	}

}
