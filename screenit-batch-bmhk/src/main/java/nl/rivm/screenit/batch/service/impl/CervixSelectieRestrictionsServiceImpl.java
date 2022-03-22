package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.CervixSelectieRestrictionsService;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
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
		addClientSelectieRestrictions(criteria, 0);
	}

	@Override
	public void addClientSelectieRestrictions(Criteria criteria, int daysToAddToToday)
	{
		criteria.createAlias("dossier.laatsteScreeningRonde", "ronde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.monsterHpvUitslag", "monsterHpvUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("monsterHpvUitslag.laatsteHpvBeoordeling", "laatsteHpvBeoordeling", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeCytologieUitslag", "uitstrijkjeCytologieUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeCytologieUitslag.cytologieVerslag", "cytologieVerslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("ronde.uitstrijkjeVervolgonderzoekUitslag", "uitstrijkjeVervolgonderzoekUitslag", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("uitstrijkjeVervolgonderzoekUitslag.cytologieVerslag", "vervolgonderzoekVerslag", JoinType.LEFT_OUTER_JOIN);

		LocalDate vandaag = dateSupplier.getLocalDate();
		if (daysToAddToToday > 0)
		{
			vandaag = vandaag.minusDays(daysToAddToToday);
		}

		LocalDate geboortedatumMinimaal = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
		LocalDate geboortedatumMinimaalVervolgonderzoekNegatief = vandaag.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());

		Integer dagenVoorDeVooraankondiging = preferenceService.getInteger(PreferenceKey.CERVIX_VOORAANKONDIGINGS_PERIODE.name());
		LocalDate exactDertigJaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		LocalDate exact35JaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._35.getLeeftijd());
		LocalDate dertigJaarGeledenPlusVooraankondigingsDagen =
			dagenVoorDeVooraankondiging != null ? exactDertigJaarGeleden.plusDays(dagenVoorDeVooraankondiging) : exactDertigJaarGeleden;

		criteria.add(
			Restrictions.or(
				Restrictions.and( 
					Restrictions.isNull("dossier.vooraankondigingsBrief"),
					Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(exactDertigJaarGeleden)),
					Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(dertigJaarGeledenPlusVooraankondigingsDagen))),
				Restrictions.and( 
					Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(exactDertigJaarGeleden)),
					Restrictions.or(
						Restrictions.and(
							Restrictions.isNull("dossier.volgendeRondeVanaf"),
							Restrictions.or(
								Restrictions.isNull("dossier.laatsteScreeningRonde"),
								Restrictions.eq("ronde.aangemeld", true),
								Restrictions.le("persoon.geboortedatum", DateUtil.toUtilDate(exact35JaarGeleden)))),
						Restrictions.le("dossier.volgendeRondeVanaf", DateUtil.toUtilDate(vandaag))))));

		criteria.add(Restrictions.or(
			Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaal)),
			Restrictions.and(
				Restrictions.eq("ronde.leeftijdcategorie", CervixLeeftijdcategorie._60),
				Restrictions.eq("laatsteHpvBeoordeling.hpvUitslag", CervixHpvBeoordelingWaarde.POSITIEF),
				Restrictions.or(
					Restrictions.isNull("cytologieVerslag.cytologieUitslag"),
					Restrictions.eq("cytologieVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1)),
				Restrictions.or(
					Restrictions.isNull("vervolgonderzoekVerslag.cytologieUitslag"),
					Restrictions.eq("vervolgonderzoekVerslag.cytologieUitslag", CervixCytologieUitslag.PAP1)),
				Restrictions.gt("persoon.geboortedatum", DateUtil.toUtilDate(geboortedatumMinimaalVervolgonderzoekNegatief)))));
	}

}
