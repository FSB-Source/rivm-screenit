package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.List;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import org.apache.commons.lang.StringUtils;
import org.hibernate.query.NativeQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import static nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie.GEKOPPELD;
import static nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie.TEHUIS_ADRES;
import static nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie.UIT_TE_NODIGEN;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseTehuisClientenDaoImpl extends AbstractAutowiredDao implements MammaBaseTehuisClientenDao
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseTehuisDao tehuisDao;

	private NativeQuery createQuery(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres, boolean count, String sortProperty, Boolean isAscending)
	{
		LocalDate vandaag = dateSupplier.getLocalDate();

		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		Integer minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());

		MammaStandplaatsRonde huidigeStandplaatsRonde = tehuisDao.getHuidigeStandplaatsRonde(tehuis.getStandplaats(), false);
		LocalDate standplaatsRondeVanaf = null;
		LocalDate standplaatsRondeTotEnMet = null;
		for (MammaStandplaatsPeriode standplaatsPeriode : huidigeStandplaatsRonde.getStandplaatsPerioden())
		{
			LocalDate standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
			LocalDate standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());
			if (standplaatsRondeVanaf == null || standplaatsPeriodeVanaf.isBefore(standplaatsRondeVanaf))
			{
				standplaatsRondeVanaf = standplaatsPeriodeVanaf;
			}
			if (standplaatsRondeTotEnMet == null || standplaatsPeriodeTotEnMet.isAfter(standplaatsRondeTotEnMet))
			{
				standplaatsRondeTotEnMet = standplaatsPeriodeTotEnMet;
			}
		}

		int vanafGeboortejaar = Integer.min(standplaatsRondeVanaf.getYear(), vandaag.getYear()) - maximaleLeeftijd;
		int totGeboortejaar = standplaatsRondeTotEnMet.getYear() - minimaleLeeftijd + 1;

		String selectString = count ? "select count(*)" : "select client.*";

		String fromString = "";
		fromString += " from gedeeld.pat_persoon persoon";
		fromString += " join gedeeld.org_adres adres on (persoon.gba_adres = adres.id or persoon.tijdelijk_gba_adres = adres.id)";
		fromString += " join gedeeld.org_adres gba_adres on persoon.gba_adres = gba_adres.id";
		fromString += " join gedeeld.pat_patient client on persoon.patient = client.id";
		fromString += " left join gedeeld.gba_mutatie laatste_gba_mutatie on client.laatste_gba_mutatie = laatste_gba_mutatie.id";
		fromString += " join mamma.dossier dossier on client.mamma_dossier = dossier.id";
		fromString += " left join mamma.screening_ronde laatste_screening_ronde on dossier.laatste_screening_ronde = laatste_screening_ronde.id";

		StringBuilder whereString = new StringBuilder();
		whereString.append(" where persoon.geslacht = \'").append(Geslacht.VROUW.name()).append("\'");
		whereString.append(" and persoon.geboortedatum >= :vanafGeboortedatum");
		whereString.append(" and persoon.geboortedatum < :totGeboortedatum");

		if (tehuisSelectie == UIT_TE_NODIGEN && vandaag.isBefore(standplaatsRondeVanaf.minusMonths(2))
				|| tehuis.getAdressen().isEmpty())
		{

			whereString.append(" and client.id is null");
		}
		else
		{
			whereString.append(" and (");
			for (int i = 0; i < tehuis.getAdressen().size(); i++)
			{
				if (i > 0)
				{
					whereString.append(" or");
				}

				Adres tehuisAdres = tehuis.getAdressen().get(i);

				whereString.append(" adres.postcode = upper(\'").append(tehuisAdres.getPostcode()).append("\')");
				whereString.append(" and adres.huisnummer = ").append(tehuisAdres.getHuisnummer());
				if (StringUtils.isNotBlank(tehuisAdres.getHuisletter()))
				{
					whereString.append(" and lower(adres.huisletter) = lower(\'").append(tehuisAdres.getHuisletter()).append("\')");
				}
				else
				{
					whereString.append(" and adres.huisletter is null");
				}
				if (StringUtils.isNotBlank(tehuisAdres.getHuisnummerToevoeging()))
				{
					whereString.append(" and lower(adres.huisnummer_toevoeging) = lower(\'").append(tehuisAdres.getHuisnummerToevoeging()).append("\')");
				}
				else
				{
					whereString.append(" and adres.huisnummer_toevoeging is null");
				}
			}
			whereString.append(")");

			if (zoekAdres != null)
			{
				if (StringUtils.isNotBlank(zoekAdres.getPostcode()))
				{
					whereString.append(" and adres.postcode = upper(\'").append(zoekAdres.getPostcode()).append("\')");
				}
				if (zoekAdres.getHuisnummer() != null)
				{
					whereString.append(" and adres.huisnummer = ").append(zoekAdres.getHuisnummer());
				}
				if (StringUtils.isNotBlank(zoekAdres.getHuisletter()))
				{
					whereString.append(" and lower(adres.huisletter) = lower(\'").append(zoekAdres.getHuisletter()).append("\')");
				}
				if (StringUtils.isNotBlank(zoekAdres.getHuisnummerToevoeging()))
				{
					whereString.append(" and lower(adres.huisnummer_toevoeging) = lower(\'").append(zoekAdres.getHuisnummerToevoeging()).append("\')");
				}
			}
		}

		if (tehuisSelectie == TEHUIS_ADRES || tehuisSelectie == GEKOPPELD)
		{
			whereString.append(" and ").append(ScreenitRestrictions.getPersoonBaseRestrictions("persoon"));
		}
		if (tehuisSelectie == GEKOPPELD || tehuisSelectie == UIT_TE_NODIGEN)
		{
			whereString.append(" and dossier.tehuis = :tehuis");
		}
		if (tehuisSelectie == UIT_TE_NODIGEN)
		{
			whereString.append(" and ").append(ScreenitRestrictions.getClientBaseRestrictions("client", "persoon"));

			whereString.append(" and dossier.status = \'").append(DossierStatus.ACTIEF).append("\'");

			whereString.append(" and (");
			whereString.append(" dossier.laatste_mammografie_afgerond is null");
			whereString.append(" or dossier.laatste_mammografie_afgerond < :laatsteMammografieAfgerondTot");
			whereString.append(")");

			whereString.append(" and (");
			whereString.append(" laatste_screening_ronde.id is null");
			whereString.append(" or laatste_screening_ronde.standplaats_ronde != :huidigeStandplaatsRonde");
			whereString.append(" and laatste_screening_ronde.creatie_datum < :laatsteScreeningRondeCreatieDatumTot");
			whereString.append(")");

			fromString += " left join mamma.uitnodiging laatste_uitnodiging on laatste_screening_ronde.laatste_uitnodiging = laatste_uitnodiging.id";
			fromString += " left join mamma.afspraak laatste_afspraak on laatste_uitnodiging.laatste_afspraak = laatste_afspraak.id";
			fromString += " left join mamma.standplaats_periode standplaats_periode on laatste_afspraak.standplaats_periode = standplaats_periode.id";
			whereString.append("and (laatste_afspraak.id is null");
			whereString.append(" or laatste_afspraak.afgezegd_op is not null");
			whereString.append(" or standplaats_periode.tot_en_met < :standplaatsPeriodeTotEnMetTot)");
		}

		String orderString = "";

		if (sortProperty != null)
		{
			orderString += " order by " + sortProperty;
			orderString += isAscending ? " asc" : " desc";
		}

		NativeQuery query = getSession().createNativeQuery(selectString + fromString + whereString + orderString);

		query.setParameter("vanafGeboortedatum", DateUtil.toUtilDate(LocalDate.of(vanafGeboortejaar, 1, 1)));
		query.setParameter("totGeboortedatum", DateUtil.toUtilDate(LocalDate.of(totGeboortejaar, 1, 1)));
		if (tehuisSelectie == GEKOPPELD || tehuisSelectie == UIT_TE_NODIGEN)
		{
			query.setParameter("tehuis", tehuis.getId());
		}
		if (tehuisSelectie == UIT_TE_NODIGEN)
		{
			Integer minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
			Integer minimaleIntervalUitnodigingen = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());

			query.setParameter("huidigeStandplaatsRonde", huidigeStandplaatsRonde.getId());
			query.setParameter("laatsteMammografieAfgerondTot", DateUtil.toUtilDate(vandaag.minusDays(minimaleIntervalMammografieOnderzoeken)));
			query.setParameter("laatsteScreeningRondeCreatieDatumTot", DateUtil.toUtilDate(vandaag.minusDays(minimaleIntervalUitnodigingen)));
			query.setParameter("standplaatsPeriodeTotEnMetTot", DateUtil.toUtilDate(vandaag));
		}

		return query;
	}

	@Override
	public long countClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres)
	{
		NativeQuery<BigInteger> query = createQuery(tehuis, tehuisSelectie, zoekAdres, true, null, null);
		return query.getSingleResult().longValue();
	}

	@Override
	public List<Client> getClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres)
	{
		NativeQuery query = createQuery(tehuis, tehuisSelectie, zoekAdres, false, null, null);
		query.addEntity(Client.class);
		return query.list();
	}

	@Override
	public List<Client> getClienten(MammaTehuis tehuis, MammaTehuisSelectie tehuisSelectie, Adres zoekAdres, int first, int count, String sortProperty, boolean isAscending)
	{
		NativeQuery query = createQuery(tehuis, tehuisSelectie, zoekAdres, false, sortProperty, isAscending);
		query.addEntity(Client.class);

		query.setFirstResult(Math.max(first, 0));
		if (count < 0)
		{
			query.setMaxResults(Integer.MAX_VALUE);
		}
		else
		{
			query.setMaxResults(count);
		}
		return query.list();
	}
}
