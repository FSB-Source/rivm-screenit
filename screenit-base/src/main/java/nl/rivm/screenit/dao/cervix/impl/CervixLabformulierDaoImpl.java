package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.cervix.CervixLabformulierDao;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.FetchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CONTROLEREN_VOOR_CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.HUISARTS_ONBEKEND;

@Repository
public class CervixLabformulierDaoImpl extends AbstractAutowiredDao implements CervixLabformulierDao
{

	@Override
	public List<CervixLabformulier> getLabformulieren(CervixLabformulierenFilter filter, long first, long count, String sortProperty, boolean asc)
	{
		return createLabformulierenBaseCriteria(filter, false, sortProperty).list(getSession(),
			new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, asc));
	}

	@Override
	public int countLabformulieren(CervixLabformulierenFilter filter)
	{
		return createLabformulierenBaseCriteria(filter, true, null).count(getSession());
	}

	@Override
	public List<Long> getLabformulierenIds(CervixLabformulierenFilter filter, String sortProperty, boolean asc)
	{
		BaseCriteria labformulierenBaseCriteria = createLabformulierenBaseCriteria(filter, true, sortProperty);
		labformulierenBaseCriteria.addOrder(asc ? Order.asc(sortProperty) : Order.desc(sortProperty));
		labformulierenBaseCriteria.setProjection(Projections.id());
		return labformulierenBaseCriteria.list(getSession());
	}

	private BaseCriteria createLabformulierenBaseCriteria(CervixLabformulierenFilter filter, boolean usedForCountOrIdList, String sortProperty)
	{
		BaseCriteria baseCriteria = new BaseCriteria(CervixLabformulier.class, "labformulier");

		if (filter.getLabprocesStap() == null)
		{
			throw new IllegalStateException();
		}

		switch (filter.getOrganisatieType())
		{
		case BMHK_LABORATORIUM:
			baseCriteria.add(Restrictions.eq("labformulier.laboratorium.id", filter.getInstellingId()));
			break;
		case SCREENINGSORGANISATIE:
			baseCriteria.add(Restrictions.eq("gemeente.screeningOrganisatie.id", filter.getInstellingId()));
			break;
		case RIVM:
			break;
		default:
			throw new IllegalStateException();
		}

		if (filter.getMonsterId() != null)
		{
			baseCriteria.add(Restrictions.eq("labformulier.barcode", filter.getMonsterId()));
		}

		if (!filter.getLabformulierStatussen().isEmpty())
		{
			baseCriteria.add(Restrictions.in("labformulier.status", filter.getLabformulierStatussen()));
		}
		else
		{

			baseCriteria.add(Restrictions.sqlRestriction("(1=0)"));
		}

		if (filter.getScanDatumVanaf() != null)
		{
			baseCriteria.add(Restrictions.ge("labformulier.scanDatum", filter.getScanDatumVanaf()));
		}

		if (filter.getScanDatumTotEnMet() != null)
		{
			baseCriteria.add(Restrictions.lt("labformulier.scanDatum", DateUtil.plusDagen(DateUtil.startDag(filter.getScanDatumTotEnMet()), 1)));
		}

		if (filter.getGeboortedatum() != null)
		{
			baseCriteria.add(Restrictions.eq("persoon.geboortedatum", filter.getGeboortedatum()));
		}

		JoinType joinType = null;
		if (filter.getLabprocesStap() == HUISARTS_ONBEKEND ||
			filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE ||
			filter.getBsn() != null)
		{
			joinType = JoinType.INNER_JOIN;
		}
		else
		{
			joinType = JoinType.LEFT_OUTER_JOIN;
		}

		baseCriteria.createAlias("labformulier.uitstrijkje", "uitstrijkje", joinType);

		baseCriteria.createAlias("uitstrijkje.huisartsBericht", "huisartsBericht", JoinType.LEFT_OUTER_JOIN);
		baseCriteria.add(Restrictions.or(Restrictions.isNull("huisartsBericht.status"), Restrictions.ne("huisartsBericht.status", CervixHuisartsBerichtStatus.VERSTUURD)));
		baseCriteria.add(Restrictions.or(Restrictions.isNull("huisartsBericht.status"), Restrictions.ne("huisartsBericht.status", CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND),
			Restrictions.eq("labformulier.status", CervixLabformulierStatus.AFGEKEURD)));

		if (filter.getLabprocesStap() == HUISARTS_ONBEKEND ||
			filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE)
		{
			baseCriteria.createAlias("uitstrijkje.ontvangstScreeningRonde", "ontvangstScreeningRonde");
			baseCriteria.createAlias("ontvangstScreeningRonde.monsterHpvUitslag", "monster");
			baseCriteria.createAlias("monster.laatsteHpvBeoordeling", "hpvBeoordeling");

			baseCriteria.add(Restrictions.eq("hpvBeoordeling.hpvUitslag", CervixHpvBeoordelingWaarde.POSITIEF));
			baseCriteria.add(Restrictions.in("uitstrijkje.uitstrijkjeStatus", List.of(CervixUitstrijkjeStatus.ONTVANGEN,
				CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1, CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2)));
			baseCriteria.or(Restrictions.isNull("ontvangstScreeningRonde.uitstrijkjeCytologieUitslag"),
				Restrictions.and(Restrictions.isNotNull("ontvangstScreeningRonde.inVervolgonderzoekDatum"),
					Restrictions.geProperty("uitstrijkje.ontvangstdatum", "ontvangstScreeningRonde.inVervolgonderzoekDatum"),
					Restrictions.isNull("ontvangstScreeningRonde.uitstrijkjeVervolgonderzoekUitslag")));
		}

		if (filter.getLabprocesStap() == HUISARTS_ONBEKEND)
		{
			baseCriteria.createAlias("labformulier.huisartsOnbekendBrief", "huisartsOnbekendBrief");
			baseCriteria.createAlias("huisartsOnbekendBrief.mergedBrieven", "mergedBrieven");
			baseCriteria.eq("mergedBrieven.geprint", true);
		}

		if (filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE)
		{
			baseCriteria.add(Restrictions.isNull("uitstrijkje.cytologieOrder"));
			baseCriteria.add(Restrictions.isNull("huisartsBericht.id"));
		}

		if (filter.getLabprocesStap() == CYTOLOGIE)
		{
			baseCriteria.createAlias("uitstrijkje.cytologieOrder", "cytologieOrder");
			baseCriteria.add(Restrictions.eq("cytologieOrder.status", CervixCytologieOrderStatus.VERSTUURD));
		}

		if (!usedForCountOrIdList ||
			filter.getBsn() != null ||
			filter.getGeboortedatum() != null ||
			(sortProperty != null && sortProperty.contains("persoon")) ||
			filter.getOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE)
		{
			baseCriteria.createAlias("uitstrijkje.uitnodiging", "uitnodiging", joinType);
			baseCriteria.createAlias("uitnodiging.screeningRonde", "uitnodigingScreeningRonde", joinType);
			baseCriteria.createAlias("uitnodigingScreeningRonde.dossier", "dossier", joinType);
			baseCriteria.createAlias("dossier.client", "client", joinType);
			baseCriteria.createAlias("client.persoon", "persoon", joinType);
			if (filter.getBsn() != null)
			{
				baseCriteria.add(Restrictions.eq("persoon.bsn", filter.getBsn()));
			}
		}

		if (filter.getOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE)
		{
			baseCriteria.createAlias("persoon.gbaAdres", "adres");
			baseCriteria.createAlias("adres.gbaGemeente", "gemeente");
		}

		if (filter.getDigitaal() != null)
		{
			baseCriteria.add(Restrictions.eq("digitaal", filter.getDigitaal()));
		}

		baseCriteria.setFetchMode("huisartsLocatie", FetchMode.JOIN);
		return baseCriteria;
	}
}
