package nl.rivm.screenit.dao.impl;

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

import java.util.Date;
import java.util.List;

import javax.annotation.Nonnull;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class InstellingDaoImpl extends AbstractAutowiredDao implements InstellingDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Instelling getInstellingBy(String key, String value)
	{
		Criteria crit = getSession().createCriteria(Instelling.class);
		crit.add(Restrictions.eq(key, value));
		crit.add(Restrictions.eq("actief", true));
		return (Instelling) crit.uniqueResult();
	}

	@Override
	public List<Instelling> getInstellingByOrganisatieTypes(List<OrganisatieType> organisatieTypes)
	{
		Criteria crit = getSession().createCriteria(Instelling.class);
		crit.add(Restrictions.in("organisatieType", organisatieTypes));
		crit.add(Restrictions.eq("actief", true));
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public List<InstellingGebruiker> getActieveInstellingGebruikers(Gebruiker medewerker)
	{
		Criteria crit = criteriaActieveInstellingGebruikers(medewerker);
		return crit.list();
	}

	private Criteria criteriaActieveInstellingGebruikers(Gebruiker medewerker)
	{
		Criteria crit = getSession().createCriteria(InstellingGebruiker.class);
		crit.add(Restrictions.eq("actief", true));
		crit.createAlias("organisatie", "organisatie");
		crit.add(Restrictions.eq("organisatie.actief", true));
		crit.add(Restrictions.eq("medewerker", medewerker));
		return crit;
	}

	@Override
	public List<InstellingGebruiker> getInstellingGebruikersVoorInloggen(Gebruiker medewerker)
	{
		Criteria crit = criteriaActieveInstellingGebruikers(medewerker);
		crit.createAlias("rollen", "igrol", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("igrol.rol", "rol", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.or(
			Restrictions.eq("organisatie.organisatieType", OrganisatieType.HUISARTS), 
			getActieveRolCriterion("igrol", "rol")));
		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		crit.addOrder(Order.asc("organisatie.naam"));
		return crit.list();
	}

	@Override
	public Criterion getActieveRolCriterion(String igRolAlias, String rolAlias)
	{
		Date nu = currentDateSupplier.getDate();
		Date gisteren = DateUtil.minDagen(currentDateSupplier.getDate(), 1);

		return Restrictions.and(
			NvlRestrictions.le(igRolAlias + ".beginDatum", nu, Constants.BEGIN_OF_TIME),
			NvlRestrictions.ge(igRolAlias + ".eindDatum", gisteren, Constants.END_OF_TIME),
			Restrictions.eq(igRolAlias + ".actief", true),
			Restrictions.eq(rolAlias + ".actief", true));
	}

	@Override
	public <T extends Instelling> List<T> getActieveInstellingen(Class<T> clazz)
	{
		Criteria crit = getSession().createCriteria(clazz);
		crit.add(Restrictions.eq("actief", true));
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public <T extends Instelling> List<T> getActieveInstellingenBinnenRegio(Class<T> typeInstelling, ScreeningOrganisatie regio)
	{
		Criteria crit = getSession().createCriteria(typeInstelling);
		crit.add(Restrictions.eq("actief", true));
		crit.add(Restrictions.eq("regio", regio));
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		Criteria crit = getSession().createCriteria(BeoordelingsEenheid.class);
		crit.add(Restrictions.eq("actief", true));
		crit.createAlias("parent", "ce");
		crit.add(Restrictions.eq("ce.regio", regio));
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public List<Instelling> getPathologieLabs(Instelling instelling)
	{
		Criteria crit = getSession().createCriteria(PaLaboratorium.class);
		crit.add(Restrictions.eq("actief", true));
		if (instelling instanceof ColoscopieLocatie)
		{
			crit.createAlias("coloscopielocaties", "coloscopielocatie");
			crit.add(Restrictions.eq("coloscopielocatie.id", instelling.getId()));
		}
		else
		{
			crit.createAlias("coloscopielocaties", "coloscopielocatie");
			crit.add(Restrictions.eq("coloscopielocatie.parent", instelling));
		}
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public <T extends Instelling> List<T> getChildrenInstellingen(@Nonnull Instelling instelling, @Nonnull Class<T> typeInstelling)
	{
		Criteria crit = getSession().createCriteria(typeInstelling);
		crit.add(Restrictions.eq("actief", true));
		crit.add(Restrictions.eq("parent", instelling));
		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public IFobtLaboratorium getIfobtLabByLabID(String labIdScanner)
	{
		Criteria crit = getSession().createCriteria(IFobtLaboratorium.class);
		crit.add(Restrictions.eq("labIdScanner", labIdScanner));
		return (IFobtLaboratorium) crit.uniqueResult();
	}

	@Override
	public List<Long> getLandelijkeInstellingIds(OrganisatieType type)
	{
		Criteria crit = getSession().createCriteria(Instelling.class);
		crit.add(Restrictions.eq("actief", true));
		crit.add(Restrictions.eq("organisatieType", type));
		crit.setProjection(Projections.property("id"));
		return crit.list();
	}

	@Override
	public Criteria getAllILAdressenZonderCoordinanten()
	{
		Criteria crit = getSession().createCriteria(ColonIntakelocatie.class);
		crit.add(Restrictions.isNull("postcodeCoordinaten"));
		return crit;
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(String regioCode)
	{
		Criteria crit = getSession().createCriteria(ScreeningOrganisatie.class);
		crit.add(Restrictions.eq("regioCode", regioCode));
		crit.add(Restrictions.eq("actief", true));
		return (ScreeningOrganisatie) crit.uniqueResult();
	}
}
