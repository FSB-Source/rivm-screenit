package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dao.VragenlijstDao;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoordenHolder;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.stereotype.Repository;

@Repository
public class VragenlijstDaoImpl extends AbstractAutowiredDao implements VragenlijstDao
{

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<ScreenitFormulierInstantie> getAlleFormulierenMetResultatenEnHuidige(Vragenlijst vragenlijst)
	{
		List<ScreenitFormulierInstantie> lijst = getAlleFormulierInstantiesMetResultatenEnHuidige(vragenlijst);
		Collections.sort(lijst, new PropertyComparator("creatieDatum", false, true));
		return lijst.iterator();
	}

	private List<ScreenitFormulierInstantie> getAlleFormulierInstantiesMetResultatenEnHuidige(Vragenlijst vragenlijst)
	{
		Criteria crit = getAlleFormulierenMetResultatenCriteria(vragenlijst);
		List<ScreenitFormulierInstantie> lijst = new ArrayList<>(crit.list());
		ScreenitFormulierInstantie definitie = vragenlijst.getFormulierInstantie();
		if (definitie != null)
		{
			if (!lijst.contains(definitie))
			{
				lijst.add((ScreenitFormulierInstantie) getSession().load(ScreenitFormulierInstantie.class, definitie.getId()));
			}
		}
		return lijst;
	}

	private Criteria getAlleFormulierenMetResultatenCriteria(Vragenlijst vragenlijst)
	{
		Criteria crit = getSession().createCriteria(VragenlijstAntwoorden.class);
		crit.createAlias("antwoordenHolder", "antwoordenHolder");

		crit.add(Restrictions.eq("antwoordenHolder.vragenlijst", vragenlijst));

		crit.setProjection(Projections.distinct(Projections.property("formulierInstantie")));

		return crit;
	}

	@Override
	public long countAlleFormulierenMetResultatenEnHuidige(Vragenlijst vragenlijst)
	{
		return getAlleFormulierInstantiesMetResultatenEnHuidige(vragenlijst).size();
	}

	@Override
	public boolean isVragenlijstGekoppeldAanHolder(Long vragenlijstId, Class<? extends VragenlijstAntwoordenHolder> holderType)
	{
		Criteria criteria = getSession().createCriteria(holderType);
		criteria.createAlias("vragenlijst", "vragenlijst");

		criteria.add(Restrictions.eq("vragenlijst.id", vragenlijstId));
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue() > 0;
	}

	@Override
	public List<ProjectBrief> getAllProjectBrievenWithFormulier(ScreenitFormulierInstantie oldFormulierInstantie)
	{
		Criteria criteria = getSession().createCriteria(ProjectBrief.class);
		criteria.createAlias("vragenlijstAntwoordenHolder", "holder");
		criteria.createAlias("holder.vragenlijstAntwoorden", "vragenlijstAntwoorden");

		criteria.add(Restrictions.eq("vragenlijstAntwoorden.formulierInstantie", oldFormulierInstantie));

		return criteria.list();
	}

}
