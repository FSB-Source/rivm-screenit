package nl.rivm.screenit.main.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;

public class Parameterisatie implements IDetachable, Cloneable
{

	private static final long serialVersionUID = 1L;

	private SimpleListHibernateModel<UitnodigingCohort> cohorten;

	private Map<PreferenceKey, Object> parameters;

	public List<UitnodigingCohort> getCohorten()
	{
		return ModelUtil.nullSafeGet(cohorten);
	}

	public void setCohorten(List<UitnodigingCohort> cohorten)
	{
		if (cohorten == null)
		{
			this.cohorten = null;
		}
		else
		{
			this.cohorten = new SimpleListHibernateModel<>(cohorten);
		}
	}

	public void addCohort(UitnodigingCohort cohort)
	{
		if (this.cohorten == null)
		{
			this.cohorten = new SimpleListHibernateModel<>(new ArrayList<UitnodigingCohort>());
		}

		this.cohorten.add(cohort);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(cohorten);
	}

	public Map<PreferenceKey, Object> getParameters()
	{
		return parameters;
	}

	public void setParameters(Map<PreferenceKey, Object> parameters)
	{
		this.parameters = parameters;
	}

	@Override
	public Parameterisatie clone()
	{
		Parameterisatie cloned = new Parameterisatie();
		cloned.setParameters(new HashMap<>(this.getParameters()));
		cloned.setCohorten(new ArrayList<>(this.getCohorten()));
		return cloned;
	}
}
