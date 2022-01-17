package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportageProjectEntry;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class CervixUitnodigingVersturenLogEvent extends LogEvent
{
	@OneToMany
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@Cascade(CascadeType.DELETE)
	private List<CervixZasVersturenRapportage> rapportage = new ArrayList<>();

	@OneToMany
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@Cascade(CascadeType.DELETE)
	private List<CervixZasVersturenRapportageProjectEntry> projectGroepen = new ArrayList<>();

	public List<CervixZasVersturenRapportage> getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(List<CervixZasVersturenRapportage> rapportage)
	{
		this.rapportage = rapportage;
	}

	public List<CervixZasVersturenRapportageProjectEntry> getProjectGroepen()
	{
		return projectGroepen;
	}

	public void setProjectGroepen(List<CervixZasVersturenRapportageProjectEntry> projectGroepen)
	{
		this.projectGroepen = projectGroepen;
	}
}
