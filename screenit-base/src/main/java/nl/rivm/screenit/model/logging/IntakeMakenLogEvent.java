
package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.IntakeMakenLogEventRegel;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld")
public class IntakeMakenLogEvent extends LogEvent
{
	@OneToMany(mappedBy = "logEvent", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<IntakeMakenLogEventRegel> regels = new ArrayList<>();

	@Column(length = HibernateMagicNumber.L1024)
	private String plannerResultaat;

	@ElementCollection
	@Column(length = HibernateMagicNumber.L4000)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@CollectionTable(schema = "gedeeld", name = "intake_maken_log_event_exception_stack_trace")
	private List<String> exceptionStackTrace = new ArrayList<>();

	private String aantalVrijesloten;

	private String aantalClienten;

	private Integer aantalExtraDagen = 0;

	private Integer aantalRondes = 0;

	private Integer aantalBuitenMaximaleAfstand = 0;

	private Date beginTijd;

	private Date eindTijd;

	public List<String> getExceptionStackTrace()
	{
		return exceptionStackTrace;
	}

	public void setExceptionStackTrace(List<String> exceptionStackTrace)
	{
		this.exceptionStackTrace = exceptionStackTrace;
	}

	public List<IntakeMakenLogEventRegel> getRegels()
	{
		return regels;
	}

	public void setRegels(List<IntakeMakenLogEventRegel> regels)
	{
		this.regels = regels;
	}

	public String getPlannerResultaat()
	{
		return plannerResultaat;
	}

	public void setPlannerResultaat(String plannerResultaat)
	{
		this.plannerResultaat = plannerResultaat;
	}

	public String getAantalVrijesloten()
	{
		return aantalVrijesloten;
	}

	public void setAantalVrijesloten(String aantalVrijesloten)
	{
		this.aantalVrijesloten = aantalVrijesloten;
	}

	public String getAantalClienten()
	{
		return aantalClienten;
	}

	public void setAantalClienten(String aantalClienten)
	{
		this.aantalClienten = aantalClienten;
	}

	public Integer getAantalExtraDagen()
	{
		return aantalExtraDagen;
	}

	public void setAantalExtraDagen(Integer aantalExtraDagen)
	{
		this.aantalExtraDagen = aantalExtraDagen;
	}

	public Integer getAantalRondes()
	{
		return aantalRondes;
	}

	public void setAantalRondes(Integer aantalRondes)
	{
		this.aantalRondes = aantalRondes;
	}

	public Integer getAantalBuitenMaximaleAfstand()
	{
		return aantalBuitenMaximaleAfstand;
	}

	public void setAantalBuitenMaximaleAfstand(Integer aantalBuitenMaximaleAfstand)
	{
		this.aantalBuitenMaximaleAfstand = aantalBuitenMaximaleAfstand;
	}

	public Date getBeginTijd()
	{
		return beginTijd;
	}

	public void setBeginTijd(Date beginTijd)
	{
		this.beginTijd = beginTijd;
	}

	public Date getEindTijd()
	{
		return eindTijd;
	}

	public void setEindTijd(Date eindTijd)
	{
		this.eindTijd = eindTijd;
	}
}
