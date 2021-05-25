
package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class SelectieRondeBeeindigdLogEvent extends LogEvent
{
	private boolean resultaat;

	@ElementCollection
	@Column(length = HibernateMagicNumber.L4000)
	@CollectionTable(schema = "gedeeld", name = "selectie_ronde_beeindigd_log_event_exception_stack_trace")
	private List<String> exceptionStackTrace = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade(CascadeType.DELETE)
	private SelectieRapportage rapportage;

	public boolean isResultaat()
	{
		return resultaat;
	}

	public void setResultaat(boolean resultaat)
	{
		this.resultaat = resultaat;
	}

	public List<String> getExceptionStackTrace()
	{
		return exceptionStackTrace;
	}

	public void setExceptionStackTrace(List<String> exceptionStackTrace)
	{
		this.exceptionStackTrace = exceptionStackTrace;
	}

	public SelectieRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(SelectieRapportage rapportage)
	{
		this.rapportage = rapportage;
	}
}
