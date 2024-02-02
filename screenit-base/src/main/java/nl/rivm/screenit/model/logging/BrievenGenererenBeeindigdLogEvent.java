package nl.rivm.screenit.model.logging;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportage;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class BrievenGenererenBeeindigdLogEvent extends LogEvent
{
	@OneToOne(cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private BrievenGenererenRapportage rapportage;

	@ElementCollection
	@Column(length = HibernateMagicNumber.L4000)
	@CollectionTable(schema = "gedeeld", name = "brieven_genereren_beeindigd_log_event_exception_stack_trace")
	private List<String> exceptionStackTrace = new ArrayList<>();

	private boolean resultaat;

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

	public BrievenGenererenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(BrievenGenererenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}
}
