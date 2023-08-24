package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlIncidentcomplicatie
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerrichting verrichting;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_typeincident", values = {
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.10"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.10"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Type incident/complicatie", extraTekst = "Registratie plaatsgevonden incident of complicatie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.75")
	private DSValue typeIncidentcomplicatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_ernst_complicatie", values = {
		@DSValueSetValue(code = "255604002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "6736007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24484000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "399166001", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Ernst incident/complicatie", extraTekst = "Mate waarin complicatie is opgetreden of ernst van het incident", code = "2.16.840.1.113883.2.4.3.36.77.2.11.76")
	private DSValue ernstIncidentcomplicatie;

	public MdlVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(MdlVerrichting verrichting)
	{
		this.verrichting = verrichting;
	}

	public DSValue getTypeIncidentcomplicatie()
	{
		return typeIncidentcomplicatie;
	}

	public void setTypeIncidentcomplicatie(DSValue typeIncidentcomplicatie)
	{
		this.typeIncidentcomplicatie = typeIncidentcomplicatie;
	}

	public DSValue getErnstIncidentcomplicatie()
	{
		return ernstIncidentcomplicatie;
	}

	public void setErnstIncidentcomplicatie(DSValue ernstIncidentcomplicatie)
	{
		this.ernstIncidentcomplicatie = ernstIncidentcomplicatie;
	}

}
