package nl.rivm.screenit.model.colon.verslag.pa;

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
public class PaGegevensBijMaligniteit
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private PaPathologieProtocolColonbioptperPoliep pathologieProtocolColonbioptperPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_type_tumor_PA", values = {
		@DSValueSetValue(code = "35917007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "72495009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "87737001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "59367005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "32913002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "38549000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "450895005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "450948005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "388986005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128928004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "127572005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "127573000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "127574006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128628002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "74364000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "51465000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "31396002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Type tumor", extraTekst = "Vastgestelde laesie per poliep", code = "2.16.840.1.113883.2.4.3.36.77.2.11.188", isVerplicht = true)
	private DSValue typeTumor;

	public PaPathologieProtocolColonbioptperPoliep getPathologieProtocolColonbioptperPoliep()
	{
		return pathologieProtocolColonbioptperPoliep;
	}

	public void setPathologieProtocolColonbioptperPoliep(PaPathologieProtocolColonbioptperPoliep pathologieProtocolColonbioptperPoliep)
	{
		this.pathologieProtocolColonbioptperPoliep = pathologieProtocolColonbioptperPoliep;
	}

	public DSValue getTypeTumor()
	{
		return typeTumor;
	}

	public void setTypeTumor(DSValue typeTumor)
	{
		this.typeTumor = typeTumor;
	}

}
