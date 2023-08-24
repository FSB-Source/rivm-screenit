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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlMedicatie
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "medicatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Medicatiemiddel", extraTekst = "Medicatiemiddel", code = "2.16.840.1.113883.2.4.3.36.77.2.11.91", isReference = true)
	private List<MdlMedicatiemiddel> medicatiemiddel = new ArrayList<>();

	@Column
	@VraagElement(displayName = "Sedatie ja/nee", extraTekst = "Sedatie ja/nee", code = "2.16.840.1.113883.2.4.3.36.77.2.11.95", isVerplicht = true)
	private Boolean sedatieJanee;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_matevansedatie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "NA", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Mate van sedatie", extraTekst = "Mate waarin sedatie gelukt is volgens de Leeds score", code = "2.16.840.1.113883.2.4.3.36.77.2.11.96", isVerplicht = true)
	private DSValue mateVanSedatie;

	public MdlColoscopieMedischeObservatie getColoscopieMedischeObservatie()
	{
		return coloscopieMedischeObservatie;
	}

	public void setColoscopieMedischeObservatie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		this.coloscopieMedischeObservatie = coloscopieMedischeObservatie;
	}

	public List<MdlMedicatiemiddel> getMedicatiemiddel()
	{
		return medicatiemiddel;
	}

	public void setMedicatiemiddel(List<MdlMedicatiemiddel> medicatiemiddel)
	{
		this.medicatiemiddel = medicatiemiddel;
	}

	public Boolean getSedatieJanee()
	{
		return sedatieJanee;
	}

	public void setSedatieJanee(Boolean sedatieJanee)
	{
		this.sedatieJanee = sedatieJanee;
	}

	public DSValue getMateVanSedatie()
	{
		return mateVanSedatie;
	}

	public void setMateVanSedatie(DSValue mateVanSedatie)
	{
		this.mateVanSedatie = mateVanSedatie;
	}

}
