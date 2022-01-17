package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.annotation.Nonnull;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlMedicatiemiddel
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlMedicatie medicatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_medicatie_coloscopie", values = {
		@DSValueSetValue(code = "N05CD08", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N05BA01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N02AB02", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AX10", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH02", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "A03BB01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH03", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Medicatie tijdens coloscopie", extraTekst = "Medicatie gegevens tijdens coloscopie, middel ter sedatie of relaxerend. Dit concept bevat de naam van het middel.", code = "2.16.840.1.113883.2.4.3.36.77.2.10.140090", isVerplicht = true)
	private DSValue medicatieTijdensColoscopie;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "doseringMedicatieValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "doseringMedicatieUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "doseringMedicatieNf"))
	})
	@VraagElement(displayName = "Dosering medicatie", extraTekst = "Hoeveelheid medicatie die gegeven is tijdens de coloscopie", code = "2.16.840.1.113883.2.4.3.36.77.2.10.140100", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "mg"),
		@VraagElementUnit(unit = "\u00b5g")
	})
	private NullFlavourQuantity doseringMedicatie;

	public MdlMedicatie getMedicatie()
	{
		return medicatie;
	}

	public void setMedicatie(MdlMedicatie medicatie)
	{
		this.medicatie = medicatie;
	}

	public DSValue getMedicatieTijdensColoscopie()
	{
		return medicatieTijdensColoscopie;
	}

	public void setMedicatieTijdensColoscopie(DSValue medicatieTijdensColoscopie)
	{
		this.medicatieTijdensColoscopie = medicatieTijdensColoscopie;
	}

	public NullFlavourQuantity getDoseringMedicatie()
	{
		return doseringMedicatie;
	}

	public void setDoseringMedicatie(NullFlavourQuantity doseringMedicatie)
	{
		this.doseringMedicatie = doseringMedicatie;
	}

}
