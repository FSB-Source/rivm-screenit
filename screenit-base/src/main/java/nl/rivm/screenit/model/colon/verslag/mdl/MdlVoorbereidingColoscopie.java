
package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlVoorbereidingColoscopie
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_voorbereiding_coloscopie_type_voorbereiding")
	@DSValueSet(name = "vs_voorbereiding_coloscopie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.75"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(
		displayName = "Type voorbereiding",
		extraTekst = "Voorbereiding voor coloscopie door cli\u00ebnt",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.140005",
		isVerplicht = true)
	private List<DSValue> typeVoorbereiding = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_singleorsplitdose", values = {
		@DSValueSetValue(code = "307486002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "1217004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(
		displayName = "Split dose",
		extraTekst = "Indien de medicatie ter voorbereiding op de coloscopie als split-dose is voorgeschreven geef dat dan hier aan met \"ja\"",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.140027",
		isVerplicht = true)
	private DSValue splitDose;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "voorbereidingColoscopie", cascade = CascadeType.ALL)
	@VraagElement(
		displayName = "Boston Bowel Preparation",
		extraTekst = "Boston Bowel Preparation Scale: Locatie en score",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.140008",
		isReference = true)
	private MdlBostonBowelPreparation bostonBowelPreparation;

	public MdlColoscopieMedischeObservatie getColoscopieMedischeObservatie()
	{
		return coloscopieMedischeObservatie;
	}

	public void setColoscopieMedischeObservatie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		this.coloscopieMedischeObservatie = coloscopieMedischeObservatie;
	}

	public List<DSValue> getTypeVoorbereiding()
	{
		return typeVoorbereiding;
	}

	public void setTypeVoorbereiding(List<DSValue> typeVoorbereiding)
	{
		this.typeVoorbereiding = typeVoorbereiding;
	}

	public DSValue getSplitDose()
	{
		return splitDose;
	}

	public void setSplitDose(DSValue splitDose)
	{
		this.splitDose = splitDose;
	}

	public MdlBostonBowelPreparation getBostonBowelPreparation()
	{
		return bostonBowelPreparation;
	}

	public void setBostonBowelPreparation(MdlBostonBowelPreparation bostonBowelPreparation)
	{
		this.bostonBowelPreparation = bostonBowelPreparation;
	}

}
