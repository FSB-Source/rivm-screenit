package nl.rivm.screenit.model.colon.verslag.mdl;

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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlVerslagContent
	extends VerslagContent<MdlVerslag>
{

	private final static long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private MdlVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Verrichting", extraTekst = "Verrichting", code = "2.16.840.1.113883.2.4.3.36.77.2.11.68", isReference = true)
	private MdlVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Coloscopie : medische observatie", extraTekst = "Coloscopie : medische observatie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.80", isReference = true)
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Laesie (coloscopiecentrum)", extraTekst = "Weggenomen materiaal, poliep, verdenking carcinoom (coloscopiecentrum)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.121", isReference = true)
	@OrderBy("to_number(coalesce(nullif(volgnummerLaesie.value,''),to_char(id, '9999999999')), '9999999999')")
	private List<MdlLaesiecoloscopiecentrum> laesiecoloscopiecentrum = new ArrayList<>();

	@Override
	public MdlVerslag getVerslag()
	{
		return verslag;
	}

	@Override
	public void setVerslag(MdlVerslag verslag)
	{
		this.verslag = verslag;
	}

	public MdlVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(MdlVerrichting verrichting)
	{
		this.verrichting = verrichting;
	}

	public MdlColoscopieMedischeObservatie getColoscopieMedischeObservatie()
	{
		return coloscopieMedischeObservatie;
	}

	public void setColoscopieMedischeObservatie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		this.coloscopieMedischeObservatie = coloscopieMedischeObservatie;
	}

	public List<MdlLaesiecoloscopiecentrum> getLaesiecoloscopiecentrum()
	{
		return laesiecoloscopiecentrum;
	}

	public void setLaesiecoloscopiecentrum(List<MdlLaesiecoloscopiecentrum> laesiecoloscopiecentrum)
	{
		this.laesiecoloscopiecentrum = laesiecoloscopiecentrum;
	}

}
