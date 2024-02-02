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

import javax.annotation.Nonnull;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlPoliep
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "diameterPoliepValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "diameterPoliepUnit"))
	})
	@VraagElement(displayName = "Diameter poliep", extraTekst = "Diameter van de poliep zoals vastgesteld door endoscopist", code = "2.16.840.1.113883.2.4.3.36.77.2.11.133", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "mm")
	})
	private Quantity diameterPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_morfologie_coloscopie", values = {
		@DSValueSetValue(code = "103680002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103679000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Morfologie", extraTekst = "Beschrijving morfologie van de poliep", code = "2.16.840.1.113883.2.4.3.36.77.2.11.134", isVerplicht = true)
	private DSValue morfologie;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_poliep_manier_van_verwijderen")
	@DSValueSet(name = "vs_verwijderingstechniek", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Manier van verwijderen", extraTekst = "Manier waarop de poliep of anderszins is weggehaald", code = "2.16.840.1.113883.2.4.3.36.77.2.11.135", isVerplicht = true)
	private List<DSValue> manierVanVerwijderen = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_verwijdering_compleet", values = {
		@DSValueSetValue(code = "255619001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35"),
		@DSValueSetValue(code = "255599008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35")
	})
	@VraagElement(displayName = "Volledigheid wegname materiaal", extraTekst = "Of materiaal volledig/partieel is weggenomen", code = "2.16.840.1.113883.2.4.3.36.77.2.3.145090", isVerplicht = true, useInFormulier = false)
	private DSValue volledigheidWegnameMateriaal;

	@Column(length = 4096)
	@VraagElement(displayName = "Overige manier van verwijderen", extraTekst = "Overige manier van verwijderen", code = "2.16.840.1.113883.2.4.3.36.77.2.11.136", isVerplicht = true)
	private String overigeManierVanVerwijderen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_method_of_excision", values = {
		@DSValueSetValue(code = "255619001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35")
	})
	@VraagElement(displayName = "Methode van verwijderen", extraTekst = "Methode van verwijderen", code = "2.16.840.1.113883.2.4.3.36.77.2.11.137", isVerplicht = true)
	private DSValue methodeVanVerwijderen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_extent", values = {
		@DSValueSetValue(code = "255612005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "255599008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Resultaat verwijdering", extraTekst = "Resultaat verwijdering", code = "2.16.840.1.113883.2.4.3.36.77.2.11.138", isVerplicht = true)
	private DSValue resultaatVerwijdering;

	public MdlLaesiecoloscopiecentrum getLaesiecoloscopiecentrum()
	{
		return laesiecoloscopiecentrum;
	}

	public void setLaesiecoloscopiecentrum(MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum)
	{
		this.laesiecoloscopiecentrum = laesiecoloscopiecentrum;
	}

	public Quantity getDiameterPoliep()
	{
		return diameterPoliep;
	}

	public void setDiameterPoliep(Quantity diameterPoliep)
	{
		this.diameterPoliep = diameterPoliep;
	}

	public DSValue getMorfologie()
	{
		return morfologie;
	}

	public void setMorfologie(DSValue morfologie)
	{
		this.morfologie = morfologie;
	}

	public DSValue getVolledigheidWegnameMateriaal()
	{
		return volledigheidWegnameMateriaal;
	}

	public void setVolledigheidWegnameMateriaal(DSValue volledigheidWegnameMateriaal)
	{
		this.volledigheidWegnameMateriaal = volledigheidWegnameMateriaal;
	}

	public List<DSValue> getManierVanVerwijderen()
	{
		return manierVanVerwijderen;
	}

	public void setManierVanVerwijderen(List<DSValue> manierVanVerwijderen)
	{
		this.manierVanVerwijderen = manierVanVerwijderen;
	}

	public String getOverigeManierVanVerwijderen()
	{
		return overigeManierVanVerwijderen;
	}

	public void setOverigeManierVanVerwijderen(String overigeManierVanVerwijderen)
	{
		this.overigeManierVanVerwijderen = overigeManierVanVerwijderen;
	}

	public DSValue getMethodeVanVerwijderen()
	{
		return methodeVanVerwijderen;
	}

	public void setMethodeVanVerwijderen(DSValue methodeVanVerwijderen)
	{
		this.methodeVanVerwijderen = methodeVanVerwijderen;
	}

	public DSValue getResultaatVerwijdering()
	{
		return resultaatVerwijdering;
	}

	public void setResultaatVerwijdering(DSValue resultaatVerwijdering)
	{
		this.resultaatVerwijdering = resultaatVerwijdering;
	}

}
