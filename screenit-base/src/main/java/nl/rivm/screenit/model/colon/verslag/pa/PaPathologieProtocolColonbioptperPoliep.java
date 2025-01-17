package nl.rivm.screenit.model.colon.verslag.pa;

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

import javax.annotation.Nonnull;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
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
public class PaPathologieProtocolColonbioptperPoliep
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private PaVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(displayName = "Nummer potje materiaal", extraTekst = "Nummer potje van het binngekomen materiaal", code = "2.16.840.1.113883.2.4.3.36.77.2.11.156", isVerplicht = true)
	private String nummerPotjeMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_internextern", values = {
		@DSValueSetValue(code = "260521003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "261074009", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Consult materiaal aangevraagd", extraTekst = "Of er (intern/extern) consult materiaal is opgevraagd uit een ander laboratorium", code = "2.16.840.1.113883.2.4.3.36.77.2.11.157")
	private DSValue consultMateriaalAangevraagd;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_neoplasiegraad", values = {
		@DSValueSetValue(code = "399611001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "399415002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Dysplasiegraad", extraTekst = "Hoog- of laaggradige dysplasie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.158", isVerplicht = true)
	private DSValue dysplasiegraad;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "diameterPoliepValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "diameterPoliepUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "diameterPoliepNf"))
	})
	@VraagElement(displayName = "Diameter poliep", extraTekst = "Diameter van het verwijderde materiaal", code = "2.16.840.1.113883.2.4.3.36.77.2.11.160", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "cm", min = "0.1", max = "20.0")
	})
	private NullFlavourQuantity diameterPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_primaire_afwijking", values = {
		@DSValueSetValue(code = "23875004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "12402003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443897009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "61722000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "62047007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128859003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443734007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443157008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "783210009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTHPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13"),
		@DSValueSetValue(code = "76235005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "110448004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "277161008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "46720004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44598004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "80297003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "53801007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "89084002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "12169001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128795001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24183004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "281268007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "125154007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "400110009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "32110003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "123827008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373379001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "162572001:246090004=269533000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "64226004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "MUCPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13", deprecated = true),
		@DSValueSetValue(code = "MESPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13", deprecated = true),
		@DSValueSetValue(code = "269533000:408729009=415684004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Primaire afwijking", extraTekst = "Gevonden primaire afwijking", code = "2.16.840.1.113883.2.4.3.36.77.2.11.162", isVerplicht = true)
	private DSValue primaireAfwijking;

	@Column(length = 4096)
	@VraagElement(displayName = "Andere primaire afwijking", extraTekst = "Specificatie (tekst) andere primaire afwijking", code = "2.16.840.1.113883.2.4.3.36.77.2.11.163")
	private String anderePrimaireAfwijking;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_pa_bevinding", values = {
		@DSValueSetValue(code = "110396000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373379001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "395705003", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "162572001:246090004=269533000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Bevinding", extraTekst = "Bevinding", code = "2.16.840.1.113883.2.4.3.36.77.2.11.164")
	private DSValue bevinding;

	@Column(length = 4096)
	@VraagElement(displayName = "Specificatie overige bevinding", extraTekst = "Specificatie (tekst) overige bevinding", code = "2.16.840.1.113883.2.4.3.36.77.2.11.165")
	private String specificatieOverigeBevinding;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "pathologieProtocolColonbioptperPoliep", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Gegevens bij maligniteit", extraTekst = "Gegevens bij maligniteit", code = "2.16.840.1.113883.2.4.3.36.77.2.11.177", isReference = true)
	private PaGegevensBijMaligniteit gegevensBijMaligniteit;

	public PaVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(PaVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public String getNummerPotjeMateriaal()
	{
		return nummerPotjeMateriaal;
	}

	public void setNummerPotjeMateriaal(String nummerPotjeMateriaal)
	{
		this.nummerPotjeMateriaal = nummerPotjeMateriaal;
	}

	public DSValue getConsultMateriaalAangevraagd()
	{
		return consultMateriaalAangevraagd;
	}

	public void setConsultMateriaalAangevraagd(DSValue consultMateriaalAangevraagd)
	{
		this.consultMateriaalAangevraagd = consultMateriaalAangevraagd;
	}

	public DSValue getDysplasiegraad()
	{
		return dysplasiegraad;
	}

	public void setDysplasiegraad(DSValue dysplasiegraad)
	{
		this.dysplasiegraad = dysplasiegraad;
	}

	public NullFlavourQuantity getDiameterPoliep()
	{
		return diameterPoliep;
	}

	public void setDiameterPoliep(NullFlavourQuantity diameterPoliep)
	{
		this.diameterPoliep = diameterPoliep;
	}

	public DSValue getPrimaireAfwijking()
	{
		return primaireAfwijking;
	}

	public void setPrimaireAfwijking(DSValue primaireAfwijking)
	{
		this.primaireAfwijking = primaireAfwijking;
	}

	public String getAnderePrimaireAfwijking()
	{
		return anderePrimaireAfwijking;
	}

	public void setAnderePrimaireAfwijking(String anderePrimaireAfwijking)
	{
		this.anderePrimaireAfwijking = anderePrimaireAfwijking;
	}

	public DSValue getBevinding()
	{
		return bevinding;
	}

	public void setBevinding(DSValue bevinding)
	{
		this.bevinding = bevinding;
	}

	public String getSpecificatieOverigeBevinding()
	{
		return specificatieOverigeBevinding;
	}

	public void setSpecificatieOverigeBevinding(String specificatieOverigeBevinding)
	{
		this.specificatieOverigeBevinding = specificatieOverigeBevinding;
	}

	public PaGegevensBijMaligniteit getGegevensBijMaligniteit()
	{
		return gegevensBijMaligniteit;
	}

	public void setGegevensBijMaligniteit(PaGegevensBijMaligniteit gegevensBijMaligniteit)
	{
		this.gegevensBijMaligniteit = gegevensBijMaligniteit;
	}

}
