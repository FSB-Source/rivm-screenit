package nl.rivm.screenit.model.colon.verslag.pa;

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
public class PaGegevensBijMaligniteit
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private PaPathologieProtocolColonbioptperPoliep pathologieProtocolColonbioptperPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_morfologie_PA_DDK", values = {
		@DSValueSetValue(code = "103680002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103679000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Vorm van de laesie", extraTekst = "Vorm is poliepeus of sessiel", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155059", isVerplicht = true)
	private DSValue vormVanDeLaesie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_neoplasm_histologic_grade_2tier", values = {
		@DSValueSetValue(code = "1960", codeSystem = "1.3.6.1.4.1.19376.1.8.5.50"),
		@DSValueSetValue(code = "1959", codeSystem = "1.3.6.1.4.1.19376.1.8.5.50"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Differentiatie", extraTekst = "Differentiatie", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155060", isVerplicht = true)
	private DSValue differentiatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_invasiediepte", values = {
		@DSValueSetValue(code = "370059003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "370060008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Invasiediepte", extraTekst = "Invasiediepte", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155070", isVerplicht = true)
	private DSValue invasiediepte;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_haggitt", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.11"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.11"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.11"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.11"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Haggitt level", extraTekst = "Haggitt level is een maat voor de invasiediepte", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155080")
	private DSValue haggittLevel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_kikuchi", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.12"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.12"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.12"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Kikuchi level", extraTekst = "Kikuchi level", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155090", isVerplicht = true)
	private DSValue kikuchiLevel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_invasion_lv", values = {
		@DSValueSetValue(code = "395553001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "395552006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "395554007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "395717001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369733002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369734008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "(Lymf-)angioinvasie", extraTekst = "(Lymf-)angioinvasie", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155100", isVerplicht = true)
	private DSValue lymfangioinvasie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_surgical_margin", values = {
		@DSValueSetValue(code = "55182004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "55182004:363714003=396631001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "370109009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Snijvlak vrij/niet vrij", extraTekst = "Of snijvlak vrij/niet vrij/niet te beoordelen is", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155110", isVerplicht = true)
	private DSValue snijvlakVrijnietVrij;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_distanceoftumorfrommargincategory", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.79")
	})
	@VraagElement(displayName = "Afstand tot snijvlak (categorie\u00ebn)", extraTekst = "Afstand tot snijvlak (in cm categorie\u00ebn)", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155120", isVerplicht = true)
	private DSValue afstandTotSnijvlakcategorieen;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "afstandTotSnijvlakexactValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "afstandTotSnijvlakexactUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "afstandTotSnijvlakexactNf"))
	})
	@VraagElement(displayName = "Afstand tot snijvlak (exact)", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155121", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "cm")
	})
	private NullFlavourQuantity afstandTotSnijvlakexact;

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
	@VraagElement(displayName = "Type tumor", extraTekst = "Vastgestelde laesie per poliep", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155150", isVerplicht = true)
	private DSValue typeTumor;

	@Column(length = 4096)
	@VraagElement(displayName = "Type tumor overige", extraTekst = "Ander soort tumor, als Type tumor =\"overige\"", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155151", isVerplicht = true)
	private String typeTumorOverige;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_category_proliferation", values = {
		@DSValueSetValue(code = "g1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.107"),
		@DSValueSetValue(code = "g2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.107"),
		@DSValueSetValue(code = "g3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.107")
	})
	@VraagElement(displayName = "Proliferatie KI-67", extraTekst = "Proliferatie van de KI-67 marker (bij neuro-endocriene tumor)", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155155", isVerplicht = true)
	private DSValue proliferatieKi67;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_category_mitotic_figures", values = {
		@DSValueSetValue(code = "g1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.108"),
		@DSValueSetValue(code = "g2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.108"),
		@DSValueSetValue(code = "g3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.108")
	})
	@VraagElement(displayName = "Mitosen per 2mm2", extraTekst = "Aantal mitosen per 2 mm2 (bij neuro-endocriene tumor)", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155160", isVerplicht = true)
	private DSValue mitosenPer2mm2;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_neuroendocrine_pT", values = {
		@DSValueSetValue(code = "443357004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443506002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "80898003", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Invasie diepte (NET)", extraTekst = "Invasie diepte (Neuroendocriene tumor)", code = "2.16.840.1.113883.2.4.3.36.77.2.10.155165", isVerplicht = true)
	private DSValue invasieDieptenet;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "perineuralgrowth", values = {
		@DSValueSetValue(code = "260415000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260373001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "720735008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "1156316003", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Perineurale groei", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290191")
	private DSValue perineuraleGroei;

	public PaPathologieProtocolColonbioptperPoliep getPathologieProtocolColonbioptperPoliep()
	{
		return pathologieProtocolColonbioptperPoliep;
	}

	public void setPathologieProtocolColonbioptperPoliep(PaPathologieProtocolColonbioptperPoliep pathologieProtocolColonbioptperPoliep)
	{
		this.pathologieProtocolColonbioptperPoliep = pathologieProtocolColonbioptperPoliep;
	}

	public DSValue getVormVanDeLaesie()
	{
		return vormVanDeLaesie;
	}

	public void setVormVanDeLaesie(DSValue vormVanDeLaesie)
	{
		this.vormVanDeLaesie = vormVanDeLaesie;
	}

	public DSValue getDifferentiatie()
	{
		return differentiatie;
	}

	public void setDifferentiatie(DSValue differentiatie)
	{
		this.differentiatie = differentiatie;
	}

	public DSValue getInvasiediepte()
	{
		return invasiediepte;
	}

	public void setInvasiediepte(DSValue invasiediepte)
	{
		this.invasiediepte = invasiediepte;
	}

	public DSValue getHaggittLevel()
	{
		return haggittLevel;
	}

	public void setHaggittLevel(DSValue haggittLevel)
	{
		this.haggittLevel = haggittLevel;
	}

	public DSValue getKikuchiLevel()
	{
		return kikuchiLevel;
	}

	public void setKikuchiLevel(DSValue kikuchiLevel)
	{
		this.kikuchiLevel = kikuchiLevel;
	}

	public DSValue getLymfangioinvasie()
	{
		return lymfangioinvasie;
	}

	public void setLymfangioinvasie(DSValue lymfangioinvasie)
	{
		this.lymfangioinvasie = lymfangioinvasie;
	}

	public DSValue getSnijvlakVrijnietVrij()
	{
		return snijvlakVrijnietVrij;
	}

	public void setSnijvlakVrijnietVrij(DSValue snijvlakVrijnietVrij)
	{
		this.snijvlakVrijnietVrij = snijvlakVrijnietVrij;
	}

	public DSValue getAfstandTotSnijvlakcategorieen()
	{
		return afstandTotSnijvlakcategorieen;
	}

	public void setAfstandTotSnijvlakcategorieen(DSValue afstandTotSnijvlakcategorieen)
	{
		this.afstandTotSnijvlakcategorieen = afstandTotSnijvlakcategorieen;
	}

	public NullFlavourQuantity getAfstandTotSnijvlakexact()
	{
		return afstandTotSnijvlakexact;
	}

	public void setAfstandTotSnijvlakexact(NullFlavourQuantity afstandTotSnijvlakexact)
	{
		this.afstandTotSnijvlakexact = afstandTotSnijvlakexact;
	}

	public DSValue getTypeTumor()
	{
		return typeTumor;
	}

	public void setTypeTumor(DSValue typeTumor)
	{
		this.typeTumor = typeTumor;
	}

	public String getTypeTumorOverige()
	{
		return typeTumorOverige;
	}

	public void setTypeTumorOverige(String typeTumorOverige)
	{
		this.typeTumorOverige = typeTumorOverige;
	}

	public DSValue getProliferatieKi67()
	{
		return proliferatieKi67;
	}

	public void setProliferatieKi67(DSValue proliferatieKi67)
	{
		this.proliferatieKi67 = proliferatieKi67;
	}

	public DSValue getMitosenPer2mm2()
	{
		return mitosenPer2mm2;
	}

	public void setMitosenPer2mm2(DSValue mitosenPer2mm2)
	{
		this.mitosenPer2mm2 = mitosenPer2mm2;
	}

	public DSValue getInvasieDieptenet()
	{
		return invasieDieptenet;
	}

	public void setInvasieDieptenet(DSValue invasieDieptenet)
	{
		this.invasieDieptenet = invasieDieptenet;
	}

	public DSValue getPerineuraleGroei()
	{
		return perineuraleGroei;
	}

	public void setPerineuraleGroei(DSValue perineuraleGroei)
	{
		this.perineuraleGroei = perineuraleGroei;
	}

}
