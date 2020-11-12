
package nl.rivm.screenit.model.colon.verslag.mdl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlLaesiecoloscopiecentrum
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(
		displayName = "Nummer potje monster",
		extraTekst = "Nummer potje afgenomen materiaal zoals ingestuurd voor pathologie (bijvoorbeeld: I, II, III, IV, of 1, 2, 3)",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145050",
		isVerplicht = true)
	private String nummerPotjeMonster;

	@Column(length = 255)
	@VraagElement(displayName = "Monster identificatie", extraTekst = "Unieke monster identificatie (bijvoorbeeld T12-12345.I)", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145054")
	private String monsterIdentificatie;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "volgnummerLaesieValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "volgnummerLaesieUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "volgnummerLaesieNf"))
	})
	@VraagElement(displayName = "Volgnummer laesie", extraTekst = "Volgnummer laesie", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145053", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "aantal")
	})
	private NullFlavourQuantity volgnummerLaesie;

	@Column
	@VraagElement(
		displayName = "Materiaal (laesie) ingezonden voor pathologie",
		extraTekst = "Materiaal (laesie) ingezonden voor pathologie",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145052")
	private Boolean materiaallaesieIngezondenVoorPathologie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_lokalisatie", values = {
		@DSValueSetValue(code = "181256004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "32713005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "9040008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245427008", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "48338005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "485005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24542800", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "245428003", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "72592005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "32622004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "362166007", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "60184004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245429006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "49832006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "181261002", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "34402009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "53505006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "85774003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "23153004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "41796003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "256874006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245857005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(
		displayName = "Lokalisatie laesie",
		extraTekst = "Lokalisatie van de gedetecteerde laesie in het colon",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145060",
		isVerplicht = true)
	private DSValue lokalisatieLaesie;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "afstandVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "afstandVanafAnusUnit"))
	})
	@VraagElement(
		displayName = "Afstand vanaf anus",
		extraTekst = "Afstand vanaf anus (in cm), als alternatief indien localisatie laesie niet is ingevuld",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145061",
		isVerplicht = true,
		unit = {
			@VraagElementUnit(unit = "cm", min = "1.0", max = "200.0")
		})
	private Quantity afstandVanafAnus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_monstermateriaal", values = {
		@DSValueSetValue(code = "309226005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "258415003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "NA", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(
		displayName = "Type afgenomen materiaal",
		extraTekst = "Materiaal dat is afgenomen tijdens coloscopie per poliep",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145070",
		isVerplicht = true)
	private DSValue typeAfgenomenMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_klindiag_coloscopie", values = {
		@DSValueSetValue(code = "89452002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "428054006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "67401000119103:116676008=61647009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "449855005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "408645001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "76235005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "277163006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "82375006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.30", deprecated = true),
		@DSValueSetValue(code = "128653004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "35917007", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(
		displayName = "Klinische diagnose",
		extraTekst = "Macroscopische diagnose (per poliep) zoals gesteld door endoscopist",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145040")
	private DSValue klinischeDiagnose;

	@Column(length = 4096)
	@VraagElement(displayName = "Overige klinische diagnose (tekst)", extraTekst = "Overige klinische diagnose (tekst)", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145042")
	private String overigeKlinischeDiagnosetekst;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "laesiecoloscopiecentrum", cascade = CascadeType.ALL)
	@VraagElement(
		displayName = "Poliep",
		extraTekst = "Gegevens van een poliep zoals vastgesteld door endoscopist",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145019",
		isReference = true)
	private MdlPoliep poliep;

	@Column
	@VraagElement(displayName = "Verdenking carcinoom ja/nee", extraTekst = "Verdenking carcinoom ja/nee", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145105", isVerplicht = true)
	private Boolean verdenkingCarcinoomJanee;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "laesiecoloscopiecentrum", cascade = CascadeType.ALL)
	@VraagElement(
		displayName = "(Verdenking van) carcinoom",
		extraTekst = "Gegevens indien sprake is van (verdenking van) een carcinoom",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.145104",
		isReference = true)
	private MdlVanCarcinoom vanCarcinoom;

	@Column
	@VraagElement(displayName = "Markering geplaatst", extraTekst = "Of er een markering (tatoeage) is geplaatst", code = "2.16.840.1.113883.2.4.3.36.77.2.8.145092")
	private Boolean markeringGeplaatst;

	public MdlVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(MdlVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public String getNummerPotjeMonster()
	{
		return nummerPotjeMonster;
	}

	public void setNummerPotjeMonster(String nummerPotjeMonster)
	{
		this.nummerPotjeMonster = nummerPotjeMonster;
	}

	public String getMonsterIdentificatie()
	{
		return monsterIdentificatie;
	}

	public void setMonsterIdentificatie(String monsterIdentificatie)
	{
		this.monsterIdentificatie = monsterIdentificatie;
	}

	public NullFlavourQuantity getVolgnummerLaesie()
	{
		return volgnummerLaesie;
	}

	public void setVolgnummerLaesie(NullFlavourQuantity volgnummerLaesie)
	{
		this.volgnummerLaesie = volgnummerLaesie;
	}

	public Boolean getMateriaallaesieIngezondenVoorPathologie()
	{
		return materiaallaesieIngezondenVoorPathologie;
	}

	public void setMateriaallaesieIngezondenVoorPathologie(Boolean materiaallaesieIngezondenVoorPathologie)
	{
		this.materiaallaesieIngezondenVoorPathologie = materiaallaesieIngezondenVoorPathologie;
	}

	public DSValue getLokalisatieLaesie()
	{
		return lokalisatieLaesie;
	}

	public void setLokalisatieLaesie(DSValue lokalisatieLaesie)
	{
		this.lokalisatieLaesie = lokalisatieLaesie;
	}

	public Quantity getAfstandVanafAnus()
	{
		return afstandVanafAnus;
	}

	public void setAfstandVanafAnus(Quantity afstandVanafAnus)
	{
		this.afstandVanafAnus = afstandVanafAnus;
	}

	public DSValue getTypeAfgenomenMateriaal()
	{
		return typeAfgenomenMateriaal;
	}

	public void setTypeAfgenomenMateriaal(DSValue typeAfgenomenMateriaal)
	{
		this.typeAfgenomenMateriaal = typeAfgenomenMateriaal;
	}

	public DSValue getKlinischeDiagnose()
	{
		return klinischeDiagnose;
	}

	public void setKlinischeDiagnose(DSValue klinischeDiagnose)
	{
		this.klinischeDiagnose = klinischeDiagnose;
	}

	public String getOverigeKlinischeDiagnosetekst()
	{
		return overigeKlinischeDiagnosetekst;
	}

	public void setOverigeKlinischeDiagnosetekst(String overigeKlinischeDiagnosetekst)
	{
		this.overigeKlinischeDiagnosetekst = overigeKlinischeDiagnosetekst;
	}

	public MdlPoliep getPoliep()
	{
		return poliep;
	}

	public void setPoliep(MdlPoliep poliep)
	{
		this.poliep = poliep;
	}

	public Boolean getVerdenkingCarcinoomJanee()
	{
		return verdenkingCarcinoomJanee;
	}

	public void setVerdenkingCarcinoomJanee(Boolean verdenkingCarcinoomJanee)
	{
		this.verdenkingCarcinoomJanee = verdenkingCarcinoomJanee;
	}

	public MdlVanCarcinoom getVanCarcinoom()
	{
		return vanCarcinoom;
	}

	public void setVanCarcinoom(MdlVanCarcinoom vanCarcinoom)
	{
		this.vanCarcinoom = vanCarcinoom;
	}

	public Boolean getMarkeringGeplaatst()
	{
		return markeringGeplaatst;
	}

	public void setMarkeringGeplaatst(Boolean markeringGeplaatst)
	{
		this.markeringGeplaatst = markeringGeplaatst;
	}

}
