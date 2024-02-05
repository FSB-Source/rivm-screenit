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
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
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
public class MdlColoscopieMedischeObservatie
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Voorbereiding coloscopie", extraTekst = "Voorbereiding voor coloscopie door cli\u00ebnt", code = "2.16.840.1.113883.2.4.3.36.77.2.11.81", isReference = true)
	private MdlVoorbereidingColoscopie voorbereidingColoscopie;

	@Column
	@VraagElement(displayName = "Time-out procedure doorlopen", extraTekst = "Time-out procedure doorlopen ja/nee", code = "2.16.840.1.113883.2.4.3.36.77.2.11.89", isVerplicht = true)
	private Boolean timeoutProcedureDoorlopen;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Medicatie", extraTekst = "Medicatie gegeven tijdens coloscopie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.90", isReference = true)
	private MdlMedicatie medicatie;

	@Column
	@VraagElement(displayName = "CO2 insufflatie", extraTekst = "Wel of geen CO2 insufflatie gebruikt", code = "2.16.840.1.113883.2.4.3.36.77.2.11.97", isVerplicht = true)
	private Boolean co2Insufflatie;

	@Column
	@VraagElement(displayName = "Coecum / terminaal ileum intubatie", extraTekst = "Coecum / terminaal ileum bereikt (ja/nee)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.98", isVerplicht = true)
	private Boolean coecumTerminaalIleumIntubatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_reden_coecum_niet_bereikt", values = {
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "13", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.250"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Reden coecum niet bereikt", extraTekst = "Als het coecum niet is bereikt is, wat is de reden", code = "2.16.840.1.113883.2.4.3.36.77.2.11.99", isVerplicht = true)
	private DSValue redenCoecumNietBereikt;

	@Column(length = 4096)
	@VraagElement(displayName = "Text reden coecum niet bereikt", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.100", isVerplicht = true)
	private String textRedenCoecumNietBereikt;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_lokalisatie_bereikt", values = {
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
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Diepste punt insertie", extraTekst = "Als het coecum niet bereikt is, hoever er wel gekomen is (diepste punt insertie)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.101", isVerplicht = true)
	private DSValue diepstePuntInsertie;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "afstandVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "afstandVanafAnusUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "afstandVanafAnusNf"))
	})
	@VraagElement(displayName = "Afstand vanaf anus", extraTekst = "Afstand vanaf anus (in cm)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.102", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "cm", min = "1.0", max = "200.0")
	})
	private NullFlavourQuantity afstandVanafAnus;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "totaleTerugtrektijdValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "totaleTerugtrektijdUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "totaleTerugtrektijdNf"))
	})
	@VraagElement(displayName = "Totale terugtrektijd", extraTekst = "Totale terugtrektijd in minuten. Ongecorrigeerd voor tijdelijke stops voor bijv. nemen van een biopt of poliepectomie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.103", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "min")
	})
	private NullFlavourQuantity totaleTerugtrektijd;

	@Column
	@VraagElement(displayName = "Retroflexie rectum", extraTekst = "Retroflexie rectum", code = "2.16.840.1.113883.2.4.3.36.77.2.11.104", isVerplicht = true)
	private Boolean retroflexieRectum;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_gloucestercomfortscore", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104")
	})
	@VraagElement(displayName = "Pati\u00ebntcomfort (GCS)", extraTekst = "Mate van pati\u00ebntcomfort, gecodeerd volgens Gloucester comfort score standaard", code = "2.16.840.1.113883.2.4.3.36.77.2.11.105", isVerplicht = true)
	private DSValue patientcomfortgcs;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_coloscopie_medische_observatie_reden_afbreking_coloscopie")
	@DSValueSet(name = "vs_afbreken_coloscopie", values = {
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Reden afbreking coloscopie", extraTekst = "Als de coloscopie is afgebroken, wat is de reden dat het onderzoek incompleet is", code = "2.16.840.1.113883.2.4.3.36.77.2.11.106")
	private List<DSValue> redenAfbrekingColoscopie = new ArrayList<>();

	@Column(length = 4096)
	@VraagElement(displayName = "Text reden afbreking coloscopie", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.107", isVerplicht = true)
	private String textRedenAfbrekingColoscopie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_terkoppeling_coloscopie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38")
	})
	@VraagElement(displayName = "Terugkoppeling cli\u00ebnt", extraTekst = "Directe terugkoppeling naar cli\u00ebnt na afloop van coloscopie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.108")
	private DSValue terugkoppelingClient;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Definitief vervolgbeleid voor bevolkingsonderzoek (groep)", extraTekst = "Definitief vervolgbeleid voor bevolkingsonderzoek (groep)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.109", isReference = true)
	private MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidVoorBevolkingsonderzoekg;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_coloscopie_medische_observatie_overige_bevinding")
	@DSValueSet(name = "vs_overige_bevindingen", values = {
		@DSValueSetValue(code = "64226004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "3951002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "52457000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "397881000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "307496006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "70153002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30037006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "54609002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "90858003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Overige bevinding", extraTekst = "Bevinding die niet binnen screening past", code = "2.16.840.1.113883.2.4.3.36.77.2.11.114")
	private List<DSValue> overigeBevinding = new ArrayList<>();

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "totaalAantalGedetecteerdeLaesiesValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "totaalAantalGedetecteerdeLaesiesUnit"))
	})
	@VraagElement(displayName = "Totaal aantal gedetecteerde laesies", extraTekst = "Het totaal aantal gedetecteerde laesies, omvat alle poliepen, ongeacht of deze zijn verwijderd, zijn geregistreerd of zijn ingezonden voor PA", code = "2.16.840.1.113883.2.4.3.36.77.2.11.115", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "aantal")
	})
	private Quantity totaalAantalGedetecteerdeLaesies;

	@Embedded
	@Nonnull
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "aantalVerwijderdeLaesiesNietIngezondenVoorPaEnGValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "aantalVerwijderdeLaesiesNietIngezondenVoorPaEnGUnit"))
	})
	@VraagElement(displayName = "Aantal verwijderde laesies niet ingezonden voor PA en gegevens niet vastgelegd", extraTekst = "Totaal aantal verwijderde poliepen van het totaal aantal gedetecteerde poliepen, welke niet zijn vastgelegd en niet zijn ingezonden voor PA", code = "2.16.840.1.113883.2.4.3.36.77.2.11.116", isVerplicht = true, unit = {
		@VraagElementUnit(unit = "aantal")
	})
	private Quantity aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_reasonnotsenttopathology", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Reden poliep(en) niet ingezonden voor pathologie", extraTekst = "Reden poliep(en) niet ingezonden voor pathologie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.117", isVerplicht = true)
	private DSValue redenPoliepenNietIngezondenVoorPathologie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Opdrachtnummer PA-lab", extraTekst = "Opdrachtnummer PA-lab", code = "2.16.840.1.113883.2.4.3.36.77.2.11.118-group", isReference = true)
	private List<MdlOpdrachtnummerPalab> opdrachtnummerPalab = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(displayName = "T-nummer pathologie verslag", extraTekst = "T-nummer pathologie verslag", code = "2.16.840.1.113883.2.4.3.36.77.2.11.119-group", isReference = true)
	private List<MdlTnummerPathologieVerslag> tnummerPathologieVerslag = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_conclusie_coloscopie", values = {
		@DSValueSetValue(code = "313170008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "NAAD", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78", deprecated = true),
		@DSValueSetValue(code = "AAD", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78", deprecated = true),
		@DSValueSetValue(code = "449855005", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "CRC", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "255046005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "448315008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "285611007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "HRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "LRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Eindconclusie", extraTekst = "Eindconclusie na interpretatie van eventueel PA verslag. Op basis van dit veld wordt vervolgbeleid voor deze pati\u00ebnt vastgesteld. Het gaat hier om vastlegging van de meest significante afwijking.", code = "2.16.840.1.113883.2.4.3.36.77.2.11.120", isVerplicht = true)
	private DSValue eindconclusie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_vervolgbeleid_afbreken_coloscopie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.106"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.106"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.106"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.106"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(displayName = "Vervolgbeleid n.a.v. afbreking coloscopie", extraTekst = "Vervolgbeleid n.a.v. afbreking coloscopie", code = "2.16.840.1.113883.2.4.3.36.77.2.2.140122", useInFormulier = false)
	private DSValue vervolgbeleidNavAfbrekingColoscopie;

	public MdlVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(MdlVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public MdlVoorbereidingColoscopie getVoorbereidingColoscopie()
	{
		return voorbereidingColoscopie;
	}

	public void setVoorbereidingColoscopie(MdlVoorbereidingColoscopie voorbereidingColoscopie)
	{
		this.voorbereidingColoscopie = voorbereidingColoscopie;
	}

	public Boolean getTimeoutProcedureDoorlopen()
	{
		return timeoutProcedureDoorlopen;
	}

	public void setTimeoutProcedureDoorlopen(Boolean timeoutProcedureDoorlopen)
	{
		this.timeoutProcedureDoorlopen = timeoutProcedureDoorlopen;
	}

	public MdlMedicatie getMedicatie()
	{
		return medicatie;
	}

	public void setMedicatie(MdlMedicatie medicatie)
	{
		this.medicatie = medicatie;
	}

	public Boolean getCo2Insufflatie()
	{
		return co2Insufflatie;
	}

	public void setCo2Insufflatie(Boolean co2Insufflatie)
	{
		this.co2Insufflatie = co2Insufflatie;
	}

	public Boolean getCoecumTerminaalIleumIntubatie()
	{
		return coecumTerminaalIleumIntubatie;
	}

	public void setCoecumTerminaalIleumIntubatie(Boolean coecumTerminaalIleumIntubatie)
	{
		this.coecumTerminaalIleumIntubatie = coecumTerminaalIleumIntubatie;
	}

	public DSValue getRedenCoecumNietBereikt()
	{
		return redenCoecumNietBereikt;
	}

	public void setRedenCoecumNietBereikt(DSValue redenCoecumNietBereikt)
	{
		this.redenCoecumNietBereikt = redenCoecumNietBereikt;
	}

	public String getTextRedenCoecumNietBereikt()
	{
		return textRedenCoecumNietBereikt;
	}

	public void setTextRedenCoecumNietBereikt(String textRedenCoecumNietBereikt)
	{
		this.textRedenCoecumNietBereikt = textRedenCoecumNietBereikt;
	}

	public DSValue getDiepstePuntInsertie()
	{
		return diepstePuntInsertie;
	}

	public void setDiepstePuntInsertie(DSValue diepstePuntInsertie)
	{
		this.diepstePuntInsertie = diepstePuntInsertie;
	}

	public NullFlavourQuantity getAfstandVanafAnus()
	{
		return afstandVanafAnus;
	}

	public void setAfstandVanafAnus(NullFlavourQuantity afstandVanafAnus)
	{
		this.afstandVanafAnus = afstandVanafAnus;
	}

	public NullFlavourQuantity getTotaleTerugtrektijd()
	{
		return totaleTerugtrektijd;
	}

	public void setTotaleTerugtrektijd(NullFlavourQuantity totaleTerugtrektijd)
	{
		this.totaleTerugtrektijd = totaleTerugtrektijd;
	}

	public Boolean getRetroflexieRectum()
	{
		return retroflexieRectum;
	}

	public void setRetroflexieRectum(Boolean retroflexieRectum)
	{
		this.retroflexieRectum = retroflexieRectum;
	}

	public DSValue getPatientcomfortgcs()
	{
		return patientcomfortgcs;
	}

	public void setPatientcomfortgcs(DSValue patientcomfortgcs)
	{
		this.patientcomfortgcs = patientcomfortgcs;
	}

	public List<DSValue> getRedenAfbrekingColoscopie()
	{
		return redenAfbrekingColoscopie;
	}

	public void setRedenAfbrekingColoscopie(List<DSValue> redenAfbrekingColoscopie)
	{
		this.redenAfbrekingColoscopie = redenAfbrekingColoscopie;
	}

	public String getTextRedenAfbrekingColoscopie()
	{
		return textRedenAfbrekingColoscopie;
	}

	public void setTextRedenAfbrekingColoscopie(String textRedenAfbrekingColoscopie)
	{
		this.textRedenAfbrekingColoscopie = textRedenAfbrekingColoscopie;
	}

	public DSValue getTerugkoppelingClient()
	{
		return terugkoppelingClient;
	}

	public void setTerugkoppelingClient(DSValue terugkoppelingClient)
	{
		this.terugkoppelingClient = terugkoppelingClient;
	}

	public MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg()
	{
		return definitiefVervolgbeleidVoorBevolkingsonderzoekg;
	}

	public void setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidVoorBevolkingsonderzoekg)
	{
		this.definitiefVervolgbeleidVoorBevolkingsonderzoekg = definitiefVervolgbeleidVoorBevolkingsonderzoekg;
	}

	public List<DSValue> getOverigeBevinding()
	{
		return overigeBevinding;
	}

	public void setOverigeBevinding(List<DSValue> overigeBevinding)
	{
		this.overigeBevinding = overigeBevinding;
	}

	public Quantity getTotaalAantalGedetecteerdeLaesies()
	{
		return totaalAantalGedetecteerdeLaesies;
	}

	public void setTotaalAantalGedetecteerdeLaesies(Quantity totaalAantalGedetecteerdeLaesies)
	{
		this.totaalAantalGedetecteerdeLaesies = totaalAantalGedetecteerdeLaesies;
	}

	public Quantity getAantalVerwijderdeLaesiesNietIngezondenVoorPaEnG()
	{
		return aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG;
	}

	public void setAantalVerwijderdeLaesiesNietIngezondenVoorPaEnG(Quantity aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG)
	{
		this.aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG = aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG;
	}

	public DSValue getRedenPoliepenNietIngezondenVoorPathologie()
	{
		return redenPoliepenNietIngezondenVoorPathologie;
	}

	public void setRedenPoliepenNietIngezondenVoorPathologie(DSValue redenPoliepenNietIngezondenVoorPathologie)
	{
		this.redenPoliepenNietIngezondenVoorPathologie = redenPoliepenNietIngezondenVoorPathologie;
	}

	public List<MdlOpdrachtnummerPalab> getOpdrachtnummerPalab()
	{
		return opdrachtnummerPalab;
	}

	public void setOpdrachtnummerPalab(List<MdlOpdrachtnummerPalab> opdrachtnummerPalab)
	{
		this.opdrachtnummerPalab = opdrachtnummerPalab;
	}

	public List<MdlTnummerPathologieVerslag> getTnummerPathologieVerslag()
	{
		return tnummerPathologieVerslag;
	}

	public void setTnummerPathologieVerslag(List<MdlTnummerPathologieVerslag> tnummerPathologieVerslag)
	{
		this.tnummerPathologieVerslag = tnummerPathologieVerslag;
	}

	public DSValue getEindconclusie()
	{
		return eindconclusie;
	}

	public void setEindconclusie(DSValue eindconclusie)
	{
		this.eindconclusie = eindconclusie;
	}

	public DSValue getVervolgbeleidNavAfbrekingColoscopie()
	{
		return vervolgbeleidNavAfbrekingColoscopie;
	}

	public void setVervolgbeleidNavAfbrekingColoscopie(DSValue vervolgbeleidNavAfbrekingColoscopie)
	{
		this.vervolgbeleidNavAfbrekingColoscopie = vervolgbeleidNavAfbrekingColoscopie;
	}

}
