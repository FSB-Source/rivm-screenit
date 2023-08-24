package nl.rivm.screenit.model.cervix.verslag.cytologie;

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
@Table(schema = "cervix")
public class CervixCytologieCytologieUitslagBvoBmhk
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "cytologieUitslagBvoBmhk", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Monster BMHK", extraTekst = "Monster BMHK", code = "2.16.840.1.113883.2.4.3.36.77.2.11.247", isReference = true)
	private CervixCytologieMonsterBmhk monsterBmhk;

	@Column(length = 255)
	@VraagElement(displayName = "C-nummer laboratorium", extraTekst = "Het rapportnummer dat is uitgereikt door het pathologie laboratorium", code = "2.16.840.1.113883.2.4.3.36.77.2.11.253", isVerplicht = true)
	private String cnummerLaboratorium;

	@Column(length = 255)
	@VraagElement(displayName = "Versie protocol", extraTekst = "Versienummer van het gebruikte protocol m.b.v. de Palga protocolmodule", code = "2.16.840.1.113883.2.4.3.36.77.2.11.254", isVerplicht = true)
	private String versieProtocol;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Kompositie", values = {
		@DSValueSetValue(code = "K0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231")
	})
	@VraagElement(displayName = "KOPAC-B: Kompositie", extraTekst = "KOPAC-B: Kompositie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.255", isVerplicht = true)
	private DSValue kopacbKompositie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Ontstekingsverschijnselen", values = {
		@DSValueSetValue(code = "O0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232")
	})
	@VraagElement(displayName = "KOPAC-B: Ontstekingsverschijnselen", extraTekst = "KOPAC-B: Ontstekingsverschijnselen", code = "2.16.840.1.113883.2.4.3.36.77.2.11.256", isVerplicht = true)
	private DSValue kopacbOntstekingsverschijnselen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Plaveiselepitheel", values = {
		@DSValueSetValue(code = "P0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233")
	})
	@VraagElement(displayName = "KOPAC-B: Plaveiselepitheel", extraTekst = "KOPAC-B: Plaveiselepitheel", code = "2.16.840.1.113883.2.4.3.36.77.2.11.257", isVerplicht = true)
	private DSValue kopacbPlaveiselepitheel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Andere_afwijkingen", values = {
		@DSValueSetValue(code = "A0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234")
	})
	@VraagElement(displayName = "KOPAC-B: Andere afwijkingen / endometrium", extraTekst = "KOPAC-B: Andere afwijkingen / endometrium", code = "2.16.840.1.113883.2.4.3.36.77.2.11.258", isVerplicht = true)
	private DSValue kopacbAndereAfwijkingenEndometrium;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Cilindercelepitheel", values = {
		@DSValueSetValue(code = "C0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235")
	})
	@VraagElement(displayName = "KOPAC-B: Cilindercelepitheel", extraTekst = "KOPAC-B: Cilindercelepitheel", code = "2.16.840.1.113883.2.4.3.36.77.2.11.259", isVerplicht = true)
	private DSValue kopacbCilindercelepitheel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Beoordeelbaarheid", values = {
		@DSValueSetValue(code = "B1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.236"),
		@DSValueSetValue(code = "B3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.236")
	})
	@VraagElement(displayName = "KOPAC-B: Beoordeelbaarheid", extraTekst = "KOPAC-B: Beoordeelbaarheid", code = "2.16.840.1.113883.2.4.3.36.77.2.11.260", isVerplicht = true)
	private DSValue kopacbBeoordeelbaarheid;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "cervix", name = "cervix_cytologie_cytologie_uitslag_bvo_bmhk_kopacb_extra")
	@DSValueSet(name = "vs_KOPAC_B_Extra", values = {
		@DSValueSetValue(code = "E03", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E05", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E14", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E15", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E18", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E23", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238")
	})
	@VraagElement(displayName = "KOPAC-B: Extra", extraTekst = "KOPAC-B: Extra", code = "2.16.840.1.113883.2.4.3.36.77.2.11.261")
	private List<DSValue> kopacbExtra = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_reden_niet_beoordeelbaar", values = {
		@DSValueSetValue(code = "B3a", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3b", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3c", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3d", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3e", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3f", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3g", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3h", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237")
	})
	@VraagElement(displayName = "Reden onbeoordeelbaar B3", extraTekst = "Reden onbeoordeelbaar B3", code = "2.16.840.1.113883.2.4.3.36.77.2.11.262", isVerplicht = true)
	private DSValue redenOnbeoordeelbaarB3;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_PAPKlasse", values = {
		@DSValueSetValue(code = "Pap0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3a1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3a2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3b", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239")
	})
	@VraagElement(displayName = "PAP klasse", extraTekst = "PAP Klasse nav Cytologie op uitstrijkje", code = "2.16.840.1.113883.2.4.3.36.77.2.11.263", isVerplicht = true)
	private DSValue papKlasse;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "cervix", name = "cervix_cytologie_cytologie_uitslag_bvo_bmhk_bethesda_score")
	@DSValueSet(name = "vs_Bethesda", values = {
		@DSValueSetValue(code = "373887005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "168402006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "39035006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103639009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "112662005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "22725004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103648004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373883009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373878001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "51642000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "88400008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Bethesda score", extraTekst = "Bethesda (2001) score", code = "2.16.840.1.113883.2.4.3.36.77.2.11.264", isVerplicht = true)
	private List<DSValue> bethesdaScore = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_adviesBMHK", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101")
	})
	@VraagElement(displayName = "Screeningsadvies / herhaling", extraTekst = "Screeningsadvies n.a.v. uitkomst cytologie onderzoek", code = "2.16.840.1.113883.2.4.3.36.77.2.11.265", isVerplicht = true)
	private DSValue screeningsadviesHerhaling;

	@Column
	@VraagElement(displayName = "COS", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.275")
	private Boolean cos;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_COSPlatform", values = {
		@DSValueSetValue(code = "TIS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.11.267"),
		@DSValueSetValue(code = "FPGIS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.11.267")
	})
	@VraagElement(displayName = "COS platform", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.11.276")
	private DSValue cosPlatform;

	public CervixCytologieVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(CervixCytologieVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public CervixCytologieMonsterBmhk getMonsterBmhk()
	{
		return monsterBmhk;
	}

	public void setMonsterBmhk(CervixCytologieMonsterBmhk monsterBmhk)
	{
		this.monsterBmhk = monsterBmhk;
	}

	public String getCnummerLaboratorium()
	{
		return cnummerLaboratorium;
	}

	public void setCnummerLaboratorium(String cnummerLaboratorium)
	{
		this.cnummerLaboratorium = cnummerLaboratorium;
	}

	public String getVersieProtocol()
	{
		return versieProtocol;
	}

	public void setVersieProtocol(String versieProtocol)
	{
		this.versieProtocol = versieProtocol;
	}

	public DSValue getKopacbKompositie()
	{
		return kopacbKompositie;
	}

	public void setKopacbKompositie(DSValue kopacbKompositie)
	{
		this.kopacbKompositie = kopacbKompositie;
	}

	public DSValue getKopacbOntstekingsverschijnselen()
	{
		return kopacbOntstekingsverschijnselen;
	}

	public void setKopacbOntstekingsverschijnselen(DSValue kopacbOntstekingsverschijnselen)
	{
		this.kopacbOntstekingsverschijnselen = kopacbOntstekingsverschijnselen;
	}

	public DSValue getKopacbPlaveiselepitheel()
	{
		return kopacbPlaveiselepitheel;
	}

	public void setKopacbPlaveiselepitheel(DSValue kopacbPlaveiselepitheel)
	{
		this.kopacbPlaveiselepitheel = kopacbPlaveiselepitheel;
	}

	public DSValue getKopacbAndereAfwijkingenEndometrium()
	{
		return kopacbAndereAfwijkingenEndometrium;
	}

	public void setKopacbAndereAfwijkingenEndometrium(DSValue kopacbAndereAfwijkingenEndometrium)
	{
		this.kopacbAndereAfwijkingenEndometrium = kopacbAndereAfwijkingenEndometrium;
	}

	public DSValue getKopacbCilindercelepitheel()
	{
		return kopacbCilindercelepitheel;
	}

	public void setKopacbCilindercelepitheel(DSValue kopacbCilindercelepitheel)
	{
		this.kopacbCilindercelepitheel = kopacbCilindercelepitheel;
	}

	public DSValue getKopacbBeoordeelbaarheid()
	{
		return kopacbBeoordeelbaarheid;
	}

	public void setKopacbBeoordeelbaarheid(DSValue kopacbBeoordeelbaarheid)
	{
		this.kopacbBeoordeelbaarheid = kopacbBeoordeelbaarheid;
	}

	public List<DSValue> getKopacbExtra()
	{
		return kopacbExtra;
	}

	public void setKopacbExtra(List<DSValue> kopacbExtra)
	{
		this.kopacbExtra = kopacbExtra;
	}

	public DSValue getRedenOnbeoordeelbaarB3()
	{
		return redenOnbeoordeelbaarB3;
	}

	public void setRedenOnbeoordeelbaarB3(DSValue redenOnbeoordeelbaarB3)
	{
		this.redenOnbeoordeelbaarB3 = redenOnbeoordeelbaarB3;
	}

	public DSValue getPapKlasse()
	{
		return papKlasse;
	}

	public void setPapKlasse(DSValue papKlasse)
	{
		this.papKlasse = papKlasse;
	}

	public List<DSValue> getBethesdaScore()
	{
		return bethesdaScore;
	}

	public void setBethesdaScore(List<DSValue> bethesdaScore)
	{
		this.bethesdaScore = bethesdaScore;
	}

	public DSValue getScreeningsadviesHerhaling()
	{
		return screeningsadviesHerhaling;
	}

	public void setScreeningsadviesHerhaling(DSValue screeningsadviesHerhaling)
	{
		this.screeningsadviesHerhaling = screeningsadviesHerhaling;
	}

	public Boolean getCos()
	{
		return cos;
	}

	public void setCos(Boolean cos)
	{
		this.cos = cos;
	}

	public DSValue getCosPlatform()
	{
		return cosPlatform;
	}

	public void setCosPlatform(DSValue cosPlatform)
	{
		this.cosPlatform = cosPlatform;
	}

}
