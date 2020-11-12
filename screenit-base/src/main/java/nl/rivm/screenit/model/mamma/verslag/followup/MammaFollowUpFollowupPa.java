
package nl.rivm.screenit.model.mamma.verslag.followup;

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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
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
@Table(schema = "mamma")
public class MammaFollowUpFollowupPa
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MammaFollowUpVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "followupPa", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Monster/materiaal", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.320632", isReference = true)
	private MammaFollowUpMonstermateriaal monstermateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_punctie_conclusie_bk", values = {
		@DSValueSetValue(code = "77289001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30389008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103635003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44085002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "68453008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(
		displayName = "C-classificatie punctie",
		extraTekst = "Diagnose na beoordeling cytologische punctie",
		code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300015",
		isVerplicht = true)
	private DSValue cclassificatiePunctie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_ER_bk", values = {
		@DSValueSetValue(code = "OHT-2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "416053008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "441117001", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Oestrogeen receptor status", extraTekst = "Oestrogeen receptor status", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300020", isVerplicht = true)
	private DSValue oestrogeenReceptorStatus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_PR_bk", values = {
		@DSValueSetValue(code = "OHT-4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "416561008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "441118006", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Progesteron receptor status", extraTekst = "Progesteron receptor status", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300030", isVerplicht = true)
	private DSValue progesteronReceptorStatus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_HER2_bk", values = {
		@DSValueSetValue(code = "OHT-6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "431396003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "427685000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OHT-8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1")
	})
	@VraagElement(
		displayName = "HER2 status",
		extraTekst = "HER2 (human epidermal growth factor receptor 2) status",
		code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300040",
		isVerplicht = true)
	private DSValue her2Status;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_typetumor_bk", values = {
		@DSValueSetValue(code = "77289001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30389008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "110396000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "50673007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44085002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "399919001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "68453008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "49611000146109", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "86049000", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "B-classificatie op mammabiopt", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300050", isVerplicht = true)
	private DSValue bclassificatieOpMammabiopt;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BloomRichardson_bk", values = {
		@DSValueSetValue(code = "369790002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369791003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369792005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "384668003", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Maligniteitsgraad", extraTekst = "Maligniteitsgraad", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300060")
	private DSValue maligniteitsgraad;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "followupPa", cascade = CascadeType.ALL)
	@VraagElement(displayName = "pTNM en gradering", extraTekst = "pTNM en gradering", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300070", isReference = true)
	private MammaFollowUpPtnmEnGradering ptnmEnGradering;

	public MammaFollowUpVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(MammaFollowUpVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public MammaFollowUpMonstermateriaal getMonstermateriaal()
	{
		return monstermateriaal;
	}

	public void setMonstermateriaal(MammaFollowUpMonstermateriaal monstermateriaal)
	{
		this.monstermateriaal = monstermateriaal;
	}

	public DSValue getCclassificatiePunctie()
	{
		return cclassificatiePunctie;
	}

	public void setCclassificatiePunctie(DSValue cclassificatiePunctie)
	{
		this.cclassificatiePunctie = cclassificatiePunctie;
	}

	public DSValue getOestrogeenReceptorStatus()
	{
		return oestrogeenReceptorStatus;
	}

	public void setOestrogeenReceptorStatus(DSValue oestrogeenReceptorStatus)
	{
		this.oestrogeenReceptorStatus = oestrogeenReceptorStatus;
	}

	public DSValue getProgesteronReceptorStatus()
	{
		return progesteronReceptorStatus;
	}

	public void setProgesteronReceptorStatus(DSValue progesteronReceptorStatus)
	{
		this.progesteronReceptorStatus = progesteronReceptorStatus;
	}

	public DSValue getHer2Status()
	{
		return her2Status;
	}

	public void setHer2Status(DSValue her2Status)
	{
		this.her2Status = her2Status;
	}

	public DSValue getBclassificatieOpMammabiopt()
	{
		return bclassificatieOpMammabiopt;
	}

	public void setBclassificatieOpMammabiopt(DSValue bclassificatieOpMammabiopt)
	{
		this.bclassificatieOpMammabiopt = bclassificatieOpMammabiopt;
	}

	public DSValue getMaligniteitsgraad()
	{
		return maligniteitsgraad;
	}

	public void setMaligniteitsgraad(DSValue maligniteitsgraad)
	{
		this.maligniteitsgraad = maligniteitsgraad;
	}

	public MammaFollowUpPtnmEnGradering getPtnmEnGradering()
	{
		return ptnmEnGradering;
	}

	public void setPtnmEnGradering(MammaFollowUpPtnmEnGradering ptnmEnGradering)
	{
		this.ptnmEnGradering = ptnmEnGradering;
	}

}
