
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class MdlVerrichting
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(displayName = "Identificatie onderzoek", extraTekst = "Identificatie onderzoek", code = "2.16.840.1.113883.2.4.3.36.77.2.8.125001", isVerplicht = true)
	private String identificatieOnderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_procedureindication", values = {
		@DSValueSetValue(code = "444783004:246513007=261423007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "444783004:246513007=134433005", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(displayName = "Indicatie verrichting", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.8.125010", isVerplicht = true)
	private DSValue indicatieVerrichting;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(displayName = "Aanvang verrichting", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.8.125030", isVerplicht = true)
	private Date aanvangVerrichting;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verrichting", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Incident/complicatie", extraTekst = "", code = "2.16.840.1.113883.2.4.3.36.77.2.8.125059", isReference = true)
	private List<MdlIncidentcomplicatie> incidentcomplicatie = new ArrayList<>();

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(
		displayName = "Autorisatiedatum verslag",
		extraTekst = "Datum waarop uitslag is doorgegeven aan de (aanvragende/verwijzende) zorgverlener. In het geval van pathologie gaat het om de autorisatiedatum.",
		code = "2.16.840.1.113883.2.4.3.36.77.2.8.125085",
		isVerplicht = true)
	private Date autorisatiedatumVerslag;

	public MdlVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(MdlVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public String getIdentificatieOnderzoek()
	{
		return identificatieOnderzoek;
	}

	public void setIdentificatieOnderzoek(String identificatieOnderzoek)
	{
		this.identificatieOnderzoek = identificatieOnderzoek;
	}

	public DSValue getIndicatieVerrichting()
	{
		return indicatieVerrichting;
	}

	public void setIndicatieVerrichting(DSValue indicatieVerrichting)
	{
		this.indicatieVerrichting = indicatieVerrichting;
	}

	public Date getAanvangVerrichting()
	{
		return aanvangVerrichting;
	}

	public void setAanvangVerrichting(Date aanvangVerrichting)
	{
		this.aanvangVerrichting = aanvangVerrichting;
	}

	public List<MdlIncidentcomplicatie> getIncidentcomplicatie()
	{
		return incidentcomplicatie;
	}

	public void setIncidentcomplicatie(List<MdlIncidentcomplicatie> incidentcomplicatie)
	{
		this.incidentcomplicatie = incidentcomplicatie;
	}

	public Date getAutorisatiedatumVerslag()
	{
		return autorisatiedatumVerslag;
	}

	public void setAutorisatiedatumVerslag(Date autorisatiedatumVerslag)
	{
		this.autorisatiedatumVerslag = autorisatiedatumVerslag;
	}

}
