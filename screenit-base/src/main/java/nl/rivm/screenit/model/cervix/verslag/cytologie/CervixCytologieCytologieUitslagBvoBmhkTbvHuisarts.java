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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "cervix")
public class CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieVerslagContent verslagContent;

	@Column(length = 4096)
	@VraagElement(displayName = "Protocollair verslag", extraTekst = "Het protocollair verslag (PV) zoals gegenereerd door de PALGA Protocol Module t.b.v. de uitslagverstrekking aan de huisarts", code = "2.16.840.1.113883.2.4.3.36.77.2.11.267", isVerplicht = true)
	private String protocollairVerslag;

	@Column(length = 4096)
	@VraagElement(displayName = "Conclusie", extraTekst = "De tekstuele conclusie zoals gegenereerd door de PALGA Protocol Module t.b.v. de uitslagverstrekking aan de huisarts", code = "2.16.840.1.113883.2.4.3.36.77.2.11.268", isVerplicht = true)
	private String conclusie;

	public CervixCytologieVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	public void setVerslagContent(CervixCytologieVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public String getProtocollairVerslag()
	{
		return protocollairVerslag;
	}

	public void setProtocollairVerslag(String protocollairVerslag)
	{
		this.protocollairVerslag = protocollairVerslag;
	}

	public String getConclusie()
	{
		return conclusie;
	}

	public void setConclusie(String conclusie)
	{
		this.conclusie = conclusie;
	}

}
