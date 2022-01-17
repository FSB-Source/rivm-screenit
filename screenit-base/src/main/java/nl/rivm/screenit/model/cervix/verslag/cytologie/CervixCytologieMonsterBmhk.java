package nl.rivm.screenit.model.cervix.verslag.cytologie;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
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
@Table(schema = "cervix")
public class CervixCytologieMonsterBmhk
	extends AbstractHibernateObject
{

	private final static long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk;

	@Column(length = 255)
	@VraagElement(displayName = "Monster identificatie", extraTekst = "Het ID van het monster, dit is de code die gescand wordt bij het binnenboeken.", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290172", isVerplicht = true)
	private String monsterIdentificatie;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(displayName = "Datum afname materiaal", extraTekst = "Datum waarop het materiaal is afgenomen bij de cli\u00ebnt (uitstrijkje) of is afgenomen door de cli\u00ebnt (ZAS)", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290173", isVerplicht = true)
	private Date datumAfnameMateriaal;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(displayName = "Datum ontvangst materiaal", extraTekst = "Datum waarop het materiaal is ontvangen in het laboratorium", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290174", isVerplicht = true)
	private Date datumOntvangstMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_type_monster", values = {
		@DSValueSetValue(code = "308728002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "ZAS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.241")
	})
	@VraagElement(displayName = "Type materiaal", extraTekst = "Het soort monster dat is afgenomen en de reden.", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290175", isVerplicht = true)
	private DSValue typeMateriaal;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(displayName = "Datum autorisatie", extraTekst = "Datum waarop het resultaat is geautoriseerd", code = "2.16.840.1.113883.2.4.3.36.77.2.10.290176", isVerplicht = true)
	private Date datumAutorisatie;

	public CervixCytologieCytologieUitslagBvoBmhk getCytologieUitslagBvoBmhk()
	{
		return cytologieUitslagBvoBmhk;
	}

	public void setCytologieUitslagBvoBmhk(CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk)
	{
		this.cytologieUitslagBvoBmhk = cytologieUitslagBvoBmhk;
	}

	public String getMonsterIdentificatie()
	{
		return monsterIdentificatie;
	}

	public void setMonsterIdentificatie(String monsterIdentificatie)
	{
		this.monsterIdentificatie = monsterIdentificatie;
	}

	public Date getDatumAfnameMateriaal()
	{
		return datumAfnameMateriaal;
	}

	public void setDatumAfnameMateriaal(Date datumAfnameMateriaal)
	{
		this.datumAfnameMateriaal = datumAfnameMateriaal;
	}

	public Date getDatumOntvangstMateriaal()
	{
		return datumOntvangstMateriaal;
	}

	public void setDatumOntvangstMateriaal(Date datumOntvangstMateriaal)
	{
		this.datumOntvangstMateriaal = datumOntvangstMateriaal;
	}

	public DSValue getTypeMateriaal()
	{
		return typeMateriaal;
	}

	public void setTypeMateriaal(DSValue typeMateriaal)
	{
		this.typeMateriaal = typeMateriaal;
	}

	public Date getDatumAutorisatie()
	{
		return datumAutorisatie;
	}

	public void setDatumAutorisatie(Date datumAutorisatie)
	{
		this.datumAutorisatie = datumAutorisatie;
	}

}
