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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "cervix")
public class CervixCytologieVerslagContent
	extends VerslagContent<CervixCytologieVerslag>
{

	private final static long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private CervixCytologieVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Verrichting", extraTekst = "Verrichting", code = "2.16.840.1.113883.2.4.3.36.77.2.10.125000", isReference = true)
	private CervixCytologieVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Cytologie uitslag BVO BMHK", extraTekst = "Cytologie uitslag BVO BMHK", code = "2.16.840.1.113883.2.4.3.36.77.2.10.270000", isReference = true)
	private CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Cytologie uitslag BVO BMHK tbv huisarts", extraTekst = "Cytologie uitslag BVO BMHK tbv huisarts", code = "2.16.840.1.113883.2.4.3.36.77.2.10.280000", isReference = true)
	private CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts cytologieUitslagBvoBmhkTbvHuisarts;

	@Override
	public CervixCytologieVerslag getVerslag()
	{
		return verslag;
	}

	@Override
	public void setVerslag(CervixCytologieVerslag verslag)
	{
		this.verslag = verslag;
	}

	public CervixCytologieVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(CervixCytologieVerrichting verrichting)
	{
		this.verrichting = verrichting;
	}

	public CervixCytologieCytologieUitslagBvoBmhk getCytologieUitslagBvoBmhk()
	{
		return cytologieUitslagBvoBmhk;
	}

	public void setCytologieUitslagBvoBmhk(CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk)
	{
		this.cytologieUitslagBvoBmhk = cytologieUitslagBvoBmhk;
	}

	public CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts getCytologieUitslagBvoBmhkTbvHuisarts()
	{
		return cytologieUitslagBvoBmhkTbvHuisarts;
	}

	public void setCytologieUitslagBvoBmhkTbvHuisarts(CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts cytologieUitslagBvoBmhkTbvHuisarts)
	{
		this.cytologieUitslagBvoBmhkTbvHuisarts = cytologieUitslagBvoBmhkTbvHuisarts;
	}

}
