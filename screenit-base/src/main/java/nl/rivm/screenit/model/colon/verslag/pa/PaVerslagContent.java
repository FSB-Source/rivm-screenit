package nl.rivm.screenit.model.colon.verslag.pa;

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
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
public class PaVerslagContent
	extends VerslagContent<PaVerslag>
{

	private final static long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private PaVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Verrichting", extraTekst = "Verrichting", code = "2.16.840.1.113883.2.4.3.36.77.2.11.68", isReference = true)
	private PaVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Pathologie : medische observatie", extraTekst = "Pathologie : medische observatie", code = "2.16.840.1.113883.2.4.3.36.77.2.11.147", isReference = true)
	private PaPathologieMedischeObservatie pathologieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Pathologie protocol: colonbiopt (per poliep)", extraTekst = "Pathologie protocol: colonbiopt (per poliep)", code = "2.16.840.1.113883.2.4.3.36.77.2.11.152", isReference = true)
	@OrderBy("to_number(coalesce(nullif(nummerPotjeMateriaal,''),to_char(id, '9999999999')), '9999999999')")
	private List<PaPathologieProtocolColonbioptperPoliep> pathologieProtocolColonbioptperPoliep = new ArrayList<>();

	@Override
	public PaVerslag getVerslag()
	{
		return verslag;
	}

	@Override
	public void setVerslag(PaVerslag verslag)
	{
		this.verslag = verslag;
	}

	public PaVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(PaVerrichting verrichting)
	{
		this.verrichting = verrichting;
	}

	public PaPathologieMedischeObservatie getPathologieMedischeObservatie()
	{
		return pathologieMedischeObservatie;
	}

	public void setPathologieMedischeObservatie(PaPathologieMedischeObservatie pathologieMedischeObservatie)
	{
		this.pathologieMedischeObservatie = pathologieMedischeObservatie;
	}

	public List<PaPathologieProtocolColonbioptperPoliep> getPathologieProtocolColonbioptperPoliep()
	{
		return pathologieProtocolColonbioptperPoliep;
	}

	public void setPathologieProtocolColonbioptperPoliep(List<PaPathologieProtocolColonbioptperPoliep> pathologieProtocolColonbioptperPoliep)
	{
		this.pathologieProtocolColonbioptperPoliep = pathologieProtocolColonbioptperPoliep;
	}

}
