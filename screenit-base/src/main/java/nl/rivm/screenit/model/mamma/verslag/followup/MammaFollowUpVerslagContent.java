package nl.rivm.screenit.model.mamma.verslag.followup;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Table;

import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "mamma")
public class MammaFollowUpVerslagContent
	extends VerslagContent<MammaFollowUpVerslag>
{

	private final static long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private MammaFollowUpVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Verrichting", extraTekst = "Verrichting", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.125000", isReference = true)
	private MammaFollowUpVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Pathologie : medische observatie", extraTekst = "Pathologie : medische observatie", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.150000", isReference = true)
	private MammaFollowUpPathologieMedischeObservatie pathologieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(displayName = "Follow-up PA", extraTekst = "Follow-up PA", code = "2.16.840.1.113883.2.4.3.36.77.0.2.2.300000", isReference = true)
	private List<MammaFollowUpFollowupPa> followupPa = new ArrayList<>();

	@Override
	public MammaFollowUpVerslag getVerslag()
	{
		return verslag;
	}

	@Override
	public void setVerslag(MammaFollowUpVerslag verslag)
	{
		this.verslag = verslag;
	}

	public MammaFollowUpVerrichting getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(MammaFollowUpVerrichting verrichting)
	{
		this.verrichting = verrichting;
	}

	public MammaFollowUpPathologieMedischeObservatie getPathologieMedischeObservatie()
	{
		return pathologieMedischeObservatie;
	}

	public void setPathologieMedischeObservatie(MammaFollowUpPathologieMedischeObservatie pathologieMedischeObservatie)
	{
		this.pathologieMedischeObservatie = pathologieMedischeObservatie;
	}

	public List<MammaFollowUpFollowupPa> getFollowupPa()
	{
		return followupPa;
	}

	public void setFollowupPa(List<MammaFollowUpFollowupPa> followupPa)
	{
		this.followupPa = followupPa;
	}

}
