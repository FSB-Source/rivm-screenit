package nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class OvereenkomstZoekFilter implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private String organisatieNaam;

	private String organisatiePlaats;

	private String organisatiePostcode;

	private IModel<ScreeningOrganisatie> regio;

	private String organisatieUra;

	private OrganisatieType organisatieType;

	private IModel<Overeenkomst> overeenkomst;

	private Date lopendeDatum;

	public String getOrganisatieNaam()
	{
		return organisatieNaam;
	}

	public void setOrganisatieNaam(String organisatieNaam)
	{
		this.organisatieNaam = organisatieNaam;
	}

	public String getOrganisatiePlaats()
	{
		return organisatiePlaats;
	}

	public void setOrganisatiePlaats(String organisatiePlaats)
	{
		this.organisatiePlaats = organisatiePlaats;
	}

	public String getOrganisatiePostcode()
	{
		return organisatiePostcode;
	}

	public void setOrganisatiePostcode(String organisatiePostcode)
	{
		this.organisatiePostcode = organisatiePostcode;
	}

	public ScreeningOrganisatie getRegio()
	{
		return ModelUtil.nullSafeGet(regio);
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = ModelUtil.nullSafeSet(regio);
	}

	public String getOrganisatieUra()
	{
		return organisatieUra;
	}

	public void setOrganisatieUra(String organisatieUra)
	{
		this.organisatieUra = organisatieUra;
	}

	public OrganisatieType getOrganisatieType()
	{
		return organisatieType;
	}

	public void setOrganisatieType(OrganisatieType organisatieType)
	{
		this.organisatieType = organisatieType;
	}

	public Overeenkomst getOvereenkomst()
	{
		return ModelUtil.nullSafeGet(overeenkomst);
	}

	public void setOvereenkomst(Overeenkomst overeenkomst)
	{
		this.overeenkomst = ModelUtil.nullSafeSet(overeenkomst);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(regio);
		ModelUtil.nullSafeDetach(overeenkomst);
	}

	public Date getLopendeDatum()
	{
		return lopendeDatum;
	}

	public void setLopendeDatum(Date lopendeDatum)
	{
		this.lopendeDatum = lopendeDatum;
	}
}
