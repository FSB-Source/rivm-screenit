package nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class OvereenkomstZoekFilter implements IDetachable
{
	@Getter
	@Setter
	private String organisatieNaam;

	@Getter
	@Setter
	private String organisatiePlaats;

	@Getter
	@Setter
	private String organisatiePostcode;

	private IModel<ScreeningOrganisatie> regio;

	@Getter
	@Setter
	private String organisatieUra;

	@Getter
	@Setter
	private OrganisatieType organisatieType;

	private IModel<Overeenkomst> overeenkomst;

	@Getter
	@Setter
	private Date lopendeDatum;

	public ScreeningOrganisatie getRegio()
	{
		return ModelUtil.nullSafeGet(regio);
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = ModelUtil.nullSafeSet(regio);
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
}
